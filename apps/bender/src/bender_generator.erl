-module(bender_generator).

%% API

-export([bind/4]).
-export([generate/2]).
-export([get_internal_id/2]).

%% Machinery callbacks

-behaviour(machinery).

-export([init/4]).
-export([process_call/4]).
-export([process_timeout/3]).
-export([process_repair/4]).
-export([process_notification/4]).

-type external_id() :: binary().
-type internal_id() :: binary() | {binary(), pos_integer()}.

-type schema() :: bender:schema().
-type user_context() :: msgp_msgpack_thrift:'Value'() | undefined.
-type state() :: #{
    internal_id := internal_id(),
    user_context := user_context()
}.

-type woody_context() :: woody_context:ctx().

-type args(T) :: machinery:args(T).
-type machine() :: machinery:machine(_, state()).
-type handler_args() :: machinery:handler_args(_).
-type handler_opts() :: machinery:handler_opts(_).
-type result(A) :: machinery:result(none(), A).

-include("bender_internal.hrl").

-define(NS, bender_generator).

%%% API

-spec bind(external_id(), schema(), user_context(), woody_context()) ->
    {ok, internal_id(), user_context()} | no_return().
bind(ExternalID, Schema, UserCtx, WoodyCtx) ->
    case backend_mode() of
        machinery ->
            bind_via_machinery(ExternalID, Schema, UserCtx, WoodyCtx);
        postgres ->
            bind_via_postgres(ExternalID, Schema, UserCtx, WoodyCtx)
    end.

bind_via_machinery(ExternalID, Schema, UserCtx, WoodyCtx) ->
    InternalID = generate(Schema, WoodyCtx),
    case start(ExternalID, InternalID, UserCtx, WoodyCtx) of
        ok ->
            {ok, InternalID, undefined};
        {error, exists} ->
            get_internal_id_with_retry(ExternalID, WoodyCtx)
    end.

bind_via_postgres(ExternalID, Schema, UserCtx, WoodyCtx) ->
    InternalID = generate(Schema, WoodyCtx),
    SQL = "INSERT INTO bender_generator_states (id, state) values ($1, $2) ON CONFLICT (id) DO NOTHING RETURNING state",
    State = term_to_binary(
        #{
            internal_id => InternalID,
            user_context => UserCtx
        }
    ),
    Pool = application:get_env(bender, generator_pool, default_pool),
    Result = epg_pool:query(Pool, SQL, [ExternalID, State]),
    case Result of
        {ok, _, _, []} ->
            %% already inserted
            get_internal_id_via_postgres(ExternalID, WoodyCtx);
        {ok, _, _, [{_SavedState}]} ->
            %% first insert
            {ok, InternalID, undefined};
        {error, _} = Error ->
            logger:error("binding error: ~p", [Error]),
            error({binding_error, Error})
    end.

-spec get_internal_id(external_id(), woody_context()) -> {ok, internal_id(), user_context()} | no_return().
get_internal_id(ExternalID, WoodyCtx) ->
    case backend_mode() of
        machinery ->
            get_internal_id_via_machinery(ExternalID, WoodyCtx);
        postgres ->
            get_internal_id_via_postgres(ExternalID, WoodyCtx)
    end.

get_internal_id_via_machinery(ExternalID, WoodyCtx) ->
    case machinery:get(?NS, ExternalID, get_backend(WoodyCtx)) of
        {ok, Machine} ->
            #{
                internal_id := InternalID,
                user_context := UserCtx
            } = get_machine_state(Machine),
            {ok, InternalID, UserCtx};
        {error, notfound} ->
            throw({not_found, ExternalID})
    end.

get_internal_id_via_postgres(ExternalID, _WoodyCtx) ->
    Pool = application:get_env(bender, generator_pool, default_pool),
    SQL = "SELECT state FROM bender_generator_states WHERE id = $1",
    Result = epg_pool:query(Pool, SQL, [ExternalID]),
    case Result of
        {ok, _, []} ->
            throw({not_found, ExternalID});
        {ok, _, [{State}]} ->
            #{
                internal_id := InternalID,
                user_context := UserCtx
            } = binary_to_term(State),
            {ok, InternalID, UserCtx};
        {error, _} = Error ->
            logger:error("read internal id error: ~p", [Error]),
            error({internal_error, Error})
    end.

-spec get_internal_id_with_retry(external_id(), woody_context()) -> {ok, internal_id(), user_context()} | no_return().
get_internal_id_with_retry(ExternalID, WoodyCtx) ->
    with_retry(get_retry_strategy(), fun() ->
        try
            {done, get_internal_id(ExternalID, WoodyCtx)}
        catch
            %% NOTE Underlying machinery backend can experience race
            %% condition on machine writes. Thus it MAY occur that
            %% 'aux_state' is undefined on machine read.
            error:({badmatch, undefined}):_Stacktrace ->
                retry
        end
    end).

%%% Machinery callbacks

-spec init(args({internal_id(), user_context()}), machine(), handler_args(), handler_opts()) -> result(state()).
init({InternalID, UserCtx}, _Machine, _HandlerArgs, _HandlerOpts) ->
    #{
        aux_state => #{
            internal_id => InternalID,
            user_context => UserCtx
        }
    }.

-spec process_call(args(_), machine(), handler_args(), handler_opts()) -> no_return().
process_call(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(call).

-spec process_timeout(machine(), handler_args(), handler_opts()) -> no_return().
process_timeout(_Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(timeout).

-spec process_repair(args(_), machine(), handler_args(), handler_opts()) -> no_return().
process_repair(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(repair).

-spec process_notification(args(_), machine(), handler_args(), handler_opts()) -> no_return().
process_notification(_Args, _Machine, _HandlerArgs, _HandlerOpts) ->
    not_implemented(notification).

%%% Internal functions

-spec get_retry_strategy() -> genlib_retry:strategy().
get_retry_strategy() ->
    Opts = genlib_app:env(bender, generator),
    DefaultPolicy = genlib_retry:exponential(5, 2, {jitter, 200, 100}),
    genlib_retry:new_strategy(maps:get(retry_policy, Opts, DefaultPolicy)).

-spec with_retry(genlib_retry:strategy(), fun(() -> {done, T} | retry)) -> T | no_return().
with_retry(Strategy, Fun) ->
    case Fun() of
        {done, Result} ->
            Result;
        retry ->
            case genlib_retry:next_step(Strategy) of
                {wait, Timeout, NextStrategy} ->
                    _ = timer:sleep(Timeout),
                    with_retry(NextStrategy, Fun);
                finish ->
                    erlang:error(retries_exhausted)
            end
    end.

-spec start(external_id(), internal_id(), user_context(), woody_context()) -> ok | {error, exists}.
start(ExternalID, InternalID, UserCtx, WoodyCtx) ->
    machinery:start(?NS, ExternalID, {InternalID, UserCtx}, get_backend(WoodyCtx)).

-spec get_machine_state(machine()) -> state().
get_machine_state(#{aux_state := State}) ->
    State.

-spec get_backend(woody_context()) -> machinery_mg_backend:backend().
get_backend(WoodyCtx) ->
    bender_utils:get_backend(generator, WoodyCtx).

-spec not_implemented(any()) -> no_return().
not_implemented(What) ->
    erlang:error({not_implemented, What}).

-spec generate(schema(), woody_context()) -> internal_id().
generate(snowflake, _WoodyCtx) ->
    <<IntegerID:64>> = snowflake:new(),
    ID = genlib_format:format_int_base(IntegerID, 62),
    {ID, IntegerID};
generate(#constant{internal_id = InternalID}, _WoodyCtx) ->
    InternalID;
generate(#sequence{id = SequenceID, minimum = Minimum}, WoodyCtx) ->
    {ok, IntegerID} = bender_sequence:get_next(SequenceID, Minimum, WoodyCtx),
    {integer_to_binary(IntegerID), IntegerID}.

backend_mode() ->
    application:get_env(bender, backend_mode, machinery).
