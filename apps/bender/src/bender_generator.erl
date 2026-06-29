-module(bender_generator).

%% API

-export([bind/4]).
-export([generate/2]).
-export([get_internal_id/2]).

-type external_id() :: binary().
-type internal_id() :: binary() | {binary(), pos_integer()}.

-type schema() :: bender:schema().
-type user_context() :: msgp_msgpack_thrift:'Value'() | undefined.
-type woody_context() :: woody_context:ctx().

-include("bender_internal.hrl").

%%% API

-spec bind(external_id(), schema(), user_context(), woody_context()) ->
    {ok, internal_id(), user_context()} | no_return().
bind(ExternalID, Schema, UserCtx, WoodyCtx) ->
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
            get_internal_id(ExternalID, WoodyCtx);
        {ok, _, _, [{_SavedState}]} ->
            %% first insert
            {ok, InternalID, undefined};
        {error, _} = Error ->
            logger:error("binding error: ~p", [Error]),
            error({binding_error, Error})
    end.

-spec get_internal_id(external_id(), woody_context()) -> {ok, internal_id(), user_context()} | no_return().
get_internal_id(ExternalID, _WoodyCtx) ->
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
