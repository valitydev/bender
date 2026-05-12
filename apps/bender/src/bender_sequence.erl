-module(bender_sequence).

%% API

-export([get_current/2]).
-export([get_next/2]).
-export([get_next/3]).

-type id() :: binary().
-type minimum() :: integer() | undefined.

-export_type([id/0]).
-export_type([minimum/0]).

-type woody_context() :: woody_context:ctx().

-type value() :: non_neg_integer().

-define(DEFAULT_INITIAL_VALUE, 1).

%%% API

-spec get_current(id(), woody_context()) -> {ok, value()} | {error, notfound}.
get_current(SequenceID, _WoodyCtx) ->
    SQL = "SELECT value FROM bender_sequence_values WHERE id = $1",
    Result = epg_pool:query(pg_pool(), SQL, [SequenceID]),
    case Result of
        {ok, _, []} ->
            {error, notfound};
        {ok, _, [{Value}]} ->
            {ok, Value};
        {error, _} = Error ->
            logger:error("read sequence error: ~p", [Error]),
            error({read_sequence_error, Error})
    end.

-spec get_next(id(), woody_context()) -> {ok, value()}.
get_next(SequenceID, WoodyCtx) ->
    get_next(SequenceID, ?DEFAULT_INITIAL_VALUE, WoodyCtx).

-spec get_next(id(), minimum(), woody_context()) -> {ok, value()}.
get_next(SequenceID, undefined, WoodyCtx) ->
    get_next(SequenceID, ?DEFAULT_INITIAL_VALUE, WoodyCtx);
get_next(SequenceID, Minimum, _WoodyCtx) ->
    SQL =
        "INSERT INTO bender_sequence_values (id, value) values ($1, $2) "
        " ON CONFLICT (id) DO UPDATE SET value = GREATEST(bender_sequence_values.value + 1, $2) RETURNING value",
    Result = epg_pool:query(pg_pool(), SQL, [SequenceID, Minimum]),
    case Result of
        {ok, _, _, [{Value}]} ->
            {ok, Value};
        {error, _} = Error ->
            logger:error("sequence increment error: ~p", [Error]),
            error({sequence_increment_error, Error})
    end.

pg_pool() ->
    application:get_env(bender, sequence_pool, default_pool).
