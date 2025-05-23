-module(bender_utils).

-export([unique_id/0]).
-export([get_backend/2]).
-export([get_woody_event_handlers/0]).

-type woody_context() :: woody_context:ctx().

-type schema() :: machinery_mg_schema_generic | atom().
-type event_handler() :: woody:ev_handler() | [woody:ev_handler()].

-type automaton() :: #{
    % machinegun's automaton url
    url := binary(),
    event_handler := event_handler(),
    % state processor path
    path => binary(),
    schema => schema(),
    transport_opts => woody_client_thrift_http_transport:transport_options()
}.

%%% API

-spec unique_id() -> binary().
unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).

-spec get_backend(generator | sequence, woody_context()) -> machinery_mg_backend:backend().
get_backend(Service, WoodyCtx) ->
    get_backend(genlib_app:env(bender, machinery_backend), Service, WoodyCtx).

%%% Internal functions

get_backend(hybrid, Service, WoodyCtx) ->
    {machinery_hybrid_backend, #{
        primary_backend => get_backend(progressor, Service, WoodyCtx),
        fallback_backend => get_backend(machinegun, Service, WoodyCtx)
    }};
get_backend(progressor, Service, WoodyCtx) ->
    Automaton = genlib_app:env(bender, Service, #{}),
    {Namespace, Handler} = get_machinery_namespace_handler(Service),
    machinery_prg_backend:new(WoodyCtx, #{
        namespace => Namespace,
        handler => Handler,
        schema => maps:get(schema, Automaton, machinery_mg_schema_generic)
    });
get_backend(machinegun, Service, WoodyCtx) ->
    Automaton = genlib_app:env(bender, Service, #{}),
    machinery_mg_backend:new(WoodyCtx, #{
        client => get_woody_client(Automaton),
        schema => maps:get(schema, Automaton, machinery_mg_schema_generic)
    }).

-spec get_machinery_namespace_handler(generator | sequence) -> {machinery:namespace(), machinery:logic_handler(_)}.
get_machinery_namespace_handler(generator) ->
    {'bender_generator', {bender_generator, #{}}};
get_machinery_namespace_handler(sequence) ->
    {'bender_sequence', {bender_sequence, #{}}}.

-spec get_woody_client(automaton()) -> machinery_mg_client:woody_client().
get_woody_client(#{url := Url} = Automaton) ->
    genlib_map:compact(#{
        url => Url,
        event_handler => get_woody_event_handlers(),
        transport_opts => maps:get(transport_opts, Automaton, undefined)
    }).

-spec get_woody_event_handlers() -> woody:ev_handlers().
get_woody_event_handlers() ->
    genlib_app:env(bender, woody_event_handlers, [
        scoper_woody_event_handler
    ]).
