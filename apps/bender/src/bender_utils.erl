-module(bender_utils).

-export([unique_id/0]).
-export([get_woody_event_handlers/0]).

%%% API

-spec unique_id() -> binary().
unique_id() ->
    <<ID:64>> = snowflake:new(),
    genlib_format:format_int_base(ID, 62).

%%% Internal functions

-spec get_woody_event_handlers() -> woody:ev_handlers().
get_woody_event_handlers() ->
    genlib_app:env(bender, woody_event_handlers, [
        scoper_woody_event_handler
    ]).
