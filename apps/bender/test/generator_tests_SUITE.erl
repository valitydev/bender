-module(generator_tests_SUITE).

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

-export([constant/1]).
-export([sequence/1]).
-export([sequence_minimum/1]).
-export([snowflake/1]).

-include_lib("bender_proto/include/bender_bender_thrift.hrl").

-type config() :: [{atom(), term()}].
-type group_name() :: atom().
-type test_case_name() :: atom().

-define(CONFIG(Key, C), (element(2, lists:keyfind(Key, 1, C)))).

-spec all() -> [test_case_name() | {group, group_name()}].
all() ->
    [
        {group, machinery},
        {group, postgres}
    ].

-define(PARALLEL_WORKERS, 100).

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {machinery, [], [{group, main}]},
        {postgres, [], [{group, main}]},
        {main, [parallel], [
            sequence,
            sequence_minimum,
            constant,
            snowflake
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    C.

-spec end_per_suite(config()) -> ok.
end_per_suite(_C) ->
    ok.

-spec init_per_group(atom(), config()) -> config().
init_per_group(Group, C) when Group =:= machinery; Group =:= postgres ->
    bender_ct_helper:start_apps(Group, C);
init_per_group(_Group, C) ->
    C.

-spec end_per_group(atom(), config()) -> ok.
end_per_group(Group, C) when Group =:= machinery; Group =:= postgres ->
    genlib_app:stop_unload_applications(?CONFIG(suite_apps, C));
end_per_group(_Group, _C) ->
    ok.

-spec init_per_testcase(atom(), config()) -> config().
init_per_testcase(_Name, C) ->
    Client = generator_client:new(),
    [{client, Client} | C].

-spec end_per_testcase(atom(), config()) -> ok.
end_per_testcase(_Name, _C) ->
    ok.

-spec constant(config()) -> ok.
constant(C) ->
    Client = get_client(C),
    InternalID = bender_utils:unique_id(),
    Schema = {constant, #bender_ConstantSchema{internal_id = InternalID}},
    InternalID = generate(Schema, Client),
    ok.

-spec sequence(config()) -> ok.
sequence(C) ->
    Client = get_client(C),
    SequenceID = bender_utils:unique_id(),
    ExpectedIDs = lists:seq(1, ?PARALLEL_WORKERS),
    GeneratedIDs = genlib_pmap:map(
        fun(_) ->
            Schema = {sequence, #bender_SequenceSchema{sequence_id = SequenceID}},
            {_, IntegerID} = generate(Schema, Client),
            IntegerID
        end,
        ExpectedIDs
    ),
    ExpectedIDs = lists:sort(GeneratedIDs),
    ok.

-spec sequence_minimum(config()) -> ok.
sequence_minimum(C) ->
    Client = get_client(C),
    SequenceID = bender_utils:unique_id(),
    Schema1 = {sequence, #bender_SequenceSchema{sequence_id = SequenceID}},
    {<<"1">>, 1} = generate(Schema1, Client),
    Schema2 = {sequence, #bender_SequenceSchema{sequence_id = SequenceID, minimum = 4}},
    {<<"4">>, 4} = generate(Schema2, Client),
    OtherSeqID = bender_utils:unique_id(),
    Schema3 = {sequence, #bender_SequenceSchema{sequence_id = OtherSeqID, minimum = 8}},
    {<<"8">>, 8} = generate(Schema3, Client),
    ok.

-spec snowflake(config()) -> ok.
snowflake(C) ->
    Client = get_client(C),
    Schema = {snowflake, #bender_SnowflakeSchema{}},
    {_ID, _IntegerID} = generate(Schema, Client),
    ok.

%%%

get_client(C) ->
    ?CONFIG(client, C).

generate(Schema, Client) ->
    case generator_client:generate_id(Schema, Client) of
        #bender_GeneratedID{
            id = ID,
            integer_id = undefined
        } ->
            ID;
        #bender_GeneratedID{
            id = ID,
            integer_id = IntegerID
        } ->
            {ID, IntegerID}
    end.
