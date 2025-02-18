-module(generator_tests_SUITE).

-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
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
        {group, main}
    ].

-define(PARALLEL_WORKERS, 100).

-spec groups() -> [{group_name(), list(), [test_case_name()]}].
groups() ->
    [
        {main, [parallel], [
            sequence,
            sequence_minimum,
            constant,
            snowflake
        ]}
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(C) ->
    Apps =
        genlib_app:start_application_with(epg_connector, [
            {databases, #{
                default_db => #{
                    host => "postgres",
                    port => 5432,
                    database => "progressor_db",
                    username => "progressor",
                    password => "progressor"
                }
            }},
            {pools, #{
                default_pool => #{
                    database => default_db,
                    size => 30
                }
            }}
        ]) ++
            genlib_app:start_application_with(progressor, [
                {call_wait_timeout, 20},
                {defaults, #{
                    storage => #{
                        client => prg_pg_backend,
                        options => #{
                            pool => default_pool
                        }
                    },
                    retry_policy => #{
                        initial_timeout => 5,
                        backoff_coefficient => 1.0,
                        %% seconds
                        max_timeout => 180,
                        max_attempts => 3,
                        non_retryable_errors => []
                    },
                    task_scan_timeout => 1,
                    worker_pool_size => 100,
                    process_step_timeout => 30
                }},
                {namespaces, #{
                    'bender_sequence' => #{
                        processor => #{
                            client => machinery_prg_backend,
                            options => #{
                                namespace => 'bender_sequence',
                                handler => {bender_sequence, #{}},
                                schema => machinery_mg_schema_generic
                            }
                        }
                    }
                }}
            ]) ++
            genlib_app:start_application_with(scoper, [
                {storage, scoper_storage_logger}
            ]) ++
            genlib_app:start_application_with(bender, [
                {machinery_backend, hybrid},
                {sequence, #{
                    path => <<"/v1/stateproc/bender_sequence">>,
                    schema => machinery_mg_schema_generic,
                    url => <<"http://machinegun:8022/v1/automaton">>,
                    event_handler => scoper_woody_event_handler,
                    transport_opts => #{
                        max_connections => 1000
                    }
                }},
                {protocol_opts, #{
                    timeout => 60000
                }},
                {transport_opts, #{
                    max_connections => 10000,
                    num_acceptors => 100
                }}
            ]),
    [{suite_apps, Apps} | C].

-spec end_per_suite(config()) -> ok.
end_per_suite(C) ->
    genlib_app:stop_unload_applications(?CONFIG(suite_apps, C)).

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
