-module(bender_ct_helper).

-export([start_apps/2]).

-type config() :: [{atom(), term()}].

-spec start_apps(machinery | postgres, config()) -> config().
start_apps(BackendMode, C) ->
    EpgConnectorApps = genlib_app:start_application_with(epg_connector, [
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
    ]),
    ProgressorApps = genlib_app:start_application_with(progressor, [
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
            worker_pool_size => 1000,
            process_step_timeout => 30
        }},
        {namespaces, #{
            'bender_generator' => #{
                processor => #{
                    client => machinery_prg_backend,
                    options => #{
                        namespace => 'bender_generator',
                        handler => {bender_generator, #{}},
                        schema => machinery_mg_schema_generic
                    }
                }
            },
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
    ]),
    ScoperApps = genlib_app:start_application_with(scoper, [
        {storage, scoper_storage_logger}
    ]),
    BenderApps = genlib_app:start_application_with(bender, [
        {db_ref, default_db},
        {backend_mode, BackendMode},
        {migrations_enabled, migrations_enabled(BackendMode)},
        {machinery_backend, hybrid},
        {generator, #{
            path => <<"/v1/stateproc/bender_generator">>,
            schema => machinery_mg_schema_generic,
            url => <<"http://machinegun:8022/v1/automaton">>,
            event_handler => scoper_woody_event_handler,
            transport_opts => #{
                max_connections => 1000
            }
        }},
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
    Apps = EpgConnectorApps ++ ProgressorApps ++ ScoperApps ++ BenderApps,
    [{suite_apps, Apps} | C].

migrations_enabled(machinery) ->
    false;
migrations_enabled(postgres) ->
    true.
