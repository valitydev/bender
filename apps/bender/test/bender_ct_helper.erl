-module(bender_ct_helper).

-export([start_apps/1]).

-type config() :: [{atom(), term()}].

-spec start_apps(config()) -> config().
start_apps(C) ->
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
    ScoperApps = genlib_app:start_application_with(scoper, [
        {storage, scoper_storage_logger}
    ]),
    BenderApps = genlib_app:start_application_with(bender, [
        {db_ref, default_db},
        {protocol_opts, #{
            timeout => 60000
        }},
        {transport_opts, #{
            max_connections => 10000,
            num_acceptors => 100
        }}
    ]),
    Apps = EpgConnectorApps ++ ScoperApps ++ BenderApps,
    [{suite_apps, Apps} | C].
