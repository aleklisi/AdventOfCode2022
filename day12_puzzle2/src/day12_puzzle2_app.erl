%%%-------------------------------------------------------------------
%% @doc day12_puzzle2 public API
%% @end
%%%-------------------------------------------------------------------

-module(day12_puzzle2_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    setup_db_connection(),
    day12_puzzle2_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
setup_db_connection() ->
    Eneo4jWorkerConfig = #{
    url => "http://localhost:7470",
    db => "neo4j"
    },
    persistent_term:put(eneo4j_worker_config, Eneo4jWorkerConfig),
    persistent_term:put(eneo4j_workers_count, 5),
    {ok, _} = application:ensure_all_started(eneo4j),
    #{<<"neo4j_version">> := _} = eneo4j:discovery_api().
