-module(day19_puzzle1).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([File, StepsRaw]) ->
    {ok, RawBinaryInput} = file:read_file(File),
    {Steps, ""} = string:to_integer(StepsRaw),
    predefined_materials(),
    Blueprints = parse_data(RawBinaryInput),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    [start_cache(Table) || Table <- tables()],
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    {Time, Answer} = timer:tc(fun() ->
        lists:sum([find_best_production_schema(Blueprint, Steps) || Blueprint <- Blueprints])
    end),
    io:format("Time: ~p [s] ~n", [Time / 1_000_000]),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

tables() -> [
        which_robots_can_be_build,
        update_materials,
        dfs
    ].
find_best_production_schema(Blueprint = {Id, Costs}, Time) ->
    io:format("Processing Id: ~p~n", [Id]),
    Materials = maps:from_list([{Material, 0} || Material <- predefined_materials()]),
    Robots = Materials#{ore => 1},

    CostsLst = lists:flatten(lists:map(fun({_, Map}) -> maps:to_list(Map) end, Costs)),
    MaxRobotsPerResource = [
        begin
            CostsOfMaterial = lists:filter(fun({Mat, _Cost}) -> Mat == Material end, CostsLst),
            {Material, lists:max(lists:map(fun({_, Cost}) -> Cost end, CostsOfMaterial))}
        end
            ||  Material <- predefined_materials() -- [geode] ],
    R = dfs(Time, Blueprint, Materials, Robots, maps:from_list([{geode, infinity} | MaxRobotsPerResource])),
    io:format("Result for ~p is ~p~n", [Id, R]),
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    [clear_cache(Table) || Table <- tables()],
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    io:format("Processed Id: ~p~n", [Id]),
    R * Id.

dfs(0, _, #{geode := Geode}, _Robots, _MaxRobotsNeededPerResource) ->
    Geode;
dfs(Time, Blueprint, Materials, Robots, MaxRobotsNeededPerResource) ->
    Key = [dfs, Time, Blueprint, Materials, Robots, MaxRobotsNeededPerResource],
    case cache_get(Key) of
        [] ->
            NewRobots = which_robots_can_be_build(Blueprint, Materials, Robots, MaxRobotsNeededPerResource),
            NewMaterials = update_materials(Materials, Robots, 1),
            Result = lists:max([
                        dfs(Time - 1, Blueprint, NewMaterials, Robots, MaxRobotsNeededPerResource) |
                        [ dfs(Time - 1, Blueprint, pay_for_robot(NewMaterials, NewRobot),
                        add_robot(NewRobot, Robots), MaxRobotsNeededPerResource) || NewRobot <- NewRobots]
                    ]),
            cache_put(Key, Result),
            Result;
        [{_, Value}] -> Value
    end.

which_robots_can_be_build(Blueprint = {_ID, RobotsWithCosts}, Materials, Robots, MaxRobotsNeededPerResource) ->
    Key = [which_robots_can_be_build, Blueprint, Materials, Robots, MaxRobotsNeededPerResource],
    case cache_get(Key) of
        [] ->
            Filtered = lists:filter(
                fun({RobotType, Costs}) ->
                    CurrentProduction = maps:get(RobotType, Robots),
                    MaxNeededProduction = maps:get(RobotType, MaxRobotsNeededPerResource),
                    (CurrentProduction =< MaxNeededProduction)
                    andalso
                    (maps:fold(
                        fun(CostType, CostValue, Acc) ->
                            Acc andalso (maps:get(CostType, Materials) >= CostValue)
                        end, true, Costs))
                end, RobotsWithCosts),
            Result = case lists:filter(fun({geode, _}) -> true; (_) -> false end, Filtered) of
               [] -> Filtered;
               [X] -> [X]
            end,
                cache_put(Key, Result),
                Result;
        [{_, Value}] -> Value
    end.

pay_for_robot(Materials, {_RobotType, Costs}) ->
    update_materials(Materials, Costs, -1).

add_robot({RobotType, _Costs}, Robots) ->
    update_materials(Robots, #{RobotType => 1}, 1).

update_materials(Materials, Robots, AddOrSub) ->
    Key = [update_materials, Materials, Robots, AddOrSub],
    case cache_get(Key) of
        [] -> 
            Result = lists:foldl(fun(MaterialType, TmpMaterials) -> 
                CurrentAmount = maps:get(MaterialType, TmpMaterials),
                Production = maps:get(MaterialType, Robots, 0),
                maps:put(MaterialType, CurrentAmount + (Production * AddOrSub), TmpMaterials)
            end, Materials, predefined_materials()),
            cache_put(Key, Result),
            Result;
        [{_, Value}] -> Value
    end.

predefined_materials() ->
    [ore, clay, obsidian, geode].

parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawBlueprints = string:split(RawLstInput, "\n", all),
    lists:map(fun parse_blueprint/1, RawBlueprints).

parse_blueprint(RawBlueprint) ->
    [RawBlueprintWithID, RawCosts] = string:split(RawBlueprint, ": "),
    [_, StrBlueprintWithID] = string:split(RawBlueprintWithID, " "),
    {BlueprintWithID, ""} = string:to_integer(StrBlueprintWithID),
    RawCostsLst = string:split(RawCosts, ". ", all),
    {
        BlueprintWithID,
        lists:map(fun parse_cost/1, RawCostsLst)
    }.

parse_cost(RawCost) ->
    case string:split(RawCost, " ", all) of
        [_Each, RobotType, _Robot, _Costs, CoinValueStr, CoinType] ->
            {CoinValue, ""} = string:to_integer(CoinValueStr),
            {convert_coin_type(RobotType), #{convert_coin_type(CoinType) => CoinValue}};
        [_Each, RobotType, _Robot, _Costs, CoinValueStr1, CoinType1, _And, CoinValueStr2, CoinType2] ->
            {CoinValue1, ""} = string:to_integer(CoinValueStr1),
            {CoinValue2, ""} = string:to_integer(CoinValueStr2),
            {list_to_existing_atom(RobotType), #{
                convert_coin_type(CoinType1) => CoinValue1,
                convert_coin_type(CoinType2) => CoinValue2}
            }
    end.

convert_coin_type("obsidian.") -> obsidian;
convert_coin_type(CoinType) ->
    list_to_existing_atom(CoinType).

start_cache(Table) ->
    ets:new(Table, [set, named_table]).

clear_cache(Table) ->
    ets:match_delete(Table, '_').

cache_put([Table | Key], Value) ->
    ets:insert(Table, {Key, Value}).

cache_get([Table | Key]) ->
    ets:lookup(Table, Key).