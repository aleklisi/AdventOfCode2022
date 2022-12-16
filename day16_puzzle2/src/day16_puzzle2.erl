-module(day16_puzzle2).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    TunnelsWithValves = parse_data(RawBinaryInput),
    Answer = find_answer(TunnelsWithValves, #{filter_after => -1, hard_max_considered => 100}),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

find_answer(TunnelsWithValves, FilteringOptions) ->
    ValvesThatMatter = find_valves_with_flow(TunnelsWithValves),
    PathsOfActions =
        find_answer(TunnelsWithValves,
                    FilteringOptions,
                    [{[{open, "AA"}], [{open, "AA"}], ValvesThatMatter}]),
    lists:max(
        lists:map(fun({MyPath, ElephantPath}) ->
            count_released_pressure(MyPath, TunnelsWithValves) +
            count_released_pressure(ElephantPath, TunnelsWithValves)
                  end,
                  PathsOfActions)).

find_answer(_TunnelsWithValves, _, Paths = [{_, _, []} | _]) ->
    lists:map(fun({MyPath, ElephantPath, _}) -> {MyPath, ElephantPath} end, Paths);
find_answer(TunnelsWithValves, _, Paths = [{_, _, [UnassignedValve]} | _]) ->
    lists:map(fun({MyPath, ElephantPath, _}) ->
        {_, MyFirst} = lists:last(MyPath),
        [_ | NewMyStepByStepPath] = concept_path_to_step_by_step_path([MyFirst, UnassignedValve], TunnelsWithValves),
        MyNewPath = MyPath ++ NewMyStepByStepPath,
        {_, ElephantFirst} = lists:last(ElephantPath),
        [_ | NewElephantStepByStepPath] = concept_path_to_step_by_step_path([ElephantFirst, UnassignedValve], TunnelsWithValves),
        ElephantNewPath = ElephantPath ++ NewElephantStepByStepPath,
        
        {MyNewPath, ElephantNewPath}
    end, Paths);
find_answer(TunnelsWithValves,
            FilteringOptions = #{filter_after := FilterLeastPromisingAfter, hard_max_considered := HardMax},
            Paths) ->
    AllNewPaths =
        lists:flatten([[{begin
                             {_, MyFirst} = lists:last(MyPath),
                             [_ | NewMyStepByStepPath] =
                                 concept_path_to_step_by_step_path([MyFirst, MyValve],
                                                                   TunnelsWithValves),
                             MyPath ++ NewMyStepByStepPath
                         end,
                         begin
                             {_, ElephantFirst} = lists:last(ElephantPath),
                             [_ | NewElephantStepByStepPath] =
                                 concept_path_to_step_by_step_path([ElephantFirst, ElephantValve],
                                                                   TunnelsWithValves),
                             ElephantPath ++ NewElephantStepByStepPath
                         end,
                         UnusedValvesThatMatter -- [MyValve, ElephantValve]}
                        || MyValve <- UnusedValvesThatMatter,
                           ElephantValve <- UnusedValvesThatMatter,
                           MyValve /= ElephantValve]
                       || {MyPath, ElephantPath, UnusedValvesThatMatter} <- Paths]),
    SomeNewPaths =
        case FilterLeastPromisingAfter =< 0 andalso length(AllNewPaths) > HardMax of
            true ->
                filter_least_promising_paths(AllNewPaths, FilteringOptions, TunnelsWithValves);
            false ->
                AllNewPaths
        end,
    find_answer(TunnelsWithValves,
                FilteringOptions#{filter_after => FilterLeastPromisingAfter - 1},
                SomeNewPaths).

filter_least_promising_paths(AllNewPaths,
                             #{hard_max_considered := HardMax},
                             TunnelsWithValves) ->
    AllNewPathsMeasured =
        lists:map(fun({MyPath, ElephantPath, Sth}) ->
                     {MyPath,
                      ElephantPath,
                      Sth,
                      count_released_pressure(MyPath, TunnelsWithValves)
                      + count_released_pressure(ElephantPath, TunnelsWithValves)}
                  end,
                  AllNewPaths),
    SortedPaths =
        lists:sort(fun({_, _, _, A}, {_, _, _, B}) -> A > B end, AllNewPathsMeasured),
    {PromisingPaths, _DropOuts} = lists:split(HardMax, SortedPaths),
    lists:map(fun({MyPath, ElephantPath, UnvisitedValves, _}) ->
                 {MyPath, ElephantPath, UnvisitedValves}
              end,
              PromisingPaths).

concept_path_to_step_by_step_path(ConceptPath, TunnelsWithValves) ->
    ConceptSteps = lists:zip([start | ConceptPath], ConceptPath ++ [finish]),
    [_ | Steps] =
        lists:flatten(
            lists:map(fun(FromTo) -> concept_step_to_path(FromTo, TunnelsWithValves) end,
                      ConceptSteps)),
    Steps.

find_valves_with_flow(TunnelsWithValves) ->
    maps:keys(
        maps:filter(fun(_Corridor, {Flow, _}) -> Flow > 0 end, TunnelsWithValves)).

concept_step_to_path({start, To}, _TunnelsWithValves) ->
    [{move, To}];
concept_step_to_path({From, finish}, _TunnelsWithValves) ->
    [{open, From}];
concept_step_to_path({From, To}, TunnelsWithValves) ->
    lists:map(fun (Elem) when Elem == From ->
                      {open, Elem};
                  (Elem) ->
                      {move, Elem}
              end,
              lists:reverse(shortest_path(From, To, TunnelsWithValves))).

shortest_path(From, To, TunnelsWithValves) ->
    case find_shortest_path(To, TunnelsWithValves, [{[From]}]) of
        [{Path}] ->
            Path;
        [{Path} | _] ->
            Path
    end.

find_shortest_path(To, TunnelsWithValves, Paths) ->
    case lists:any(fun({Path}) -> hd(Path) == To end, Paths) of
        true ->
            lists:filter(fun({Path}) -> hd(Path) == To end, Paths);
        false ->
            NewPaths =
                lists:flatten(
                    lists:map(fun({Path}) ->
                                 {_, NextSteps} = maps:get(hd(Path), TunnelsWithValves),
                                 [{[NextStep | Path]} || NextStep <- NextSteps]
                              end,
                              Paths)),
            find_shortest_path(To, TunnelsWithValves, NewPaths)
    end.

count_released_pressure(PathsOfActions, TunnelsWithValves) ->
    count_released_pressure(PathsOfActions, 26, TunnelsWithValves, []).

count_released_pressure(_PathsOfActions, 1, TunnelsWithValves, OpenedValvesWithTime) ->
    lists:sum(
        lists:map(fun({Valve, Time}) ->
                     {Flow, _} = maps:get(Valve, TunnelsWithValves),
                     Time * Flow
                  end,
                  OpenedValvesWithTime));
count_released_pressure([], _TimeLeft, TunnelsWithValves, OpenedValvesWithTime) ->
    count_released_pressure([], 1, TunnelsWithValves, OpenedValvesWithTime);
count_released_pressure([CurrentAction | NextActions],
                        TimeLeft,
                        TunnelsWithValves,
                        OpenedValvesWithTime) ->
    case CurrentAction of
        {open, Valve} ->
            count_released_pressure(NextActions,
                                    TimeLeft - 1,
                                    TunnelsWithValves,
                                    [{Valve, TimeLeft} | OpenedValvesWithTime]);
        {move, _Valve} ->
            count_released_pressure(NextActions,
                                    TimeLeft - 1,
                                    TunnelsWithValves,
                                    OpenedValvesWithTime)
    end.

parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawValvesList = string:split(RawLstInput, "\n", all),
    maps:from_list(
        lists:map(fun parse_valve/1, RawValvesList)).

% Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
parse_valve(RawValve) ->
    [RawNameAndFlowRate, RawTunnels] = string:split(RawValve, ";"),
    {get_valve_name(RawNameAndFlowRate),
     {get_valve_flow_rate(RawNameAndFlowRate), get_tunnels(RawTunnels)}}.

get_valve_name(RawNameAndFlowRate) ->
    [_, ValveName | _] = string:split(RawNameAndFlowRate, " ", all),
    ValveName.

get_valve_flow_rate(RawNameAndFlowRate) ->
    [_, RawFlowRate] = string:split(RawNameAndFlowRate, "="),
    {FlowRate, ""} = string:to_integer(RawFlowRate),
    FlowRate.

get_tunnels(RawTunnels) ->
    case string:find(RawTunnels, ", ") of
        nomatch ->
            [_, Tunnel] = string:split(RawTunnels, "valve "),
            [Tunnel];
        _ ->
            [_, RawCommaSeparatedTunnels] = string:split(RawTunnels, "valves "),
            string:split(RawCommaSeparatedTunnels, ", ", all)
    end.
