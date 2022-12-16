-module(day16_puzzle1).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    TunnelsWithValves = parse_data(RawBinaryInput),

    io:format("TunnelsWithValves: ~p~n", [TunnelsWithValves]),
    % Answer = dfs_all_possibilities("AA", sets:new(), #{}, 30, TunnelsWithValves),
    Answer = dfs_all_possibilities("AA", [], #{}, 30, TunnelsWithValves),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

dfs_all_possibilities(CurrentCorridor, OpenedValves, ValvesWithOpeningTimes, 1, TunnelsWithValves) ->
    lists:sum(lists:map(fun(OpenedValve) ->
        OpenTime = maps:get(OpenedValve, ValvesWithOpeningTimes),
        {ValveFlowRate, _NextSteps} = maps:get(CurrentCorridor, TunnelsWithValves),
        OpenTime * ValveFlowRate
    end, OpenedValves));
dfs_all_possibilities(CurrentCorridor, OpenedValves, ValvesWithOpeningTimes, MinutesLeft, TunnelsWithValves) ->
    {ValveFlowRate, NextCorridors} = maps:get(CurrentCorridor, TunnelsWithValves),
    
    NextStepsCalculated = lists:map(fun(NewCorridor) ->
            dfs_all_possibilities(NewCorridor, OpenedValves,
                ValvesWithOpeningTimes, MinutesLeft - 1, TunnelsWithValves)
        end, NextCorridors),
    % case sets:is_element(CurrentCorridor, OpenedValves) of
    case lists:member(CurrentCorridor, OpenedValves) orelse ValveFlowRate == 0 of
        true -> lists:max(NextStepsCalculated);
        false -> 
            lists:max([
                dfs_all_possibilities(
                    % CurrentCorridor, sets:add_element(CurrentCorridor, OpenedValves),
                    CurrentCorridor, [CurrentCorridor | OpenedValves],
                    maps:put(CurrentCorridor, MinutesLeft, ValvesWithOpeningTimes),
                    MinutesLeft - 1, TunnelsWithValves)
                | NextStepsCalculated
            ])
    end.

parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawValvesList = string:split(RawLstInput, "\n", all),
    maps:from_list(lists:map(fun parse_valve/1, RawValvesList)).

% Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
parse_valve(RawValve) ->
    [RawNameAndFlowRate, RawTunnels] = string:split(RawValve, ";"),
    {
        get_valve_name(RawNameAndFlowRate),
        {
            get_valve_flow_rate(RawNameAndFlowRate),
            get_tunnels(RawTunnels)
        }
    }.

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
