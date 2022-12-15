-module(day15_puzzle2).

-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    [FileName, RawBeaconsXandYMax] = Args,
    {ok, RawBinaryInput} = file:read_file(FileName),
    {BeaconsXandYMax, ""} = string:to_integer(RawBeaconsXandYMax),
    SensorsWithBeacons = parse_data(RawBinaryInput),
    PotentialDistressBeaconRows = [
        {{0, Y}, {BeaconsXandYMax, Y}} || Y <- lists:seq(0, BeaconsXandYMax)
    ],
    [{{X, Y}, {X, Y}}] = eliminate_impossible_positions(SensorsWithBeacons, PotentialDistressBeaconRows),
    Answer = X * 4000000 + Y,
    io:format("Answer: ~p~n", [Answer]),
    Answer.

%%====================================================================
%% Internal functions
%%====================================================================

eliminate_impossible_positions(SensorsWithBeacons, PotentialDistressBeaconRow) ->
    lists:foldl(fun(E, A) -> lists:flatten(cut_down_row(E, A)) end, PotentialDistressBeaconRow, SensorsWithBeacons).

cut_down_row({Sensor = {SensorX, SensorY}, Beacon}, PotentialDistressBeaconRows) ->
    lists:map(
        fun(Row = {From = {FromX, FromY}, To = {ToX, ToY}}) ->
            if
                SensorX < FromX ->
                    Shorten = manhattan_distance(Sensor, Beacon) - manhattan_distance(Sensor, From),
                    case Shorten >= 0 of
                        true ->
                            NewFromX = FromX + Shorten + 1,
                            case NewFromX >= ToX of
                                true -> [];
                                false -> {{NewFromX, FromY}, {ToX, ToY}}
                            end;
                        false -> Row
                    end;
                SensorX > ToX ->
                    Shorten = manhattan_distance(Sensor, Beacon) - manhattan_distance(Sensor, To),
                    case Shorten >= 0 of
                        true ->
                            NewToX = ToX - Shorten - 1,
                            case NewToX =< FromX of
                                true -> [];
                                false -> {{FromX, FromY}, {NewToX, ToY}}
                            end;
                        false -> Row
                    end;
                true ->
                    Shorten = manhattan_distance(Sensor, Beacon) - manhattan_distance({0, SensorY}, {0, FromY}) + 1,
                    case Shorten > 0 of
                        true ->
                            [
                                begin
                                    NewX1 = SensorX - Shorten,
                                    case FromX =< NewX1 of
                                        false -> [];
                                        true -> {{FromX, FromY}, {NewX1, ToY}}
                                    end
                                end,
                                begin
                                    NewX2 = SensorX + Shorten,
                                    case NewX2 =< ToX of
                                        false -> [];
                                        true -> {{NewX2, FromY}, {ToX, ToY}}
                                    end
                                end
                            ];
                        false -> Row
                    end
            end
        end
        , PotentialDistressBeaconRows).

manhattan_distance({X1, Y1}, {X2, Y2}) ->
    abs(X2 - X1) + abs(Y2 - Y1).

parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawSensorsList = string:split(RawLstInput, "\n", all),
    lists:map(fun parse_sensor/1, RawSensorsList).

parse_sensor(RawSensorAndBeacon) ->
    % "Sensor at x=2, y=18: closest beacon is at x=-2, y=15"
    [RawSensor, RawClosestBeacon] = string:split(RawSensorAndBeacon, ": closest beacon is at "),
    [RawSensorX, RawSensorY] = string:split(RawSensor -- "Sensor at x= y=", ","),
    {SensorX, ""} = string:to_integer(RawSensorX),
    {SensorY, ""} = string:to_integer(RawSensorY),
    [RawClosestBeaconX, RawClosestBeaconY] = string:split(RawClosestBeacon -- "x= y=", ","),
    {ClosestBeaconX, ""} = string:to_integer(RawClosestBeaconX),
    {ClosestBeaconY, ""} = string:to_integer(RawClosestBeaconY),
    {{SensorX, SensorY}, {ClosestBeaconX, ClosestBeaconY}}.

cut_down_row_test() ->
    [
        {{2, 0}, {20, 0}},
        {{1, 1}, {20, 1}},
        {{0, 2}, {20, 2}},
        {{0, 3}, {20, 3}}
    | _] = main(["./data/puzzle_15_test_input_A", "20"]),
    [
        {{0, 0}, {1, 0}}, {{7, 0}, {20, 0}},
        {{0, 1}, {2, 1}}, {{6, 1}, {20, 1}},
        {{0, 2}, {3, 2}}, {{5, 2}, {20, 2}},
        {{0, 3}, {20, 3}},
        {{0, 4}, {20, 4}}
    | _] = main(["./data/puzzle_15_test_input_B", "20"]),
    [
        {{0, 0}, {18, 0}},
        {{0, 1}, {19, 1}},
        {{0, 2}, {20, 2}},
        {{0, 3}, {20, 3}}
    | _] = main(["./data/puzzle_15_test_input_C", "20"]).
