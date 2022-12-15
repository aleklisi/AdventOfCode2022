-module(day15_puzzle1).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    [FileName, BeaconsRowYRaw] = Args,
    {ok, RawBinaryInput} = file:read_file(FileName),
    {BeaconsRowY, ""} = string:to_integer(BeaconsRowYRaw),
    SensorsWithBeacons = parse_data(RawBinaryInput),
    Positions = find_positions_where_beacon_cannot_be(SensorsWithBeacons, BeaconsRowY),
    Beacons = lists:map(fun({_, Beacon}) -> Beacon end, SensorsWithBeacons),
    % we need to -- Beacon to eliminate positions where there already is a beacon
    Answer = length(lists:uniq(lists:flatten(Positions)) -- Beacons),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

find_positions_where_beacon_cannot_be(SensorsWithBeacons, BeaconsRowY) ->
    lists:map(
        fun({Sensor, Beacon}) ->
            find_positions_for_beacon(Sensor, Beacon, BeaconsRowY)
        end, SensorsWithBeacons).

find_positions_for_beacon(Sensor = {SensorX, SensorY}, Beacon, BeaconsRowY) ->
    SensorToBeaconDistance = manhattan_distance(Sensor, Beacon),
    VerticalDistance = case BeaconsRowY > SensorY of
        true -> abs(BeaconsRowY - SensorY);
        false -> abs(SensorY - BeaconsRowY)
    end,
    HorizontalDistanceLeft = SensorToBeaconDistance - VerticalDistance,
    case HorizontalDistanceLeft < 1 of
        true -> [];
        false -> [{X, BeaconsRowY} || X <- lists:seq(SensorX - HorizontalDistanceLeft, SensorX + HorizontalDistanceLeft)]
    end.

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
