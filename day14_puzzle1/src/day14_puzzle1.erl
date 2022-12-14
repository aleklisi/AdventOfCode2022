-module(day14_puzzle1).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    {MinX, _MinY, MaxX, MaxY, InitialBoard} = parse_data(RawBinaryInput),
    BoardAfterSimulation = run_simulation(MinX - 1, MaxX + 1, MaxY + 2, InitialBoard),
    Answer = length(lists:filter(fun(send) -> true; (_) -> false end, maps:values(BoardAfterSimulation))),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

run_simulation(MinX, MaxX, MaxY, Board) ->
    case drop_send_unit(500, 0, Board, MinX, MaxX, MaxY) of
        {continue, UpdatedBoard} ->
            run_simulation(MinX, MaxX, MaxY, UpdatedBoard);
        {finish, UpdatedBoard} ->
            UpdatedBoard
    end.

drop_send_unit(X, Y, Board, MinX, MaxX, MaxY) ->
    case X =< MinX orelse X >= MaxX orelse Y >= MaxY of
        true ->
            {finish, Board};
        false ->
            Down = maps:get({X, Y + 1}, Board, empty),
            case Down of
                empty ->
                    drop_send_unit(X, Y + 1, Board, MinX, MaxX, MaxY);
                _ ->
                    DownLeft = maps:get({X - 1, Y + 1}, Board, empty),
                    case DownLeft of
                        empty ->
                            drop_send_unit(X - 1, Y + 1, Board, MinX, MaxX, MaxY);
                        _ ->
                            DownRight = maps:get({X + 1, Y + 1}, Board, empty),
                            case DownRight of
                                empty ->
                                    drop_send_unit(X + 1, Y + 1, Board, MinX, MaxX, MaxY);
                                _ ->
                                    {continue, maps:put({X, Y}, send, Board)}
                            end
                    end
            end
    end.

parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawRockLinesList = string:split(RawLstInput, "\n", all),
    ParsedRockLinesList = lists:map(fun parse_rock_line/1, RawRockLinesList),
    {MinX, MinY, MaxX, MaxY} = find_board_dementions(ParsedRockLinesList),
    Board = build_board(ParsedRockLinesList),
    {MinX, MinY, MaxX, MaxY, Board}.

parse_rock_line(RawRockLine) ->
    RawRockPointsList = string:split(RawRockLine, " -> ", all),
    lists:map(fun parse_point/1, RawRockPointsList).

parse_point(RawRockPoint) ->
    [RawX, RawY] = string:split(RawRockPoint, ","),
    {X, ""} = string:to_integer(RawX),
    {Y, ""} = string:to_integer(RawY),
    {X, Y}.

find_board_dementions(ParsedRockLinesList) ->
    Xs = [X || {X, _Y} <- [{500, 0} | lists:flatten(ParsedRockLinesList)]],
    Ys = [Y || {_X, Y} <- [{500, 0} | lists:flatten(ParsedRockLinesList)]],
    {lists:min(Xs), lists:min(Ys), lists:max(Xs), lists:max(Ys)}.

build_board(ParsedRockLinesList) ->
    maps:from_list(
        lists:flatten(
            lists:map(fun build_path/1, ParsedRockLinesList))).

build_path([]) ->
    [];
build_path([_]) ->
    [];
build_path([{FromX, FromY}, {ToX, ToY} | Rest]) ->
    MinX = min(FromX, ToX),
    MinY = min(FromY, ToY),
    MaxX = max(FromX, ToX),
    MaxY = max(FromY, ToY),
    [[{{X, Y}, rock} || X <- lists:seq(MinX, MaxX), Y <- lists:seq(MinY, MaxY)]
     | build_path([{ToX, ToY} | Rest])].
