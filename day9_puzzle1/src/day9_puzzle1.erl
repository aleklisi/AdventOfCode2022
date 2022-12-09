-module(day9_puzzle1).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    HeadMoves = parse_data(RawBinaryInput),
    Answer = execute_moves(HeadMoves),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).


%%====================================================================
%% Internal functions
%%====================================================================

parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawMovesList = string:split(RawLstInput, "\n", all),
    lists:map(
        fun(RawMove) ->
            [Direction, RawMovesCount] = string:split(RawMove, " "),
            {MovesCount, ""} = string:to_integer(RawMovesCount),
            {Direction, MovesCount}
        end, RawMovesList).

execute_moves(HeadMoves) ->
    Acc = #{
        head_pos => {0, 0},
        tail_pos => {0, 0},
        visited_squares => sets:new()
    },
    #{visited_squares := Visited} = lists:foldl(fun execute_move/2, Acc, HeadMoves),
    sets:size(Visited).

execute_move({Direction, Steps}, Accu) ->
    lists:foldl(
        fun(_Elem, #{head_pos := HeadPos, tail_pos := TailPos, visited_squares := Visited}) ->
            {NewHeadPos, NewTailPos} = execute_step(Direction, HeadPos, TailPos),
            #{
                head_pos => NewHeadPos,
                tail_pos => NewTailPos,
                visited_squares => sets:add_element(NewTailPos, Visited)
            }
        end, Accu, lists:seq(1, Steps)).


execute_step(Direction, {HeadX, HeadY}, {TailX, TailY}) ->
    NewHeadPos = {NewHeadX, NewHeadY} = case Direction of
        "U" -> {HeadX, HeadY + 1};
        "D" -> {HeadX, HeadY - 1};
        "L" -> {HeadX - 1, HeadY};
        "R" -> {HeadX + 1, HeadY}
    end,
    NewTailPos = maybe_move_tail(NewHeadX, NewHeadY, TailX, TailY),
    {NewHeadPos, NewTailPos}.


maybe_move_tail(HeadX, HeadY, TailX, TailY) ->
    DistanceSquared = distance_squared(HeadX, HeadY, TailX, TailY),
    NewTailPos = if
        DistanceSquared =< 2 ->
            [{TailX, TailY}];
        true ->
            [{_NewTailX, _NewTailY}] = [ {X, Y} ||{X, Y} <- moves(TailX, TailY), distance_squared(HeadX, HeadY, X, Y) < 2]
    end,
    hd(NewTailPos).

moves(TailX, TailY) ->
    [
        {TailX + 1, TailY},
        {TailX - 1, TailY},
        {TailX, TailY + 1},
        {TailX, TailY - 1},
        {TailX + 1, TailY + 1},
        {TailX + 1, TailY - 1},
        {TailX - 1, TailY + 1},
        {TailX - 1, TailY - 1}
    ].

distance_squared(X1, Y1, X2, Y2) ->
    (X1 - X2) * (X1 - X2) + (Y1 - Y2) * (Y1 - Y2).
