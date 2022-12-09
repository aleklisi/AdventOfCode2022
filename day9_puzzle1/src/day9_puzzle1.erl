-module(day9_puzzle1).

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
        rope => [{0, 0} || _ <- lists:seq(1, 2)],
        visited_squares => sets:new()
    },
    #{visited_squares := Visited} = lists:foldl(fun execute_move/2, Acc, HeadMoves),
    sets:size(Visited).

execute_move({Direction, Steps}, Accu) ->
    lists:foldl(
        fun(_Elem, #{rope := Rope, visited_squares := Visited}) ->
            NewRope = execute_step(Direction, Rope),
            #{
                rope => NewRope,
                visited_squares => sets:add_element(lists:last(NewRope), Visited)
            }
        end, Accu, lists:seq(1, Steps)).

execute_step(Direction, _Rope = [{HeadX, HeadY} | Rest ]) ->
    NewHeadPos = case Direction of
        "U" -> {HeadX, HeadY + 1};
        "D" -> {HeadX, HeadY - 1};
        "L" -> {HeadX - 1, HeadY};
        "R" -> {HeadX + 1, HeadY}
    end,
    {Last, OthersRev} = lists:foldl(
        fun(NextElem, {PrevElem, Moved}) ->
            NewPrevPos = maybe_move_tail(PrevElem, NextElem),
            {NewPrevPos, [PrevElem | Moved]}
    end, {NewHeadPos, []}, Rest),
    lists:reverse([Last | OthersRev]).

maybe_move_tail({HeadX, HeadY}, {TailX, TailY}) ->
    DistanceSquared = distance_squared(HeadX, HeadY, TailX, TailY),
    NewTailPos = if
        DistanceSquared =< 2 ->
            {TailX, TailY};
        true ->
            Moves = moves(TailX, TailY),
            [{_, NewX, NewY} | _] = lists:keysort(1, [{distance_squared(HeadX, HeadY, X, Y), X, Y} ||{X, Y} <- Moves]),
            {NewX, NewY}
    end,
    NewTailPos.

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
