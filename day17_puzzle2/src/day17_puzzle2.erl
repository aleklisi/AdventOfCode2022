-module(day17_puzzle2).

%% API exports
-export([main/1]).

-define(BOARD_SIZE, 7).
-define(MAX_ROCKS_COUNT, 1_000_000).
% -define(MAX_ROCKS_COUNT, 1_000_000_000_000).
-define(ABOVE, 4).
% -define(RESET_EVERY, infinity). % Execution time for 10_000 rocks 4739.481 ms
-define(RESET_EVERY, 20).         % Execution time for 10_000 rocks  188.019 ms

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    Moves = parse_data(RawBinaryInput),
    InitialBoard = sets:from_list(shape(floor, ok)),
    {Time, {FinalBoard, _History}} = timer:tc(fun() -> run_simulation(InitialBoard, Moves, ?MAX_ROCKS_COUNT) end),
    Answer = find_height(FinalBoard),
    io:format("SimulationTime: ~p ms~n", [Time / 1000]),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

run_simulation(InitialBoard, Moves, MaxRocksCount) ->
    run_simulation(InitialBoard, Moves, 1, 1, 0, MaxRocksCount, []).

run_simulation(Board, _Moves, _ShapeNum, _MoveNum, RocksCount, MaxRocksCount, History) when RocksCount == MaxRocksCount ->
    {Board, History};
run_simulation(_Board, Moves, ShapeNum, MoveNum, RocksCount, MaxRocksCount, History) when length(History) == ?RESET_EVERY ->
    NewBoard = sets:from_list(lists:flatten([shape(floor, ok) | History])),
    run_simulation(NewBoard, Moves, ShapeNum, MoveNum, RocksCount, MaxRocksCount, []);
run_simulation(Board, Moves, ShapeNum, MoveNum, RocksCount, MaxRocksCount, History) when ShapeNum == 6 ->
    run_simulation(Board, Moves, 1, MoveNum, RocksCount, MaxRocksCount, History);
run_simulation(Board, Moves, ShapeNum, MoveNum, RocksCount, MaxRocksCount, History) ->
    NewShapesY = find_height(Board),
    NewShape = shape(ShapeNum, NewShapesY),
    {NewMoveNum, DroppedShape} = drop_shape(NewShape, Board, Moves, MoveNum),
    NewBoard = sets:union(Board, sets:from_list(DroppedShape)),
    NewHistory = [DroppedShape | History],
    run_simulation(NewBoard, Moves, ShapeNum + 1, NewMoveNum, RocksCount + 1, MaxRocksCount, NewHistory).

drop_shape(Shape, Board, Moves, MoveNum) when MoveNum == length(Moves) + 1 ->
    drop_shape(Shape, Board, Moves, 1);
drop_shape(Shape, Board, Moves, MoveNum) ->
    MoveDirection = lists:nth(MoveNum, Moves),
    Shape1 =
        case MoveDirection of
            $< ->
                move_shape(left, Shape);
            $> ->
                move_shape(right, Shape)
        end,
    Shape2 =
        case shape_hit_side(Shape1) orelse shape_hits_the_rock(Shape1, Board) of
            true ->
                Shape;
            false ->
                Shape1
        end,
    Shape3 = move_shape(down, Shape2),
    case shape_hits_the_rock(Shape3, Board) of
        true ->
            {MoveNum + 1, Shape2};
        false ->
            drop_shape(Shape3, Board, Moves, MoveNum + 1)
    end.

shape_hit_side(Shape) ->
    lists:any(fun({X, _}) -> X =< 0 orelse X > ?BOARD_SIZE end, Shape).

shape_hits_the_rock(Shape, Board) ->
    sets:size(
        sets:intersection([sets:from_list(Shape), Board]))
    > 0.

find_height(Board) ->
    lists:max(lists:map(fun({_X, Y}) -> Y end, sets:to_list(Board))).

parse_data(RawBinaryInput) ->
    binary:bin_to_list(RawBinaryInput).

move_shape(left, Shape) ->
    lists:map(fun({X, Y}) -> {X - 1, Y} end, Shape);
move_shape(right, Shape) ->
    lists:map(fun({X, Y}) -> {X + 1, Y} end, Shape);
move_shape(down, Shape) ->
    lists:map(fun({X, Y}) -> {X, Y - 1} end, Shape).

% ####
shape(1, CurrentMaxY) ->
    X = 3,
    Y = CurrentMaxY + ?ABOVE,
    [
        {X, Y}, {X + 1, Y}, {X + 2, Y}, {X + 3, Y}
    ];
% .#.
% ###
% .#.
shape(2, CurrentMaxY) ->
    X = 3,
    Y = CurrentMaxY + ?ABOVE,
    [
                    {X + 1, Y + 2}, 
        {X, Y + 1}, {X + 1, Y + 1}, {X + 2, Y + 1}, 
                    {X + 1, Y}
    ]
    ;
% ..#
% ..#
% ###
shape(3, CurrentMaxY) ->
    X = 3,
    Y = CurrentMaxY + ?ABOVE,
    [
                            {X + 2, Y + 2},
                            {X + 2, Y + 1},
        {X, Y}, {X + 1, Y}, {X + 2, Y}
    ];
% #
% #
% #
% #
shape(4, CurrentMaxY) ->
    X = 3,
    Y = CurrentMaxY + ?ABOVE,
    [
        {X, Y + 3},
        {X, Y + 2},
        {X, Y + 1},
        {X, Y}
    ];
% ##
% ##
shape(5, CurrentMaxY) ->
    X = 3,
    Y = CurrentMaxY + ?ABOVE,
    [
        {X, Y + 1}, {X + 1, Y + 1},
        {X, Y}, {X + 1, Y}
    ];
shape(floor, _) ->
    [{X, 0} || X <- lists:seq(1, 7)].
