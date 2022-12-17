-module(day17_puzzle2).

-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([main/1]).

-define(BOARD_SIZE, 7).
-define(ABOVE, 4).
-define(ETS_HISTORY_TABLE_NAME, dropped_shapes_history_table).

%% escript Entry point
main([Filename, RawMaxRocksCount]) ->
    {ok, RawBinaryInput} = file:read_file(Filename),
    {MaxRocksCount, ""} = string:to_integer(RawMaxRocksCount),
    Moves = parse_data(RawBinaryInput),
    InitialBoard = sets:from_list(shape(floor, ok)),
    
    % Start history
    ets:new(?ETS_HISTORY_TABLE_NAME, [named_table, bag]),

    {Time, FinalBoard} =
        timer:tc(fun() -> run_simulation(InitialBoard, Moves, MaxRocksCount) end),
    Answer = find_height(FinalBoard),

    io:format("SimulationTime: ~p ms~n", [Time / 1000]),
    io:format("Answer: ~p~n", [Answer]),
    
    % Clean up history
    ets:delete(?ETS_HISTORY_TABLE_NAME),
    Answer.

%%====================================================================
%% Internal functions
%%====================================================================

run_simulation(InitialBoard, Moves, MaxRocksCount) ->
    run_simulation(InitialBoard, Moves, 1, 1, 0, MaxRocksCount, true).

run_simulation(Board, _Moves, _ShapeNum, _MoveNum, RocksCount, MaxRocksCount, _) when RocksCount >= MaxRocksCount -> Board;
run_simulation(Board, Moves, ShapeNum, MoveNum, RocksCount, MaxRocksCount, SearchPattern) when ShapeNum == 6 ->
    run_simulation(Board, Moves, 1, MoveNum, RocksCount, MaxRocksCount, SearchPattern);
run_simulation(Board, Moves, ShapeNum, MoveNum, RocksCount, MaxRocksCount, SearchPattern) ->
    NewShapesY = find_height(Board),
    NewShape = shape(ShapeNum, NewShapesY),
    {NewMoveNum, [{X, _Y} | _] = DroppedShape} = drop_shape(NewShape, Board, Moves, MoveNum),
    % Adding new squares to the board
    NewBoard = sets:union(Board, sets:from_list(DroppedShape)),
    % SearchPattern is a boolean variable to determine if I already jumped or not,
    % if I did not then I save my shapes in ETS table to be matched against
    % else I assume I jumped already and now I just need to run the simulation like in the puzzle 1 
    case SearchPattern of
        true ->
            CurrentHeight = find_height(Board),
            NewOccurrence = {{ShapeNum, MoveNum, RocksCount, find_height(Board)}, DroppedShape},
            case ets:match_object(?ETS_HISTORY_TABLE_NAME, {{ShapeNum, MoveNum, '_', '_'}, [{X, '_'} | '_']}) of
                [] ->
                    true = ets:insert(?ETS_HISTORY_TABLE_NAME, NewOccurrence),
                    run_simulation(NewBoard, Moves, ShapeNum + 1, NewMoveNum, RocksCount + 1, MaxRocksCount, SearchPattern);
                [{{_, _, PreviousRockCount, PreviousHeight}, _}] ->
                    PatternEverNRocks = RocksCount - PreviousRockCount,
                    PatternHeight = CurrentHeight - PreviousHeight,
                    jump(Board, PatternEverNRocks, PatternHeight, Moves, ShapeNum, MoveNum, RocksCount, MaxRocksCount)
            end;
        false ->
            run_simulation(NewBoard, Moves, ShapeNum + 1, NewMoveNum, RocksCount + 1, MaxRocksCount, SearchPattern)
    end.

jump(Board, PatternEverNRocks, PatternHeight, Moves, ShapeNum, MoveNum, RocksCount, MaxRocksCount) ->
    HowMayFullPatters = (MaxRocksCount - RocksCount) div PatternEverNRocks,
    HowManyRocksLeft = (MaxRocksCount - RocksCount) rem PatternEverNRocks,
    NewBoard =
        sets:from_list(
            lists:map(fun({X, Y}) -> {X, Y + PatternHeight * HowMayFullPatters} end,
                      sets:to_list(Board))),
    run_simulation(NewBoard, Moves, ShapeNum, MoveNum, 0, HowManyRocksLeft, false).

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
    [{X, Y + 3}, {X, Y + 2}, {X, Y + 1}, {X, Y}];
% ##
% ##
shape(5, CurrentMaxY) ->
    X = 3,
    Y = CurrentMaxY + ?ABOVE,
    [{X, Y + 1}, {X + 1, Y + 1}, {X, Y}, {X + 1, Y}];
shape(floor, _) ->
    [{X, 0} || X <- lists:seq(1, 7)].


small_example_test() ->
    ?assertEqual(3068, main(["./data/puzzle_17_test_input", "2022"])).

small_task_test() ->
    ?assertEqual(3149, main(["./data/puzzle_17_input", "2022"])).

big_example_test() ->
    ?assertEqual(1514285714288, main(["./data/puzzle_17_test_input", "1000000000000"])).
