-module(day20_puzzle1).

-include_lib("eunit/include/eunit.hrl").
-compile(export_all).

-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

main([Filename]) ->
    {ok, RawBinaryInput} = file:read_file(Filename),
    InitialList = parse_data(RawBinaryInput),
    List = build_list(InitialList),
    FinalNumbersList = mix(List),
    Answer = find_answer(FinalNumbersList),
    io:format("Answer: ~p~n", [Answer]),
    FinalNumbersList.

%%====================================================================
%% Internal functions
%%====================================================================

find_answer(FinalNumbersList) ->
    {Before, After} = split(0, FinalNumbersList),
    All = [0 | After] ++ Before,
    lists:sum([
        wrapping_list(1000, All, All),
        wrapping_list(2000, All, All),
        wrapping_list(3000, All, All)
    ]).

wrapping_list(0, [Elem | _ ], _) -> Elem;
wrapping_list(N, [], All) -> wrapping_list(N, All, All);
wrapping_list(N, [_Elem | Rest], All) -> wrapping_list(N - 1, Rest, All).

mix(List) ->
    case lists:all(fun(#{moves := MovesCount}) -> MovesCount == 0 end, List) of
    true -> to_numbers(List);
    false ->
        Current = find_new_current_element(List),
        ListWithMovedCurrentElem = move_and_unmark_current(List, Current),
        mix(ListWithMovedCurrentElem)
    end.

to_numbers(List) ->
    lists:map(fun(#{number := Number}) -> Number end, List).

find_new_current_element(List) ->
    NotMovedYet = lists:filter(fun(#{moves := Moves}) -> Moves /= 0 end, List),
    hd(lists:sort(
        fun(#{initial_index := IndexA}, #{initial_index := IndexB}) ->
            IndexA < IndexB
        end, NotMovedYet)).

move_and_unmark_current(ListWithMarkedCurrentElem, #{moves := Moves} = Current) ->
    NoMovesCurrent = Current#{moves := 0},
    {Before, After} = split(Current, ListWithMarkedCurrentElem),
    if
        Moves > 0 -> move_forward(Before, NoMovesCurrent, After, Moves, forward);
        Moves < 0 -> move_backwards(Before, NoMovesCurrent, After, Moves)
    end.

split(ExpectedElem, List) ->
    {_, Before, After} = lists:foldl(
        fun(Elem, {false, Before, After}) when ExpectedElem == Elem -> {true, Before, After};
           (Elem, {false, Before, After}) -> {false, [Elem | Before], After};
           (Elem, {true, Before, After}) -> {true, Before, [Elem | After]}
    end, {false, [], []}, List),
    {lists:reverse(Before), lists:reverse(After)}.

move_forward(Before, Current, After, Moves, _) when Moves =< 0 ->
    Before ++ [Current] ++ After;
move_forward(Before, Current, [], Moves, backwards) ->
    move_forward([], Current, Before, Moves - 1, backwards);
move_forward(Before, Current, [], Moves, forward) ->
    move_forward([], Current, Before, Moves, forward);
move_forward(Before, Current, After, Moves, Direction) when Moves > length(After) ->
    move_forward(Before ++ After, Current, [], Moves - length(After), Direction);
move_forward(Before, Current, [AfterH | AfterT], Moves, Direction) ->
    move_forward(Before ++ [AfterH], Current, AfterT, Moves - 1, Direction).


move_backwards(Before, Current, After, Moves) ->
    lists:reverse(move_forward(lists:reverse(After), Current, lists:reverse(Before), abs(Moves) + 1, backwards)).

build_list(InitialList) ->
    NumbersWithIndex = lists:zip(lists:seq(1, length(InitialList)), InitialList),
    lists:map(
        fun({Index, Number}) ->
            #{
                initial_index => Index,
                number => Number,
                moves => Number
            }
        end, NumbersWithIndex).

parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    StrNumbersLst = string:split(RawLstInput, "\n", all),
    lists:map(fun(StrNumber) -> {Num, ""} = string:to_integer(StrNumber), Num end, StrNumbersLst).

small_case_test() ->
    ?assertEqual([1, 2, -3, 4, 0, 3, -2], main(["./data/puzzle_20_test_input"])),
    ok.