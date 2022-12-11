-module(day11_puzzle2).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    InitialMonkeys = parse_data(RawBinaryInput),
    Rounds = 10_000,
    PostSimulationMonkeys = lists:foldl(
        fun(Round, Monkeys) -> 
            SimulatedMonkeys = simulate_round(Monkeys),
            case Round rem 100 of
                0 ->
                    normalize(SimulatedMonkeys);
                _ -> SimulatedMonkeys
            end
        end, InitialMonkeys, lists:seq(1, Rounds)),

    InspectedInspectCounts = lists:map(
        fun(#{items_inspected_count := ItemsCount}) ->
            ItemsCount
        end, maps:values(PostSimulationMonkeys)),
    MaxItemCount = lists:max(InspectedInspectCounts),
    MaxItemCount2 = lists:max(InspectedInspectCounts -- [MaxItemCount]),
    Answer = MaxItemCount * MaxItemCount2,
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

normalize(SimulatedMonkeys) ->
    Monkeys = maps:values(SimulatedMonkeys),
    TestVals = lists:map(fun(#{test := Test}) -> Test end, Monkeys),
    Rem = lists:foldl(fun(Elem, Acc) -> Elem * Acc end, 1, TestVals),
    maps:map(fun(_, Monkey = #{items := Items}) ->
        NormalizedItems = lists:map(fun(Item) -> Item rem Rem end, Items),
        Monkey#{items := NormalizedItems}
     end, SimulatedMonkeys).

simulate_round(Monkeys) ->
    lists:foldl(fun simulate_monkey/2, Monkeys, lists:seq(0, maps:size(Monkeys) - 1)).

simulate_monkey(CurrentMonkeyIndex, Monkeys) ->
    CurrentMonkey = #{
        items := Items,
        operation := Operation,
        test := TestNum,
        if_true := IfTrue,
        if_false := IfFalse,
        items_inspected_count := InspectedItemsCount
    } = maps:get(CurrentMonkeyIndex, Monkeys),
    lists:foldl(
        fun(Item, MMonkeys) ->
            inspect_test_pass(Item, MMonkeys, Operation, TestNum, IfTrue, IfFalse)
        end,
        maps:put(
            CurrentMonkeyIndex,
            CurrentMonkey#{items => [], items_inspected_count => InspectedItemsCount + length(Items)},
            Monkeys),
        Items).

inspect_test_pass(Item, Monkeys, Operation, TestNum, IfTrue, IfFalse) ->
    % You're worried you might not ever get your items back.
    % So worried, in fact, that your relief that 
    % a monkey's inspection didn't damage an item no longer causes your worry level to be divided by three.
    % NewWorryLevel = round(math:floor(Operation(Item) / 3)),
    NewWorryLevel = Operation(Item),
    PassTo = case (NewWorryLevel rem TestNum) == 0 of
        true -> IfTrue;
        false -> IfFalse
        end,
    add_item(NewWorryLevel, PassTo, Monkeys).



add_item(Item, MonkeyNum, Monkeys) ->
    Monkey = maps:get(MonkeyNum, Monkeys),
    MonkeyItems = maps:get(items, Monkey),
    NewMonkey = maps:put(items, MonkeyItems ++ [Item], Monkey),
    maps:put(MonkeyNum, NewMonkey, Monkeys).

parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawMonkeysList = string:split(RawLstInput, "\n\n", all),
    maps:from_list(
        lists:map(fun parse_monkey/1, RawMonkeysList)
    ).

parse_monkey(RawMonkeyStr) ->
    [
        MonkeyNumRow,
        StartItemsRow,
        OperationRow,
        TestRow,
        IfTrueRow,
        IfFalseRow
    ] = string:split(RawMonkeyStr, "\n", all),
    MonkeyNum = parse_monkey_num(MonkeyNumRow),
    {MonkeyNum, #{
        num => MonkeyNum,
        items => parse_start_items(StartItemsRow),
        operation => parse_operation(OperationRow),
        test => parse_test_row(TestRow),
        if_true => parse_condition(IfTrueRow),
        if_false => parse_condition(IfFalseRow),
        items_inspected_count => 0
    }}.
parse_monkey_num(MonkeyNumRow) ->
    [_Label, RawNum] = string:split(MonkeyNumRow, " "),
    {Num, ":"} = string:to_integer(RawNum),
    Num.

parse_start_items(StartItemsRow) ->
    [_Label, RawItemsTxt] = string:split(StartItemsRow, ": "),
    RawItemsList = string:split(RawItemsTxt, ", ", all),
    lists:map(
        fun(RawItem) ->
            {Item, ""} = string:to_integer(RawItem),
            Item
        end, RawItemsList).

parse_operation(OperationRow) ->
    [_Label, RawOperationTxt] = string:split(OperationRow, "new = "),
    [LeftElemRaw, OperatorRaw, RightElemRaw] = string:split(RawOperationTxt, " ", all),
    fun(Old) ->
        Left = case string:to_integer(LeftElemRaw) of
            {LeftInt, ""} -> LeftInt;
            {error, no_integer} -> Old
        end,
        Operator = case OperatorRaw of
            "*" -> fun erlang:'*'/2;
            "+" -> fun erlang:'+'/2
        end,
        Right = case string:to_integer(RightElemRaw) of
            {RightInt, ""} -> RightInt;
            {error, no_integer} -> Old
        end,
        Operator(Left, Right)
    end.

parse_test_row(TestRow) ->
    [_Label, DividerRaw] = string:split(TestRow, " by "),
    {Divider, ""} = string:to_integer(DividerRaw),
    Divider.

parse_condition(ConditionRow) ->
    [_Label, DestinationMonkeyRaw] = string:split(ConditionRow, "monkey "),
    {DestinationMonkey, ""} = string:to_integer(DestinationMonkeyRaw),
    DestinationMonkey.
