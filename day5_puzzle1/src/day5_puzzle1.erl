-module(day5_puzzle1).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    {InitialStacks, Commands} = parse_data(binary:bin_to_list(RawBinaryInput)),
    FinalStacks = lists:foldl(fun execute_command/2, InitialStacks, Commands),
    io:format("Final Stacks Tops: ~p~n", [get_stacks_tops(FinalStacks)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

get_stacks_tops(Stacks) ->
    StacksList = maps:to_list(Stacks),
    SortedStacksList = lists:keysort(1, StacksList),
    lists:map(fun({_, [Top | _]}) -> Top end, SortedStacksList).

execute_command({HowMany, From, To}, Stacks) ->
    Moves = [{From, To} || _ <- lists:seq(1, HowMany)],
    lists:foldl(fun move_one_crate/2, Stacks, Moves).

move_one_crate({From, To}, Stacks) ->
    [Element | NewFromStack] = maps:get(From, Stacks),
    ToStack = maps:get(To, Stacks),
    NewStacks1 = maps:put(From, NewFromStack, Stacks),
    maps:put(To, [Element | ToStack], NewStacks1).

parse_data(RawLstInput) ->
    [RawInitialStacks, RawCommands] = string:split(RawLstInput, "\n\n"),
    InitialStacks = parse_initial_stacks(RawInitialStacks),
    Commands = parse_commands(RawCommands),
    {InitialStacks, Commands}.

parse_initial_stacks(RawInitialStacks) ->
    RawInitialStacksHorizontalLst = string:split(RawInitialStacks, "\n", all),
    StacksHorizontal = lists:droplast(RawInitialStacksHorizontalLst),
    LinesHorizontal = lists:map(fun line_to_crates/1, StacksHorizontal),
    StacksWithEmptyMarkers = horizontal_lines_to_stacks(LinesHorizontal),
    NonEmptyStacksWithEmptyMarkers = lists:filter(
        fun([]) -> false;(_) -> true end, StacksWithEmptyMarkers),
    StacksWithOutEmptyMarkers = lists:map(
        fun(Stack) ->
            lists:dropwhile(
                fun(empty) -> true;
                   (_) -> false
                end, Stack)
        end, NonEmptyStacksWithEmptyMarkers),
    StacksNumbers = lists:seq(1, length(StacksWithOutEmptyMarkers)),
    maps:from_list(lists:zip(StacksNumbers, StacksWithOutEmptyMarkers)).

% 32 is ASCII dec for space character
line_to_crates("") -> [];
line_to_crates([ $[, Char, $] ]) -> [Char];
line_to_crates([ 32, 32, 32 ]) -> [empty];
line_to_crates([ 32, 32, 32, 32 | Rest ]) -> [empty | line_to_crates(Rest)];
line_to_crates([ $[, Char, $], 32 | Rest]) -> [Char | line_to_crates(Rest)].

horizontal_lines_to_stacks([]) -> [];
horizontal_lines_to_stacks(Lines) ->
    CurrentStack = [Head || [Head | _Rest] <- Lines],
    FollowingStacks = horizontal_lines_to_stacks([Rest || [_Head | Rest] <- Lines]),
    [CurrentStack | FollowingStacks].
    

parse_commands(RawCommands) ->
    RawCommandsLst = string:split(RawCommands, "\n", all),
    lists:map(fun parse_command/1, RawCommandsLst).

parse_command(RawCommand) ->
    % "move 1 from 7 to 6"
    [_, HowManyCrates, _, From, _, To] = string:split(RawCommand, " ", all),
    list_to_tuple(lists:map(
        fun(StrNum) ->
            {Result, ""} = string:to_integer(StrNum),
            Result
        end, [HowManyCrates, From, To])).