-module(day1_puzzle2).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    ParsedData = read_and_pase_data(hd(Args)),
    [
        {Elf1, CaloriesTotal1},
        {Elf2, CaloriesTotal2},
        {Elf3, CaloriesTotal3}
    ]
     = find_3_elves_carrying_most_calories(ParsedData),
    io:format(
        "The elves: ~p carry most calories, which is ~p calories total\n",
        [[Elf1, Elf2, Elf3], lists:sum(CaloriesTotal1 ++ CaloriesTotal2 ++ CaloriesTotal3)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

% The data returned for parsing is a list of tuples.
% The first element in a tuple represents an elf number.
% The second element in a tuple represents a list of items (calories that the elf caries).
% For the puzzle_1_test_input, the output of this function is:
% [
%   {1,[1000,2000,3000]},
%   {2,[4000]},
%   {3,[5000,6000]},
%   {4,[7000,8000,9000]},
%   {5,[10000]}
% ]
read_and_pase_data(Filename) ->
    {ok, RawBinaryInput} = file:read_file(Filename),
    RawListInput = binary:bin_to_list(RawBinaryInput),
    ElvesRawInput = string:split(RawListInput, "\n\n", all),
    ElvesCalories = lists:map(
        fun(ElfRawInput) ->
            StringCaloriesLst = string:split(ElfRawInput, "\n", all),
            lists:map(
                fun(StringCalories) ->
                    {IntCalories, ""} = string:to_integer(StringCalories),
                    IntCalories
                end, StringCaloriesLst)
        end, ElvesRawInput),
    lists:zip(lists:seq(1, length(ElvesCalories)), ElvesCalories).

find_3_elves_carrying_most_calories(ParsedElves) ->
    ElfWithTopCalories1 = find_elf_carrying_most_calories(ParsedElves),
    ParsedElvesWithoutTopOne = ParsedElves -- [ElfWithTopCalories1],
    ElfWithTopCalories2 = find_elf_carrying_most_calories(ParsedElvesWithoutTopOne),
    ParsedElvesWithoutTopTwo = ParsedElvesWithoutTopOne -- [ElfWithTopCalories2],
    ElfWithTopCalories3 = find_elf_carrying_most_calories(ParsedElvesWithoutTopTwo),
    [
        ElfWithTopCalories1,
        ElfWithTopCalories2,
        ElfWithTopCalories3
    ].

% This is effectively a max function finding an elf with a sum of calories being the biggest.
find_elf_carrying_most_calories([FirstElf | OtherElves]) ->
    lists:foldl(
        fun({ElfA, CaloriesA}, {ElfB, CaloriesB}) ->
            case lists:sum(CaloriesA) >= lists:sum(CaloriesB) of
                true -> {ElfA, CaloriesA};
                false -> {ElfB, CaloriesB}
            end
        end, FirstElf, OtherElves).