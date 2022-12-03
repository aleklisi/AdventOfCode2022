-module(day3_puzzle1).

-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    ParsedRucksacks = parse_data(binary:bin_to_list(RawBinaryInput)),
    RucksackCompartmentsIntersections = lists:map(fun find_rucksack_compartments_intersection/1, ParsedRucksacks),
    IntersectionsPriorities = lists:map(
        fun(Intersection) ->
            [item_to_priority(Elem) || Elem <- Intersection]
        end, RucksackCompartmentsIntersections),
    IntersectionsPrioritiesSum = lists:sum(lists:flatten(IntersectionsPriorities)),
    io:format("IntersectionsPrioritiesSum: ~p~n", [IntersectionsPrioritiesSum]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

parse_data(RawLstInput) ->
    RawRucksacksList = string:split(RawLstInput, "\n", all),
    lists:map(
        fun(RawRucksacks) ->
            lists:split(round(length(RawRucksacks) / 2), RawRucksacks)
        end, RawRucksacksList).

find_rucksack_compartments_intersection({FirstCompartment, SecondCompartment}) ->
    SetIntersection = sets:intersection([
        sets:from_list(FirstCompartment),
        sets:from_list(SecondCompartment)
    ]),
    sets:to_list(SetIntersection).

item_to_priority($a) -> 1;
item_to_priority($z) -> 26;
item_to_priority(Item) when Item > $a andalso Item < $z -> Item - $a + 1;
item_to_priority($A) -> 27;
item_to_priority($Z) -> 52;
item_to_priority(Item) when Item > $A andalso Item < $Z -> Item - $A + 27.

%%====================================================================
%% Tests
%%====================================================================

% Lowercase item types a through z have priorities 1 through 26.
% Uppercase item types A through Z have priorities 27 through 52.
priority_conversions_are_correct_for_edge_cases_test() ->
    ?assertEqual(1, item_to_priority($a)),
    ?assertEqual(26, item_to_priority($z)),
    ?assertEqual(27, item_to_priority($A)),
    ?assertEqual(52, item_to_priority($Z)).

% the priority of the item type that appears in both compartments of each rucksack is
% 16 (p),
% 38 (L),
% 42 (P),
% 22 (v),
% 20 (t),
% 19 (s)
priority_conversions_are_correct_test() ->
    ?assertEqual(16, item_to_priority($p)),
    ?assertEqual(38, item_to_priority($L)),
    ?assertEqual(42, item_to_priority($P)),
    ?assertEqual(22, item_to_priority($v)),
    ?assertEqual(20, item_to_priority($t)),
    ?assertEqual(19, item_to_priority($s)).
