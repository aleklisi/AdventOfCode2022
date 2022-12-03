-module(day3_puzzle2).

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
    Badges = lists:map(fun find_badge/1, ParsedRucksacks),
    BadgesPriorities = lists:map(
        fun(Badge) ->
            [item_to_priority(Char) ||Char <- Badge]
        end, Badges),
    PrioritiesSum = lists:sum(lists:flatten(BadgesPriorities)),
    io:format("PrioritiesSum: ~p~n", [PrioritiesSum]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

parse_data(RawLstInput) ->
    RawRucksacksList = string:split(RawLstInput, "\n", all),
    group_into_threes(RawRucksacksList).

group_into_threes([]) -> [];
group_into_threes([First, Second, Third | Rest]) ->
    [[First, Second, Third] | group_into_threes(Rest)].

% The only way to tell which item type is the right one is by 
% finding the one item type that is common between all three Elves in each group.
find_badge(Rucksacks) ->
    SetBadge = sets:intersection([sets:from_list(Rucksack)|| Rucksack <- Rucksacks]),
    sets:to_list(SetBadge).

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
