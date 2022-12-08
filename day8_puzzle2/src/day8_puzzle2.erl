-module(day8_puzzle2).

-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    {GridWidth, GridHeight, TreesMap} = parse_data(RawBinaryInput),
    ScenicScores = [
        count_scenic_score(X, Y, GridWidth, GridHeight, TreesMap)
        || X <- lists:seq(1, GridWidth), Y <- lists:seq(1, GridHeight)],
    Answer = lists:max(ScenicScores),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

count_scenic_score(X, Y, GridWidth, GridHeight, TreesMap) ->
    CurrentTreeHeight = maps:get({Y, X}, TreesMap),
    VisibilitySides = [
        lists:reverse([maps:get({Row, Col}, TreesMap) || Col <- lists:seq(1, X - 1), Row <- [Y]]),
        [maps:get({Row, Col}, TreesMap) || Col <- lists:seq(X + 1, GridWidth), Row <- [Y]],
        lists:reverse([maps:get({Row, Col}, TreesMap) || Row <- lists:seq(1, Y - 1), Col <- [X]]),
        [maps:get({Row, Col}, TreesMap) || Row <- lists:seq(Y + 1, GridHeight), Col <- [X]]
    ],
    VisibilitySideScores = lists:map(
        fun(VisibilitySide) ->
            VisibleView = take_while(CurrentTreeHeight, VisibilitySide),
            length(VisibleView)
        end, VisibilitySides),
    lists:foldl(fun(E, A) -> E * A end, 1, VisibilitySideScores).

take_while(_TreeHeight, []) -> [];
take_while(CurrentTreeHeight, [ViewTreeHeight | _])
    when CurrentTreeHeight =< ViewTreeHeight -> [ViewTreeHeight];
take_while(CurrentTreeHeight, [ViewTreeHeight | Rest])
    when CurrentTreeHeight > ViewTreeHeight ->
        [ViewTreeHeight | take_while(CurrentTreeHeight, Rest)].


parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawTreeLinesList = string:split(RawLstInput, "\n", all),
    GridHeight = length(RawTreeLinesList),
    GridWidth = length(hd(RawTreeLinesList)),
    ColumnIndexes = lists:seq(1, GridWidth),
    RawTreeLinesListWithIndexes = lists:zip(RawTreeLinesList, lists:seq(1, GridHeight)),
    RowsMapsLst = lists:map(
        fun({RawTreeLine, RowIndex}) ->
            TreeHeights = lists:map(
                fun(StrTreeHeight) ->
                    {TreeHeight, ""} = string:to_integer([StrTreeHeight]),
                    TreeHeight
                end, RawTreeLine),
            maps:from_list(lists:zip(
                [{RowIndex, ColumnIndex} || ColumnIndex <- ColumnIndexes],
                TreeHeights))
        end, RawTreeLinesListWithIndexes),
    {GridWidth, GridHeight, lists:foldl(fun maps:merge/2, #{}, RowsMapsLst)}.

sth_test() ->
    RawBinaryInput = <<"30373\n25512\n65332\n33549\n35390">>,
    {GridWidth, GridHeight, TreesMap} = parse_data(RawBinaryInput),
    ?assertEqual(4, count_scenic_score(3, 2, GridWidth, GridHeight, TreesMap)),
    ?assertEqual(8, count_scenic_score(3, 4, GridWidth, GridHeight, TreesMap)).