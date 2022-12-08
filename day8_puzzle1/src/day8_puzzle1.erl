-module(day8_puzzle1).

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
    IfTreesAreVisible = [
        is_visible(X, Y, GridWidth, GridHeight, TreesMap)
        || X <- lists:seq(1, GridWidth), Y <- lists:seq(1, GridHeight)],
    Answer = length(lists:filter(fun(X) -> X end, IfTreesAreVisible)),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

is_visible(X, Y, GridWidth, GridHeight, TreesMap) ->
    CurrentTreeHeight = maps:get({Y, X}, TreesMap),
    
    VisibilitySides = [
        [maps:get({Row, Col}, TreesMap) || Col <- lists:seq(1, X - 1), Row <- [Y]],
        [maps:get({Row, Col}, TreesMap) || Col <- lists:seq(X + 1, GridWidth), Row <- [Y]],
        [maps:get({Row, Col}, TreesMap) || Row <- lists:seq(1, Y - 1), Col <- [X]],
        [maps:get({Row, Col}, TreesMap) || Row <- lists:seq(Y + 1, GridHeight), Col <- [X]]
    ],
    lists:any(
        fun(VisibilitySide) ->
            lists:all(
                fun(VisibilitySideTree) ->
                VisibilitySideTree < CurrentTreeHeight
            end, VisibilitySide)
        end, VisibilitySides).
    

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


grid_positive_test() ->
    RawGrid = <<"30373\n25512\n65332\n33549\n35390">>,
    {Width, Height, TreesMap} = parse_data(RawGrid),
    % The top-left 5 is visible from the left and top. (It isn't visible from the right or bottom since other trees of height 5 are in the way.)
    ?assert(is_visible(2, 2, Width, Height, TreesMap)),
    % The top-right 1 is not visible from any direction; for it to be visible, there would need to only be trees of height 0 between it and an edge.
    ?assert(not is_visible(4, 2, Width, Height, TreesMap)),
    % The left-middle 5 is visible, but only from the right.
    ?assert(is_visible(3, 2, Width, Height, TreesMap)),
    % The center 3 is not visible from any direction; for it to be visible, there would need to be only trees of at most height 2 between it and an edge.
    ?assert(not is_visible(3, 3, Width, Height, TreesMap)),
    % The right-middle 3 is visible from the right.
    ?assert(is_visible(3, 4, Width, Height, TreesMap)),
    % In the bottom row, the middle 5 is visible, but the 3 and 4 are not.
    ?assert(is_visible(4, 3, Width, Height, TreesMap)),
    ?assert(not is_visible(4, 2, Width, Height, TreesMap)),
    ?assert(not is_visible(4, 4, Width, Height, TreesMap)),
    ok.