-module(day18_puzzle2).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    Cubes = parse_data(RawBinaryInput),
    Answer = count_common_walls(Cubes),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

count_common_walls(Cubes) ->
    MinX = lists:min(lists:map(fun({X, _Y, _Z}) -> X end, Cubes)) - 1,
    MaxX = lists:max(lists:map(fun({X, _Y, _Z}) -> X end, Cubes)) + 1,
    MinY = lists:min(lists:map(fun({_X, Y, _Z}) -> Y end, Cubes)) - 1,
    MaxY = lists:max(lists:map(fun({_X, Y, _Z}) -> Y end, Cubes)) + 1,
    MinZ = lists:min(lists:map(fun({_X, _Y, Z}) -> Z end, Cubes)) - 1,
    MaxZ = lists:max(lists:map(fun({_X, _Y, Z}) -> Z end, Cubes)) + 1,
    CubesLimits = [MinX, MaxX, MinY, MaxY, MinZ, MaxZ],
    AllCubesSet = sets:from_list(Cubes),
    lists:sum(lists:map(
        fun(Cube) ->
            length(find_external_walls_not_touching_any_other_cube(Cube, CubesLimits, AllCubesSet))
        end, Cubes)).

find_external_walls_not_touching_any_other_cube(Cube, CubesLimits, AllCubesSet) ->
    PotentialNeighbors = potential_neighbors(Cube),
    lists:filter(fun(Neighbor) ->
        (not sets:is_element(Neighbor, AllCubesSet)) andalso (not is_enclosed([Neighbor], 0, 1, CubesLimits, AllCubesSet))
    end, PotentialNeighbors).

is_enclosed(_Neighbors, PrevCubesCount, CubesCount, _CubesLimits, _AllCubesSet) when PrevCubesCount == CubesCount ->
    true;
is_enclosed(Neighbors, _PrevCubesCount, CubesCount, CubesLimits, AllCubesSet) ->
    case lists:any(fun(Neighbor) -> has_reached_box_border(Neighbor, CubesLimits) end, Neighbors) of
        true -> false;
    false ->
        NewNeighbors = lists:uniq(
            lists:flatten(
                lists:map(
                    fun(Neighbor) ->
                        lists:filter(fun(N) -> not sets:is_element(N, AllCubesSet) end, [Neighbor | potential_neighbors(Neighbor)])
                    end, Neighbors))),
        is_enclosed(NewNeighbors, CubesCount, length(NewNeighbors), CubesLimits, AllCubesSet)
                end.

has_reached_box_border({X, Y, Z}, [MinX, MaxX, MinY, MaxY, MinZ, MaxZ]) ->
    X =< MinX orelse X >= MaxX orelse
    Y =< MinY orelse Y >= MaxY orelse
    Z =< MinZ orelse Z >= MaxZ.


potential_neighbors(_Cube = {X, Y, Z}) ->
    [
        {X + 1, Y, Z},
        {X - 1, Y, Z},
        {X, Y + 1, Z},
        {X, Y - 1, Z},
        {X, Y, Z + 1},
        {X, Y, Z - 1}
    ].

parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawCubes = string:split(RawLstInput, "\n", all),
    lists:map(fun parse_cube/1, RawCubes).

parse_cube(RawCube) ->
    list_to_tuple(
    lists:map(
        fun(StrDimention) ->
            {Dimention, ""} = string:to_integer(StrDimention),
            Dimention
        end, string:split(RawCube, ",", all))).
    