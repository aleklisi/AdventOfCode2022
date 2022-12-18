-module(day18_puzzle1).

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
    AllCubesSet = sets:from_list(Cubes),
    lists:sum(lists:map(
        fun(Cube) ->
            length(find_walls_not_touching_any_other_cube(Cube, AllCubesSet))
        end, Cubes)).

find_walls_not_touching_any_other_cube(_Cube = {X, Y, Z}, AllCubesSet) ->
    PotentialNeighbors = [
        {X + 1, Y, Z},
        {X - 1, Y, Z},
        {X, Y + 1, Z},
        {X, Y - 1, Z},
        {X, Y, Z + 1},
        {X, Y, Z - 1}
    ],
    lists:filter(fun(Neighbor) ->
        not sets:is_element(Neighbor, AllCubesSet)
    end, PotentialNeighbors).

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
    