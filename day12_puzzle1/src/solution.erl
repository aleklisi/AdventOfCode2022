-module(solution).

-export([main/1]).
-export([load_data_to_db/1, find_answer/2]).

-define(TIMEOUT, 360_000).

%%====================================================================
%% API functions
%%====================================================================

load_data_to_db(InputFile) ->
    {ok, RawBinaryInput} = file:read_file(InputFile),
    {From, To, Grid} = parse_data(RawBinaryInput),
    change_grid_into_graph(Grid),
    {From, To}.

main(InputFile) ->
    {From, To} = load_data_to_db(InputFile),
    Answer = find_answer(From, To),    
    io:format("Answer: ~p~n", [Answer]).

%%====================================================================
%% Internal functions
%%====================================================================

find_answer(From, To) ->
    {ok,#{<<"results">> := [#{<<"data">> := [#{<<"row">> := [Answer]}]}]}} = 
        find_shortest_path_length(From, To),
    Answer.

change_grid_into_graph(Grid) ->
    create_nodes(Grid),
    io:format("Nodes loaded~n", []),
    MaxY = lists:max(lists:map(fun({_X, Y}) -> Y end, maps:keys(Grid))),
    MaxX = lists:max(lists:map(fun({X, _Y}) -> X end, maps:keys(Grid))),
    [create_relations(X, Y) || X <- lists:seq(1, MaxX), Y <- lists:seq(1, MaxY)],
    io:format("Relations created~n", []).

create_nodes(Grid) ->
    Query = <<"MERGE (n:Point { x: $x, y: $y, value: $value });">>,
    lists:map(
        fun(Point) ->
            Statement = create_node_statement(Point, maps:get(Point, Grid), Query),
            eneo4j:begin_and_commit_transaction([Statement], ?TIMEOUT)
        end, maps:keys(Grid)).

create_node_statement({X, Y}, Value, Query) ->
    Params = #{
      <<"x">> => X,
      <<"y">> => Y,
      <<"value">> => Value
      },
    eneo4j:build_statement(Query, Params).

create_relations(X, Y) ->
    Query = <<"MATCH
        (from:Point {x: $x, y: $y}),
        (to:Point)
    WHERE 
        (
            (from.x = to.x AND (from.y + 1 = to.y OR from.y - 1 = to.y)) OR
            (from.y = to.y AND (from.x + 1 = to.x OR from.x - 1 = to.x))
        ) AND (to.value <= from.value + 1)

    MERGE (from)-[r:STEP]->(to)
    RETURN type(r);">>,
    Statement = eneo4j:build_statement(Query, #{<<"x">> => X, <<"y">> => Y}),
    eneo4j:begin_and_commit_transaction([Statement], ?TIMEOUT).

find_shortest_path_length({FromX, FromY}, {ToX, ToY}) ->
    Query = <<"MATCH
        (from:Point {x: $from_x, y: $from_y} ),
        (to:Point {x: $to_x, y: $to_y} ),
        p = shortestPath((from)-[:STEP*]->(to))
    RETURN length(p);">>,
    Params = #{
        <<"from_x">> => FromX,
        <<"from_y">> => FromY,
        <<"to_x">> => ToX,
        <<"to_y">> => ToY
    },
    Statement = eneo4j:build_statement(Query, Params),
    eneo4j:begin_and_commit_transaction([Statement], ?TIMEOUT).


parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawRowsList = string:split(RawLstInput, "\n", all),
    RawRowsListWithIndexes = lists:zip(RawRowsList, lists:seq(1, length(RawRowsList))),
    Grid = maps:from_list(lists:flatten(
        lists:map(
            fun({Chars, Row}) ->
                chars_to_pos(Chars, Row)
            end, RawRowsListWithIndexes))),
    [{StartX, StartY}] = maps:keys(find_pos(Grid, $S)),
    [{EndX, EndY}] = maps:keys(find_pos(Grid, $E)),
    {{StartX, StartY}, {EndX, EndY}, Grid#{ {StartX, StartY} => $a, {EndX, EndY} => $z}}.

chars_to_pos(Chars, Y) ->
    [{{X, Y}, Char} || {Char, X} <- lists:zip(Chars, lists:seq(1, length(Chars)))].

find_pos(Grid, StartOrEndChar) ->
    maps:filter(
        fun(_Key, StartOrEnd) when StartOrEndChar == StartOrEnd -> true;
           (_, _) -> false end, Grid).
