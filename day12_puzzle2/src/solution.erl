-module(solution).

-export([main/1]).

-define(TIMEOUT, 360_000).

%%====================================================================
%% API functions
%%====================================================================

main(InputFile) ->
    {ok, RawBinaryInput} = file:read_file(InputFile),
    {_From, To, _Grid} = parse_data(RawBinaryInput),
    Answer = find_answer(To),
    io:format("Answer: ~p~n", [Answer]).

%%====================================================================
%% Internal functions
%%====================================================================

find_answer(To) ->
    {ok,#{<<"results">> := [#{<<"data">> := [#{<<"row">> := Answer}]}]}} = 
        find_shortest_path_length(To),
    Answer.

find_shortest_path_length({ToX, ToY}) ->
    Query = <<"MATCH
        (from:Point {value: $value} ),
        (to:Point {x: $to_x, y: $to_y} ),
        p = shortestPath((from)-[:STEP*]->(to))
    RETURN from, length(p) AS distance ORDER BY distance LIMIT 1;">>,
    Params = #{
        <<"value">> => $a,
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
