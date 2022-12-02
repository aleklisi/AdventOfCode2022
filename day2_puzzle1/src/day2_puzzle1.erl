-module(day2_puzzle1).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    ParsedGames = read_and_parse_data(hd(Args)),
    GamesScores = lists:map(fun count_games_score/1, ParsedGames),
    TotalScore = lists:sum(GamesScores),
    io:format("TotalScore: ~p~n", [TotalScore]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

count_games_score({OpponentMove, MyMove}) ->
    count_shape_score(MyMove) + count_result_score(OpponentMove, MyMove).

% The score for a single round is the score for the shape you selected (
% 1 for Rock,
% 2 for Paper,
% 3 for Scissors
count_shape_score(rock) -> 1;
count_shape_score(paper) -> 2;
count_shape_score(scissors) -> 3.

% ) plus the score for the outcome of the round (
% 0 if you lost,
% 3 if the round was a draw, 
% 6 if you won
% ).
count_result_score(scissors, rock) -> 6;
count_result_score(rock, paper) -> 6;
count_result_score(paper, scissors) -> 6;
count_result_score(rock, scissors) -> 0;
count_result_score(paper, rock) -> 0;
count_result_score(scissors, paper) -> 0;
count_result_score(OpponentMove, MyMove) when MyMove == OpponentMove -> 3.

read_and_parse_data(Filename) ->
    {ok, RawBinaryInput} = file:read_file(Filename),
    RawGamesInput = binary:bin_to_list(RawBinaryInput),
    RawGamesList = string:split(RawGamesInput, "\n", all),
    lists:map(
        fun(RawGame) ->
            [OpponentMove, MyMove] = string:split(RawGame, " "),
            {translate_opponent_move(OpponentMove), translate_my_move(MyMove)}
        end, RawGamesList).

% "The first column is what your opponent is going to play:
% A for Rock,
% B for Paper,
% C for Scissors.
translate_opponent_move("A") -> rock;
translate_opponent_move("B") -> paper;
translate_opponent_move("C") -> scissors.

% The second column, you reason, must be what you should play in response:
% X for Rock,
% Y for Paper,
% Z for Scissors.
translate_my_move("X") -> rock;
translate_my_move("Y") -> paper;
translate_my_move("Z") -> scissors.


