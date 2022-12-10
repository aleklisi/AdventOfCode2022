-module(day10_puzzle2).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    Instructions = parse_data(RawBinaryInput),
    {_FinalX, ExecutionHistory} = build_cpu_execution_history(Instructions),
    Answer = run_crt(ExecutionHistory),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

parse_data(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawInstructionsList = string:split(RawLstInput, "\n", all),
    lists:map(fun parse_instruction/1, RawInstructionsList).

parse_instruction("noop") -> noop;
parse_instruction(RawAddX) ->
    [_, StrV] = string:split(RawAddX, " "),
    {V, ""} = string:to_integer(StrV),
    {addx, V}.

build_cpu_execution_history(Instructions) ->
    #{x := FinalX, history := TotalHistory}
        = lists:foldl(
            fun build_execution_step/2,
            #{x => 1, history => [], cycle_counter => 1},
            Instructions),
    {FinalX, lists:reverse(TotalHistory)}.

build_execution_step(noop,
    #{x := X, history := History, cycle_counter := Count}) ->
        #{
            x => X,
            history => [{noop, Count, X} | History],
            cycle_counter => Count + 1
        };
build_execution_step({addx, V}, 
    #{x := X, history := History, cycle_counter := Count}) ->
        NewX = X + V,
        #{
            x => NewX,
            history => [{addx, Count + 1, X}, {addx, Count, X} | History],
            cycle_counter => Count + 2
        }.

run_crt(ExecutionHistory) ->
    CRTPositions = [{X, Y} || Y <- lists:seq(1, 6), X <- lists:seq(0, 39)],
    CRTMap = maps:from_list(
        lists:map(
            fun render_pixel/1,
            lists:zip(CRTPositions, ExecutionHistory))),
    [[maps:get({X, Y}, CRTMap) || X <- lists:seq(0, 39)] || Y <- lists:seq(1, 6)].

% If the sprite is positioned such that 
% one of its three pixels is the pixel currently being drawn,
% the screen produces a lit pixel (#);
% otherwise, the screen leaves the pixel dark (.).
render_pixel({{CRTX, CRTY}, {_, _, RegisterXValue}}) ->
    SpritePixels = [RegisterXValue + Shift || Shift <- [-1, 0, 1]],
    Char = case lists:member(CRTX, SpritePixels) of
        false -> {{CRTX, CRTY}, $.};
        true -> {{CRTX, CRTY}, $#}
    end,
    Char.