-module(day10_puzzle1).

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
    % Find the signal strength during the 
    % 20th, 60th, 100th, 140th, 180th, and 220th cycles.
    % What is the sum of these six signal strengths?
    SearchedSignalsNumbers = [20, 60, 100, 140, 180, 220],
    Answer = lists:sum(lists:map(
        fun(CycleNumber) ->
            Cycle = lists:nth(CycleNumber, ExecutionHistory),
            signal_strength(Cycle)
        end ,SearchedSignalsNumbers)),
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

% signal strength (the cycle number multiplied by the value of the X register)
signal_strength({_Instruction, CycleNumber, XRegisterValue}) ->
    CycleNumber * XRegisterValue.
