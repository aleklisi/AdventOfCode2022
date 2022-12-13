-module(day13_puzzle2).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    Packets = parse_data(RawBinaryInput),
    DividerPackets = [{[[2]]}, {[[6]]}],
    AllPackets = lists:flatten(DividerPackets ++ Packets),
    
    AllPacketsSorted = lists:sort(fun({A}, {B}) -> compare(A, B) end, AllPackets),
    
    {FirstDivider, _} = lists:keyfind({[[2]]}, 2, lists:enumerate(AllPacketsSorted)),
    {SecondDivider, _} = lists:keyfind({[[6]]}, 2, lists:enumerate(AllPacketsSorted)),
    Answer = FirstDivider * SecondDivider,
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================


parse_data(RawBinaryInput) ->
    RawPacketsList = string:split(RawBinaryInput, "\n\n", all),
    lists:map(fun parse_packet_pair/1, RawPacketsList).

parse_packet_pair(RawPacketsPair) ->
    [LeftRawPacket, RightRawPacket] = string:split(RawPacketsPair, "\n"),
    LeftPacket = to_term(LeftRawPacket),
    RightPacket = to_term(RightRawPacket),
    [{LeftPacket}, {RightPacket}].

to_term(Input) ->
    String = binary:bin_to_list(Input) ++ ".",
    {ok,Tokens,_EndLine} = erl_scan:string(String),
    {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
    {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

% If the left integer is lower than the right integer,
% the inputs are in the right order.
compare(Left, Right)
    when is_integer(Left), is_integer(Right), Left < Right -> true;
% If the left integer is higher than the right integer,
% the inputs are not in the right order. 
compare(Left, Right)
    when is_integer(Left), is_integer(Right), Left > Right -> false;
% Otherwise, the inputs are the same integer;
% continue checking the next part of the input.
compare(Left, Right)
    when is_integer(Left), is_integer(Right), Left == Right -> continue;
% If exactly one value is an integer,
% convert the integer to a list which contains that integer as its only value,
% then retry the comparison.
compare(Left, Right) when is_list(Left), is_integer(Right) ->
    compare(Left, [Right]);
compare(Left, Right) when is_integer(Left), is_list(Right) ->
    compare([Left], Right);
% % If the lists are the same length and no comparison makes a decision about the order, continue checking the next part of the input.
compare([], []) -> continue;
% If the left list runs out of items first, the inputs are in the right order.
compare([], _) -> true;
% If the right list runs out of items first, the inputs are not in the right order.
compare(_, []) -> false;
% If both values are lists,
% compare the first value of each list,
% then the second value, and so on.
compare([LeftHead | LeftTail], [RightHead | RightTail]) ->
    case compare(LeftHead, RightHead) of
        continue -> compare(LeftTail, RightTail);
        true -> true;
        false -> false
    end.
