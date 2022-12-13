-module(day13_puzzle1).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    Packets = parse_data(RawBinaryInput),
    
    % Uncomment to see the trace in the output
    % dbg:tracer(),
    % dbg:p(all, c),
    % dbg:tpl(?MODULE, compare, x),

    PacketsInOrder = find_packets_in_order(Packets),
    Answer = lists:sum(lists:map(
        fun({_, Index}) -> Index end, PacketsInOrder)),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

find_packets_in_order(Packets) ->
    lists:filter(fun({{Left, Right}, _Index}) -> compare(Left, Right) == in_order end, Packets).

parse_data(RawBinaryInput) ->
    RawPacketsList = string:split(RawBinaryInput, "\n\n", all),
    PacketPairs = lists:map(fun parse_packet_pair/1, RawPacketsList),
    lists:zip(PacketPairs, lists:seq(1, length(PacketPairs))).

parse_packet_pair(RawPacketsPair) ->
    [LeftRawPacket, RightRawPacket] = string:split(RawPacketsPair, "\n"),
    LeftPacket = to_term(LeftRawPacket),
    RightPacket = to_term(RightRawPacket),
    {LeftPacket, RightPacket}.

to_term(Input) ->
    String = binary:bin_to_list(Input) ++ ".",
    {ok,Tokens,_EndLine} = erl_scan:string(String),
    {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
    {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
    Value.

% If the left integer is lower than the right integer,
% the inputs are in the right order.
compare(Left, Right)
    when is_integer(Left), is_integer(Right), Left < Right -> in_order;
% If the left integer is higher than the right integer,
% the inputs are not in the right order. 
compare(Left, Right)
    when is_integer(Left), is_integer(Right), Left > Right -> not_in_order;
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
compare([], _) -> in_order;
% If the right list runs out of items first, the inputs are not in the right order.
compare(_, []) -> not_in_order;
% If both values are lists,
% compare the first value of each list,
% then the second value, and so on.
compare([LeftHead | LeftTail], [RightHead | RightTail]) ->
    case compare(LeftHead, RightHead) of
        continue -> compare(LeftTail, RightTail);
        in_order -> in_order;
        not_in_order -> not_in_order
    end.
