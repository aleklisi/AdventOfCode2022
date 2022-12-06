-module(day6_puzzle2).

-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    Subroutine = binary:bin_to_list(RawBinaryInput),
    StartMarkerPosition = find_start_of_message_marker(Subroutine),
    io:format("StartMarkerPosition: ~p~n", [StartMarkerPosition]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

find_start_of_message_marker(Subroutine) ->
    find_start_of_packet_marker(14, Subroutine).

find_start_of_packet_marker(MarkerPos, [_ | Rest] = Subroutine) ->
    case first_14_chars_are_all_different(Subroutine) of
        true -> MarkerPos;
        false -> find_start_of_packet_marker(MarkerPos + 1, Rest)
    end.

first_14_chars_are_all_different(Lst) ->
    {First14Chars, _Rest} = lists:split(14, Lst),
    sets:size(sets:from_list(First14Chars)) == 14.

finding_start_of_packet_works_for_included_examples_test() ->
    ?assertEqual(19, find_start_of_message_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb")),
    ?assertEqual(23, find_start_of_message_marker("bvwbjplbgvbhsrlpgdmjqwftvncz")),
    ?assertEqual(23, find_start_of_message_marker("nppdvjthqldpwncqszvftbrmjlhg")),
    ?assertEqual(29, find_start_of_message_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")),
    ?assertEqual(26, find_start_of_message_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")).

