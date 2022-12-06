-module(day6_puzzle1).

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
    StartMarkerPosition = find_start_of_packet_marker(Subroutine),
    io:format("StartMarkerPosition: ~p~n", [StartMarkerPosition]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

find_start_of_packet_marker(Subroutine) ->
    find_start_of_packet_marker(4, Subroutine).

find_start_of_packet_marker(MarkerPos, [_ | Rest] = Subroutine) ->
    case first_4_chars_are_all_different(Subroutine) of
        true -> MarkerPos;
        false -> find_start_of_packet_marker(MarkerPos + 1, Rest)
    end.

first_4_chars_are_all_different([A, B, C, D | _]) ->
    A /= B andalso A /= C andalso A /= D
           andalso B /= C andalso B /= D
                          andalso C /= D.

finding_start_of_packet_works_for_included_examples_test() ->
    ?assertEqual(5, find_start_of_packet_marker("bvwbjplbgvbhsrlpgdmjqwftvncz")),
    ?assertEqual(6, find_start_of_packet_marker("nppdvjthqldpwncqszvftbrmjlhg")),
    ?assertEqual(10, find_start_of_packet_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg")),
    ?assertEqual(11, find_start_of_packet_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw")).

