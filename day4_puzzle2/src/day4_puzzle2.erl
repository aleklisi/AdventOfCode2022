-module(day4_puzzle2).

-include_lib("eunit/include/eunit.hrl").

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    ParsedSections = parse_data(binary:bin_to_list(RawBinaryInput)),
    SectionsContainingOneAnother = lists:filter(fun overlap/1, ParsedSections),
    io:format("SectionsContainingOneAnotherCount: ~p~n", [length(SectionsContainingOneAnother)]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

parse_data(RawLstInput) ->
    RawLinesList = string:split(RawLstInput, "\n", all),
    lists:map(fun(RawLine) ->
        [FirstRawSection, SecondRawSection] = string:split(RawLine, ","),
        #{
            first_section => parse_section(FirstRawSection),
            second_section => parse_section(SecondRawSection)
        }
    end, RawLinesList).

parse_section(RawSection) ->
    [FromRaw, ToRaw] = string:split(RawSection, "-"),
    {From, ""} = string:to_integer(FromRaw),
    {To, ""} = string:to_integer(ToRaw),
    #{from => From, to => To}.

% In how many assignment pairs do the ranges overlap?
overlap(#{first_section := #{from := FromF, to := ToF},
         second_section := #{from := FromS, to := ToS}}) ->
    % Result = sets:size(sets:intersection([
    %     sets:from_list(lists:seq(FromF, ToF)),
    %     sets:from_list(lists:seq(FromS, ToS))
    % ])) > 0,
    (FromF =< FromS andalso FromS =< ToF)
    orelse
    (FromS =< FromF andalso FromF =< ToS)
    orelse
    (FromF =< ToS andalso ToS =< ToF)
    orelse
    (FromS =< ToF andalso ToF =< ToS).

%%====================================================================
%% Tests
%%====================================================================

% In the above example, the first two pairs (2-4,6-8 and 2-3,4-5) don't overlap.
do_not_overlap_test() ->
    ?assertEqual(false, overlap(#{
        first_section => #{from => 2, to => 4},
        second_section => #{from => 6, to => 8}})),
    ?assertEqual(false, overlap(#{
        first_section => #{from => 2, to => 3},
        second_section => #{from => 4, to => 5}})).

% 5-7,7-9 overlaps in a single section, 7.
% 2-8,3-7 overlaps all of the sections 3 through 7.
% 6-6,4-6 overlaps in a single section, 6.
% 2-6,4-8 overlaps in sections 4, 5, and 6.
do_overlap_test() ->
    Cases = [
        {{5, 7},{7, 9}},
        {{2, 8},{3, 7}},
        {{6, 6},{4, 6}},
        {{2, 6},{4, 8}}
    ],
    [?assertEqual(true, overlap(#{
        first_section => #{from => FromF, to => ToF},
        second_section => #{from => FromS, to => ToS}}))
        ||{{FromF, ToF}, {FromS, ToS}} <- Cases].

% My custom tests
do_overlap_sections_test() ->
    Cases = [
        {{2, 5},{5, 7}},
        {{2, 6},{5, 7}},
        {{2, 7},{5, 7}},
        {{2, 8},{5, 7}},
        {{2, 9},{5, 7}},
        {{4, 9},{5, 7}},
        {{5, 9},{5, 7}},
        {{6, 9},{5, 7}},
        {{7, 9},{5, 7}},
        {{2, 7},{3, 5}},
        {{3, 5},{2, 7}}
    ],
    [?assertEqual(true, overlap(#{
        first_section => #{from => FromF, to => ToF},
        second_section => #{from => FromS, to => ToS}}))
        ||{{FromF, ToF}, {FromS, ToS}} <- Cases].

do_not_overlap_sections_test() ->
    Cases = [
        {{2, 4},{5, 7}},
        {{8, 9},{5, 7}}
    ],
    [?assertEqual(false, overlap(#{
        first_section => #{from => FromF, to => ToF},
        second_section => #{from => FromS, to => ToS}}))
        ||{{FromF, ToF}, {FromS, ToS}} <- Cases].
