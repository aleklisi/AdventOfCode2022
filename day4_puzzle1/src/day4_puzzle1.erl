-module(day4_puzzle1).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    ParsedSections = parse_data(binary:bin_to_list(RawBinaryInput)),
    SectionsContainingOneAnother = lists:filter(fun contains/1, ParsedSections),
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

% In how many assignment pairs does one range fully contain the other?
contains(#{first_section := #{from := FromF, to := ToF},
         second_section := #{from := FromS, to := ToS}}) ->
        (FromF =< FromS andalso ToS =<ToF)
        orelse
        (FromS =< FromF andalso ToF =< ToS).