-module(day7_puzzle1).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    ShellHistory = parse_shell_history(RawBinaryInput),
    FilesAndDrisList = list_files_and_dirs(ShellHistory),
    DirsWithFilesTheyInclude = find_files_in_dirs(FilesAndDrisList),
    FoldersWithSizes = get_folders_sizes(DirsWithFilesTheyInclude),
    % To begin, find all of the directories with a total size of at most 100000,
    % then calculate the sum of their total sizes.
    Answer = lists:sum([Size || {_, Size} <- FoldersWithSizes, Size < 100000]),
    io:format("Answer: ~p~n", [Answer]),
    erlang:halt(0).

%%====================================================================
%% Internal functions
%%====================================================================

parse_shell_history(RawBinaryInput) ->
    RawLstInput = binary:bin_to_list(RawBinaryInput),
    RawCommandsList = string:split(RawLstInput, "\n$ ", all),
    lists:map(fun parse_command/1, RawCommandsList).

parse_command("$ cd /") -> {cd, "/"};
parse_command([$c, $d, 32 | DirName]) -> {cd, DirName};
parse_command([$l, $s, 10 | RawContent]) ->
    Content = lists:map(
        fun(Element) ->
            [First, Second] = string:split(Element, " ", all),
            case First of
                "dir" -> {dir, Second};
                SizeRaw ->
                    {Size, ""} = string:to_integer(SizeRaw),
                    {file, Second, Size}
            end
        end, string:split(RawContent, "\n", all)),
    {ls, Content}.

list_files_and_dirs(ShellHistory) ->

    {_, FilesAndDirs} = lists:foldl(fun execute_command/2, {[], []}, ShellHistory),
    lists:map(
        fun({dir, Path}) -> {dir, lists:reverse(Path)};
           ({file, Path, Size}) -> {file, lists:reverse(Path), Size}
        end, FilesAndDirs).

execute_command({cd, ".."}, {[_BottomDir | PathRest], FilesAndDirs}) ->
    {PathRest, FilesAndDirs};
execute_command({cd, Name}, {CurrentPath, FilesAndDirs}) ->
    {[Name | CurrentPath], FilesAndDirs};
execute_command({ls, CurrFilesAndDirs}, {CurrentPath, FilesAndDirs}) ->
    NewFilesAndDirs = lists:map(
        fun({dir, Name}) -> {dir, [Name | CurrentPath]};
           ({file, Name, Size}) -> {file, [Name | CurrentPath], Size}
        end, CurrFilesAndDirs),
    {CurrentPath, NewFilesAndDirs ++ FilesAndDirs}.

find_files_in_dirs(FilesAndDrisList) ->
    DirsLst = lists:filter(fun({dir, _Name}) -> true; (_) -> false end, FilesAndDrisList),
    lists:map(fun({dir, DirPath}) ->
        {DirPath,
            lists:filter(fun({dir, _}) -> false;
                            ({file, FilePath, _Size}) ->
                contains(DirPath, FilePath) 
            end, FilesAndDrisList)}
    end, DirsLst).

contains([], _) -> true;
contains([Elem | DirRest], [Elem | FileRest]) ->
    contains(DirRest, FileRest);
contains(_, _) -> false.

get_folders_sizes(DirsWithFilesTheyInclude) ->
    lists:map(fun get_folder_size/1, DirsWithFilesTheyInclude).

get_folder_size({FolderName, Files}) ->
    {FolderName, lists:sum([Size || {file, _Name, Size} <- Files])}.
