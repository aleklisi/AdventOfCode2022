-module(day7_puzzle2).

%% API exports
-export([main/1]).

% The total disk space available to the filesystem is 70000000.
-define(TOTAL_DISK_SPACE, 70_000_000).
% To run the update, you need unused space of at least 30000000.
-define(UPDATE_NEEDED_DISK_SPACE, 30_000_000).

%%====================================================================
%% API functions
%%====================================================================

main(Args) ->
    {ok, RawBinaryInput} = file:read_file(hd(Args)),
    ShellHistory = parse_shell_history(RawBinaryInput),
    FilesAndDrisList = list_files_and_dirs(ShellHistory),
    DirsWithFilesTheyInclude = find_files_in_dirs(FilesAndDrisList),
    FoldersWithSizes = get_folders_sizes(DirsWithFilesTheyInclude),
    Answer = find_smallest_folder_to_be_deleted(FoldersWithSizes),
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
    {_, FilesAndDirs} = lists:foldl(fun execute_command/2, {[], [{dir, ["/"]}]}, ShellHistory),
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

find_smallest_folder_to_be_deleted(FoldersWithSizes) ->
    MemoryOccupied = proplists:get_value(["/"], FoldersWithSizes),
    FreeMemory = (?TOTAL_DISK_SPACE - MemoryOccupied),
    MemoryAmountToBeFreed = ?UPDATE_NEEDED_DISK_SPACE - FreeMemory,
    lists:min([DirSize || {_DirName, DirSize} <- FoldersWithSizes, DirSize > MemoryAmountToBeFreed]).
