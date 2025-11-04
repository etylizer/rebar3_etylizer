-module(rebar3_etylizer_prv).

-export([init/1, do/1, format_error/1]).

-include_lib("etylizer/src/etylizer_main.hrl").

-define(PROVIDER, etylizer).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 etylizer"}, % How to use the plugin
            % list of options understood by the plugin
            {opts, [project_root, src_path]},
            {short_desc, "etylizer plugin"},
            {desc, "A rebar plugin to analyze a codebase using etylizer"}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Config = case dict:find(etylizer, rebar_state:opts(State)) of
                 error -> [];
                 {ok, C} -> C
             end,
    Defines = proplists:get_value(defines, Config, []),
    IncludePaths = proplists:get_value(include, Config, ["include/"]),
    SrcPaths = proplists:get_value(src_paths, Config, ["src/"]),
    ProjectRoot = proplists:get_value(project_root, Config, "."),
    
    % Get all .erl source files from source paths
    SourceFiles = lists:flatmap(fun(SrcPath) ->
        filelib:wildcard(filename:join([SrcPath, "*.erl"]))
    end, SrcPaths),
    
    rebar_log:log(info, "Found ~p source files to analyze", [length(SourceFiles)]),
    
    % Analyze each source file individually
    analyze_files(SourceFiles, Defines, IncludePaths, SrcPaths, ProjectRoot),
    
    {ok, State}.

-spec analyze_files([string()], list(), [string()], [string()], string()) -> ok.
analyze_files([], _, _, _, _) ->
    ok;
analyze_files([File | Rest], Defines, IncludePaths, SrcPaths, ProjectRoot) ->
    rebar_log:log(info, "Analyzing source file: ~s", [File]),
    
    try
        % Create opts for individual file analysis with report mode
        Opts = #opts{
            defines = Defines,
            includes = IncludePaths,
            src_paths = SrcPaths,
            project_root = ProjectRoot,
            files = [File],
            % Enable report mode - you might need to check the actual option name in etylizer
            % If report mode is controlled differently, adjust accordingly
            dump = true,  % or whatever option enables reporting
            mode = test_mode  % or use whatever mode enables individual file reporting
        },
        
        etylizer_main:main(Opts)
    catch
        error:Reason:St ->
            rebar_log:log(error, "Failed to analyze ~s: ~p", [File, Reason]),
            rebar_log:log(debug, "Stack trace: ~p", [St])
    end,
    
    analyze_files(Rest, Defines, IncludePaths, SrcPaths, ProjectRoot).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
