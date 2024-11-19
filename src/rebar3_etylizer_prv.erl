-module(rebar3_etylizer_prv).

-export([init/1, do/1, format_error/1]).

-include_lib("etylizer/src/ety_main.hrl").

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
    Opts = #opts{defines = Defines,
                 includes = IncludePaths,
                 src_paths = SrcPaths,
                 project_root = ProjectRoot},
    try ety_main:doWork(Opts), {ok, State}
    catch
        error:Reason ->
            rebar_log:log(error, "etylizer: ~ts", [Reason]),
            {error, "etylizer failure dectected"}
    end.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
