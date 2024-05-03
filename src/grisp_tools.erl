-module(grisp_tools).

% API
-export([handlers_init/1]).
-export([handlers_finalize/1]).
-export([build/1]).
-export([deploy/1]).
-export([list_packages/1]).
-export([report/1]).
-export([configure/1]).

%--- API -----------------------------------------------------------------------

handlers_init(Handlers) ->
    maps:map(fun(_Name, {Handler, State}) ->
        grisp_tools_handler:init(Handler, State)
    end, Handlers).

handlers_finalize(#{handlers := Handlers}) ->
    maps:map(fun(_Name, {Handler, State}) ->
        grisp_tools_handler:finalize(Handler, State)
    end, Handlers).

build(Configuration) -> grisp_tools_build:run(Configuration).

deploy(State) -> grisp_tools_deploy:run(State).

list_packages(Opts) -> grisp_tools_package:list(Opts).

report(Opts) -> grisp_tools_report:run(Opts).

configure(Opts) -> grisp_tools_configure:run(Opts).
