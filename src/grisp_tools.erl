-module(grisp_tools).

% API
-export([handlers_init/1]).
-export([handlers_finalize/1]).
-export([deploy/1]).

%--- API -----------------------------------------------------------------------

handlers_init(Handlers) ->
    maps:map(fun(_Name, {Handler, State}) ->
        grisp_tools_handler:init(Handler, State)
    end, Handlers).

handlers_finalize(#{handlers := Handlers}) ->
    maps:map(fun(_Name, {Handler, State}) ->
        grisp_tools_handler:finalize(Handler, State)
    end, Handlers).

deploy(State) -> grisp_tools_deploy:run(State).
