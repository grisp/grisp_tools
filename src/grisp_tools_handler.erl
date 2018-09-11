-module(grisp_tools_handler).

% API
-export([init/2]).
-export([run/3]).
-export([finalize/2]).

%--- API -----------------------------------------------------------------------

init(Handler, State) -> {Handler, State}.

run(Handler, Args, Handlers) ->
    {Fun, State} = maps:get(Handler, Handlers),
    {Result, NewState} = apply(Fun, Args ++ [State]),
    {Result, maps:put(Handler, {Fun, NewState}, Handlers)}.

finalize(_Handler, State) -> State.
