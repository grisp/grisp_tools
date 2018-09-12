-module(grisp_tools_handler).

% API
-export([init/2]).
-export([run/3]).
-export([finalize/2]).

%--- API -----------------------------------------------------------------------

init(Handler, State) -> {Handler, State}.

run(Handler, Args, Handlers) ->
    {Fun, State} = maps:get(Handler, Handlers),
    {Result, NewState} = case apply(Fun, Args ++ [State]) of
        {R, S} -> {R, S};
        Other  -> error({invalid_handler_return, Handler, Other})
    end,
    {Result, maps:put(Handler, {Fun, NewState}, Handlers)}.

finalize(_Handler, State) -> State.
