-module(grisp_tools_configure).

% API
-export([run/1]).

%--- API -----------------------------------------------------------------------


run(State) ->
    List = settings(),
    lists:foldl(
        fun(Setting, Acc) ->
            ask(Setting, Acc)
        end,
        State,
        List).


ask(_, #{flags := #{yes := true}} = State) ->
    State;
ask({Prompt, Key, Type}, #{flags := Flags} = State) ->
    Default = maps:get(Key, Flags),
    Value = grisp_tools_io:ask(Prompt, Type, Default),
    State#{flags => Flags#{Key => Value}}.

settings() -> [
    {"App name", name, string},
    {"Erlang version", otp_version, string}
].
