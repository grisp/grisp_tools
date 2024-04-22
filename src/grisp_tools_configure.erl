-module(grisp_tools_configure).

% API
-export([run/1]).
-export([settings/0]).

%--- Types ---------------------------------------------------------------------

% {Name, Token, Type, Default, Description}
-type setting() :: {string(), atom(), {boolean, boolean()} | {string, string()}, string()}.

%--- API -----------------------------------------------------------------------


run(State) ->
    Options = settings_options(),
    do_ask(State, Options).

do_ask(State, Options) ->
    lists:foldl(
        fun(Setting, Acc) ->
            ask(Setting, Acc)
        end,
        State,
        Options).

ask(_, #{flags := #{interactive := false}} = State) ->
    State;
ask({Prompt, Key, {Type, _}, _}, #{flags := Flags} = State) ->
    Default = maps:get(Key, Flags),
    Value = grisp_tools_io:ask(Prompt, Type, Default),
    State#{flags => Flags#{Key => Value}};
ask({Prompt, Key, {boolean, _}, _, OptsGen}, #{flags := Flags} = State) ->
    Default = maps:get(Key, Flags),
    case grisp_tools_io:ask(Prompt, boolean, Default) of
        true -> do_ask(State#{flags := Flags#{Key => true}}, OptsGen());
        _ -> State#{flags := Flags#{Key => false}}
    end.

-spec settings() -> [setting()].
settings() -> [
    {"Interactive", interactive, {boolean, true}, "Activates the interactive mode"}
    ] ++ format_settings_options(settings_options(), []).

-spec settings_options() -> [setting()].
settings_options() -> [
    {"App name", name, {string, "robot"}, "The name of your GRiSP application"},
    {"Erlang version", otp_version, {string, "25"}, "The·OTP·version·of·the·GRiSP·app"},
    {"Network configuration", network, {boolean, false}, "Network configuration files generation", fun network_options/0}
].

-spec network_options() -> [setting()].
network_options() -> [
    {"Wifi configuration", wifi, {boolean, false}, "Wifi configuration", fun wifi_options/0},
    {"GRiSP.io configuration", grisp_io, {boolean, false}, "GRiSP.io configuration", fun grisp_io_options/0},
    {"empd", epmd, {boolean, false}, "Distributed Erlang configuration generation"}
].

wifi_options() -> [
    {"Wifi SSID", ssid, {string, "My Wifi"}, "The SSID of your Wifi"},
    {"Wifi PSK", psk, {string, "..."}, "The PSK of your Wifi"}
].


-spec grisp_io_options() -> [setting()].
grisp_io_options() -> [
    {"GRiSP.io token", token, {string, "..."}, "Your private GRiSP.io token"}
].

format_settings_options([], Acc) ->
    Acc;
format_settings_options([{_, _, _, _} = Opt | Tail], Acc) ->
    format_settings_options(Tail, [Opt | Acc]);
format_settings_options([{Prompt, Key, Type, Descr, FollowUp} | Tail], Acc) ->
    format_settings_options(Tail ++ FollowUp(), [{Prompt, Key, Type, Descr} | Acc]).
