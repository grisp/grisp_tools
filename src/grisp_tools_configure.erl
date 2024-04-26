-module(grisp_tools_configure).

% API
-export([run/1]).
-export([settings/0]).

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

settings() ->
    {{Year, _, _}, _} = calendar:universal_time(),
    {AuthorName, AuthorEmail} = default_author_and_email(),
    [{"Interactive", interactive, {boolean, true},
      "Activates the interactive mode"},
     {"Description", desc, {string, "A GRiSP application"},
      "Short description of the app"},
     {"Copyright year", copyright_year, {string, Year},
      "The copyright year"},
     {"Author name", author_name, {string, AuthorName},
      "The name of the author"},
     {"Author email", author_email, {string, AuthorEmail},
      "The email of the author"}
    ] ++ format_settings_options(settings_options(), []).

settings_options() -> [
    {"App name", name, {string, "robot"},
     "The name of your GRiSP application"},
    {"Erlang version", otp_version, {string, "25"},
     "The·OTP·version·of·the·GRiSP·app"},
    {"Destination", dest, {string, "/path/to/SD-card"},
     "The path to the SD card where you want to deploy the GRISP app"},
    {"Network configuration", network, {boolean, false},
     "Network configuration files generation", fun network_options/0}
].

network_options() -> [
    {"Wifi configuration", wifi, {boolean, false},
     "Wifi configuration", fun wifi_options/0},
    {"GRiSP.io configuration", grisp_io, {boolean, false},
     "GRiSP.io configuration", fun grisp_io_options/0},
    {"empd", epmd, {boolean, false},
     "Distributed Erlang configuration generation", fun epmd_options/0}
].

wifi_options() -> [
    {"Wifi SSID", ssid, {string, "My Wifi"}, "The SSID of your Wifi"},
    {"Wifi PSK", psk, {string, "..."}, "The PSK of your Wifi"}
].

grisp_io_options() -> [
    {"GRiSP.io token", token, {string, "..."}, "Your private GRiSP.io token"}
].

epmd_options() -> [
    {"Erlang cookie", cookie, {string, "grisp"},
     "The distributed Erlang cookie"}
].

format_settings_options([], Acc) ->
    Acc;
format_settings_options([{_, _, _, _} = Opt | Tail], Acc) ->
    format_settings_options(Tail, [Opt | Acc]);
format_settings_options([{Prompt, Key, Type, Descr, FollowUp} | Tail], Acc) ->
    format_settings_options(Tail ++ FollowUp(),
                            [{Prompt, Key, Type, Descr} | Acc]).

default_author_and_email() ->
    %% See if we can get a git user and email to use as defaults
    case rebar_utils:sh("git config --global user.name", [return_on_error]) of
        {ok, Name} ->
            case rebar_utils:sh("git config --global user.email",
                                [return_on_error]) of
                {ok, Email} ->
                    {rebar_string:trim(Name, both, "\n"),
                     rebar_string:trim(Email, both, "\n")};
                {error, _} ->
                    %% Use neither if one doesn't exist
                    {"Anonymous", "anonymous@example.org"}
            end;
        {error, _} ->
            %% Ok, try mecurial
            case rebar_utils:sh("hg showconfig ui.username",
                                [return_on_error]) of
                {ok, NameEmail} ->
                    case re:run(NameEmail, "^(.*) <(.*)>$",
                                [{capture, [1, 2], list}, unicode]) of
                        {match, [Name, Email]} ->
                            {Name, Email};
                        _ ->
                            {"Anonymous", "anonymous@example.org"}
                    end;
                {error, _} ->
                    {"Anonymous", "anonymous@example.org"}
            end
    end.
