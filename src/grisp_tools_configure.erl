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
    State; % Skipping ask in non-interactive mode
ask({_, {Key, _}, _, _}, #{user_opts := UserOpts} = State)
  when is_map_key(Key, UserOpts) ->
    user_provided_event(State, Key, UserOpts),
    State; % Skipping if user provided the option in the command args
ask({_, {Key, _}, _, _, OptsGen}, #{user_opts := UOpts, flags := Flags} = State)
  when is_map_key(Key, UOpts) ->
    user_provided_event(State, Key, UOpts),
    case maps:get(Key, UOpts) of
        true -> do_ask(State#{flags := Flags#{Key => true}}, OptsGen());
        _ -> State
    end;
ask({Prompt, {Key, _}, {Type, _}, _}, #{flags := Flags} = State) ->
    Default = maps:get(Key, Flags),
    Value = grisp_tools_io:ask(Prompt, Type, Default),
    State#{flags => Flags#{Key => Value}};
ask({Prompt, {Key, _}, {boolean, _}, _, OptsGen}, #{flags := Flags} = State) ->
    Default = maps:get(Key, Flags),
    case grisp_tools_io:ask(Prompt, boolean, Default) of
        true -> do_ask(State#{flags := Flags#{Key => true}}, OptsGen());
        _ -> State#{flags := Flags#{Key => false}}
    end.

user_provided_event(State, {Key, _}, UserOpts) ->
    Value = maps:get(Key, UserOpts),
    Prompt = io_lib:format("Value ~p provided for ~p. Skipping question",
                           [Value, Key]),
    grisp_tools_util:event(State, {info, Prompt}).

settings() ->
    {{Year, _, _}, _} = calendar:universal_time(),
    {AuthorName, AuthorEmail} = default_author_and_email(),
    [{"Interactive", {interactive, $i}, {boolean, true},
      "Activates the interactive mode"},
     {"Description", {desc, undefined}, {string, "A GRiSP application"},
      "Short description of the app"},
     {"Copyright year", {copyright_year, undefined}, {string, Year},
      "The copyright year"},
     {"Author name", {author_name, undefined}, {string, AuthorName},
      "The name of the author"},
     {"Author email", {author_email, undefined}, {string, AuthorEmail},
      "The email of the author"}
    ] ++ format_settings_options(settings_options(), []).

settings_options() -> [
    {"App name", {name, undefined}, {string, "robot"},
     "The name of your GRiSP application"},
    {"Erlang version", {otp_version, $o}, {string, "25"},
     "The·OTP·version·of·the·GRiSP·app"},
    {"Destination", {dest, $d}, {string, "/path/to/SD-card"},
     "The path to the SD card where you want to deploy the GRISP app"},
    {"Network configuration", {network, $n}, {boolean, false},
     "Network configuration files generation", fun network_options/0}
].

network_options() -> [
    {"Wifi configuration", {wifi, $w}, {boolean, false},
     "Wifi configuration", fun wifi_options/0},
    {"GRiSP.io configuration", {grisp_io, $g}, {boolean, false},
     "GRiSP.io configuration", fun grisp_io_options/0},
    {"empd", {epmd, $e}, {boolean, false},
     "Distributed Erlang configuration generation", fun epmd_options/0}
].

wifi_options() -> [
    {"Wifi SSID", {ssid, $p}, {string, "My Wifi"}, "The SSID of your Wifi"},
    {"Wifi PSK", {psk, $p}, {string, "..."}, "The PSK of your Wifi"}
].

grisp_io_options() -> [
    {"GRiSP.io token", {token, $t}, {string, "..."},
     "Your private GRiSP.io token"}
].

epmd_options() -> [
    {"Erlang cookie", {cookie, $c}, {string, "grisp"},
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
