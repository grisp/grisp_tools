-module(grisp_tools_configure).

% API
-export([run/1]).
-export([settings/0]).

%--- Types ---------------------------------------------------------------------

% {Name, {Long, Short}, {Type, Default}, Description}
-type settings() :: {string(),
                     {string(), char()},
                     {string, string()} | {boolean, boolean()},
                     string()}.

-type settings_options() :: {string(),
                             {string(), char()},
                             {string, string()} | {boolean, boolean()},
                             string(),
                             function()} | settings().

%--- API -----------------------------------------------------------------------

run(State) ->
    Options = settings_options(),
    do_ask(State, Options).

do_ask(State, Options) ->
    lists:foldl(
        fun(Setting, AccState) ->
            AccState1 = ask(AccState, Setting),
            {Key, _} = element(2, Setting),
            case validate_user_choice(AccState1, Key) of
                {ok, AccState2} -> AccState2;
                {error, _} = E -> grisp_tools_util:event(AccState1, E)
            end
        end,
        State,
        Options).

ask(#{flags := #{interactive := false}} = State, _) ->
    State; % Skipping ask in non-interactive mode
ask(#{user_opts := UserOpts} = State, {_, {Key, _}, _, _})
  when is_map_key(Key, UserOpts) ->
    user_provided_event(State, Key, UserOpts),
    State; % Skipping if user provided the option in the command args
ask(#{user_opts := UOpts, flags := Flags} = State, {_, {Key, _}, _, _, OptsGen})
  when is_map_key(Key, UOpts) ->
    user_provided_event(State, Key, UOpts),
    case maps:get(Key, UOpts) of
        true -> do_ask(State#{flags := Flags#{Key => true}}, OptsGen());
        _ -> State
    end;
ask(#{flags := Flags} = State, {Prompt, {Key, _}, {Type, _}, _}) ->
    Default = maps:get(Key, Flags),
    Value = grisp_tools_io:ask(State, Prompt, Type, Default),
    State#{flags => Flags#{Key => Value}};
ask(#{flags := Flags} = State, {Prompt, {Key, _}, {boolean, _}, _, OptsGen}) ->
    Default = maps:get(Key, Flags),
    case grisp_tools_io:ask(State, Prompt, boolean, Default) of
        true -> do_ask(State#{flags := Flags#{Key => true}}, OptsGen());
        _ -> State#{flags := Flags#{Key => false}}
    end.

user_provided_event(State, {Key, _}, UserOpts) ->
    Value = maps:get(Key, UserOpts),
    Prompt = io_lib:format("Value ~p provided for ~p. Skipping question",
                           [Value, Key]),
    grisp_tools_util:event(State, {info, Prompt}).

-spec validate_user_choice(State, Setting) -> {ok, State} | {error, Error} when
      State   :: map(),
      Setting :: atom(),
      Error   :: atom().
validate_user_choice(State, name) ->
    {ok, Cwd} = file:get_cwd(),
    #{flags := #{name := ProjectName, interactive := Interactive}} = State,
    ProjectPath = filename:join(Cwd, ProjectName),
    case {Interactive, filelib:is_dir(ProjectPath)} of
        {true, true} ->
            Prompt = io_lib:format(
                       "A directory with the name ~p already exists. Do you wish to proceed ?",
                       [ProjectName]),
            UserChoice = grisp_tools_io:ask(State, Prompt, boolean, false),
            case UserChoice of
                true -> {ok, State#{project_exists => true}};
                false -> {error, "Project name already taken"}
            end;
        {false, true} -> {error, "Project name already taken"};
        {_, false} -> {ok, State#{project_exists => false}}
    end;
validate_user_choice(State, _Param) ->
    {ok, State}.

-spec settings() -> [settings()].
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

-spec settings_options() -> [settings_options()].
settings_options() -> [
    {"App name", {name, undefined}, {string, "robot"},
     "The name of the OTP application"},
    {"Erlang version", {otp_version, $o}, {string, "25"},
     "The OTP version of the GRiSP app"},
    {"SD Card Path", {dest, $d}, {string, "/path/to/SD-card"},
     "The path to the SD card where you want to deploy the GRISP app"},
    {"Use Network ?", {network, $n}, {boolean, false},
     "Network configuration files generation", fun network_options/0}
].

-spec network_options() -> [settings_options()].
network_options() -> [
    {"Use Wi-Fi ?", {wifi, $w}, {boolean, false},
     "Wifi configuration", fun wifi_options/0},
    {"Enable GRiSP.io integration ?", {grisp_io, $g}, {boolean, false},
     "GRiSP.io configuration", fun grisp_io_options/0},
    {"Enable Distributed Erlang ?", {epmd, $e}, {boolean, false},
     "Distributed Erlang configuration generation", fun epmd_options/0}
].

-spec wifi_options() -> [settings_options()].
wifi_options() -> [
    {"Wi-Fi SSID", {ssid, $p}, {string, "My Wifi"}, "The SSID of your Wi-Fi"},
    {"Wi-Fi Password", {psk, $p}, {string, "..."}, "The PSK of your Wi-Fi"}
].

-spec grisp_io_options() -> [settings_options()].
grisp_io_options() -> [
    {"Do you need to link your GRiSP-2 board ?", {grisp_io_linking, $l},
     {boolean, false}, "GRiSP.io device linking", fun grisp_io_link_options/0}
].

-spec grisp_io_link_options() -> [settings_options()].
grisp_io_link_options() -> [
    {"Please enter your personal device linking token", {token, $t},
     {string, "..."}, "Your private GRiSP.io token"}
].

-spec epmd_options() -> [settings_options()].
epmd_options() -> [
    {"Erlang Cookie", {cookie, $c}, {string, "grisp"},
     "The distributed Erlang cookie"}
].

-spec format_settings_options([settings_options()], [settings()]) -> settings().
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
