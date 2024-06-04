-module(grisp_tools_configure).

% API
-export([run/1]).
-export([settings/0]).

%--- Record --------------------------------------------------------------------
-record(set_opts, {prompt            :: string(),
                   long              :: atom(),
                   short = undefined :: char() | undefined,
                   type              :: setting_type(),
                   default = none    :: none | string() | boolean(),
                   description       :: string(),
                   dep_setting_fun   :: function(),
                   hint  = ""        :: string()}).

%--- Types ---------------------------------------------------------------------
-type setting_type() :: string | trim_string | boolean | latin1.

% @doc {Long, Short, Type, Default, Descr}
-type setting() :: {atom(),
                    char() | undefined,
                    string(),
                    {setting_type(), string() | boolean()},
                    string()}.

-type set_opts() :: #set_opts{}.
%--- API -----------------------------------------------------------------------

run(State) ->
    Options = settings_options(),
    do_ask(State, Options).

do_ask(State, Options) ->
    lists:foldl(
        fun(Setting, AccState) ->
            Key = Setting#set_opts.long,
            AccState1 = ask(AccState, Setting),
            case validate_user_choice(AccState1, Key) of
                {ok, AccState2} -> AccState2;
                {error, _} = E -> grisp_tools_util:event(AccState1, E)
            end
        end,
        State,
        Options).

ask(#{flags := #{interactive := false}} = State, _) ->
    State; % Skipping ask in non-interactive mode
ask(#{user_opts := UOpts} = State, #set_opts{long = Key, dep_setting_fun = Fun})
  when is_map_key(Key, UOpts) andalso Fun == undefined ->
    user_provided_event(State, Key, UOpts),
    State; % Skipping if user provided the option in the command args
ask(#{user_opts := UOpts, flags := Flags}=State, #set_opts{long=Key} = Setting)
  when is_map_key(Key, UOpts) ->
    OptsGen = Setting#set_opts.dep_setting_fun,
    user_provided_event(State, Key, UOpts),
    case maps:get(Key, UOpts) of
        true -> do_ask(State#{flags := Flags#{Key => true}}, OptsGen());
        _ -> State
    end;
ask(#{flags := Flags} = State, #set_opts{dep_setting_fun = undefined} = Set) ->
    Prompt = Set#set_opts.prompt,
    Key = Set#set_opts.long,
    Type = Set#set_opts.type,
    Hint = Set#set_opts.hint,
    Default = maps:get(Key, Flags),
    Value = grisp_tools_io:ask(State, Prompt, Type, Default, Hint),
    State#{flags => Flags#{Key => Value}};
ask(#{flags := Flags} = State, #set_opts{type = boolean} = Setting) ->
    Prompt = Setting#set_opts.prompt,
    Key = Setting#set_opts.long,
    OptsGen = Setting#set_opts.dep_setting_fun,
    Hint = Setting#set_opts.hint,
    Default = maps:get(Key, Flags),
    case grisp_tools_io:ask(State, Prompt, boolean, Default, Hint) of
        true -> do_ask(State#{flags := Flags#{Key => true}}, OptsGen());
        _ -> State#{flags := Flags#{Key => false}}
    end.

user_provided_event(State, Key, UserOpts) ->
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
                       "A directory with the name ~p already exists. "
                       ++ "Do you wish to proceed? ",
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

% @doc The parameters defined directly inside this function won't be ask in
%      the interactive part of the CLI
-spec settings() -> [setting()].
settings() ->
    {{Year, _, _}, _} = calendar:universal_time(),
    {AuthorName, AuthorEmail} = default_author_and_email(),
    [{interactive, $i, {boolean, true}, "Activates the interactive mode"},
     {desc, undefined, {string, "A GRiSP application"},
      "Short description of the app"},
     {copyright_year, undefined, {string, integer_to_list(Year)},
      "The copyright year"},
     {author_name, undefined, {string, AuthorName}, "The name of the author"},
     {author_email, undefined, {string, AuthorEmail}, "The emal of the author"}
    ] ++ format_settings_options(settings_options(), []).

% @doc the settings given in this function and in all the extension functions
%      of boolean settings will be used during the interactive part of the CLI
-spec settings_options() -> [set_opts()].
settings_options() ->
    [#set_opts{prompt = "App name", long = name, type = latin1, default="robot",
              description = "The name of the OTP application",
              hint = "Enter the name of your application"},
     #set_opts{prompt = "Erlang version", long = otp_version, short = $o,
              type = string, default = "25",
              description = "The OTP version of the GRiSP app",
              hint = "Specify the Erlang version to use"},
     #set_opts{prompt = "SD Card path", long = dest, short = $d, type = string,
              default = "/path/to/SD-card",
              description = "The path to the SD card where you want to deploy "
                            ++ "the GRiSP app",
              hint = "Define the path to the SD card where the application "
                     ++ "will be deployed."},
     #set_opts{prompt = "Use network ?", long = network, short=$n, type=boolean,
              default = false, dep_setting_fun = fun network_options/0,
              description = "Network configuration files generation"}].

-spec network_options() -> [set_opts()].
network_options() -> [
    #set_opts{prompt = "Use Wi-Fi ?", long = wifi, short = $w, type = boolean,
             default = false, description = "Wi-Fi configuration",
             dep_setting_fun = fun wifi_options/0,
             hint = "If you want to use ethernet press 'n'"},
    #set_opts{prompt = "Enable GRiSP.io integration ?", long = grisp_io,
             short = $g, type = boolean, default = false,
             description = "GRiSP.io configuration",
             dep_setting_fun = fun grisp_io_options/0},
    #set_opts{prompt = "Enable Distributed Erlang ?", long = epmd, short = $e,
             type = boolean, default = false,
             description = "Distributed Erlang configuraiton generation",
             hint = "If you want to use the remote shell press 'y'",
             dep_setting_fun = fun epmd_options/0}].

-spec wifi_options() -> [set_opts()].
wifi_options() -> [
    #set_opts{prompt = "Wi-Fi SSID", long = ssid, short = $p, type = string,
             description = "The SSID of your Wi-Fi", hint = "Your Wi-Fi name"},
    #set_opts{prompt = "Wi-Fi password", long = psk, short = $p, type = string,
             description = "The PSK of your Wi-Fi", hint = "Your password"}].

-spec grisp_io_options() -> [set_opts()].
grisp_io_options() -> [
    #set_opts{prompt =  "Do you need to link your GRiSP-2 board ?", short = $l,
             long = grisp_io_linking, type = boolean, default = false,
             description = "GRiSP.io device linking",
             dep_setting_fun = fun grisp_io_link_options/0}].

-spec grisp_io_link_options() -> [set_opts()].
grisp_io_link_options() -> [
    #set_opts{prompt = "Please enter your personal device linking token",
             long = token, short = $t, type = trim_string,
             description = "Your private GRiSP.io token",
             hint = "# Write the token without any <, > or "}].

-spec epmd_options() -> [set_opts()].
epmd_options() -> [
    #set_opts{prompt = "Erlang Cookie", long = cookie, short = $c, type=string,
             default = "grisp", description = "The distributed Erlang cookie",
             hint = "Cookie is necessary for remote shell."}].

-spec format_settings_options([set_opts()], [setting()]) -> setting().
format_settings_options([], Acc) ->
    Acc;
format_settings_options([#set_opts{dep_setting_fun = undefined} = O |T], Acc) ->
    Long = O#set_opts.long,
    Short = O#set_opts.short,
    Type = O#set_opts.type,
    Default = O#set_opts.default,
    Descr = O#set_opts.description,
    format_settings_options(T, [{Long, Short, {Type, Default}, Descr} | Acc]);
format_settings_options([SetOpts | Tail], Acc) ->
    Long = SetOpts#set_opts.long,
    Short = SetOpts#set_opts.short,
    Type = SetOpts#set_opts.type,
    Descr = SetOpts#set_opts.description,
    Default = SetOpts#set_opts.default,
    FollowUp = SetOpts#set_opts.dep_setting_fun,
    format_settings_options(Tail ++ FollowUp(),
                            [{Long, Short, {Type, Default}, Descr} | Acc]).

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
