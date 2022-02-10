-module(grisp_tools_template).

% API
-export([render/2]).

%--- API -----------------------------------------------------------------------

render(File, Context) ->
    Parsed = bbmustache:parse_file(File),
    Options = [{key_type, atom}, raise_on_context_miss],
    try
        bbmustache:compile(Parsed, default(Context), Options)
    catch
        error:{context_missing, {key, Key}} ->
            error({template_error, File, {missing_key, Key}});
        error:{context_missing, {file_not_found, Include}} ->
            error({template_error, File, {include_not_found, Include}})
    end.

%--- Internal ------------------------------------------------------------------

default(Context) -> maps:merge(env(), Context).

env() -> #{env => maps:from_list([parse_env(E) || E <- os:getenv()])}.

parse_env(E) ->
    % TODO: Use os:env() once we only support 24+
    {match, [Name, Value]} = re:run(E, "([^=]+)=(.*)", [
        {capture, all_but_first, binary},
        unicode
    ]),
    {binary_to_atom(Name, utf8), Value}.
