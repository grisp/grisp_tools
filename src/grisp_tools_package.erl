-module(grisp_tools_package).

% API
-export([list/1]).

%--- API -----------------------------------------------------------------------

list(#{type := otp, platform := Platform}) ->
    list_bucket(iolist_to_binary([[E, <<"/">>] || E <- [
        <<"platforms">>,
        atom_to_binary(Platform),
        <<"otp">>
    ]])).

%--- Internal ------------------------------------------------------------------

list_bucket(Prefix) ->
    Bucket = <<"grisp.s3.amazonaws.com">>,
    {ok, Client} = hackney:connect(hackney_ssl, Bucket, 443, [with_body]),
    list_bucket(Client, Prefix, undefined, []).

list_bucket(Client, Prefix, Token, Items) ->
    TokenQS = [{<<"continuation-token">>, Token} || Token =/= undefined],
    URL = hackney_url:make_url(<<>>, <<"">>, [
        {<<"list-type">>, <<"2">>},
        {<<"max-keys">>, <<"100">>},
        {<<"prefix">>, Prefix}
    ] ++ TokenQS),
    Request = {get, URL, [], <<>>},
    {ok, 200, _Headers, Body} = hackney:send_request(Client, Request),
    XML = decode(Body),
    Contents = xpath(XML, "/ListBucketResult/Contents/Key/child::text()"),
    Packages = simplify(Contents),
    case continue(XML) of
        eof ->
            hackney:close(Client),
            lists:sort(fun version_sort/2, [
                clean(Prefix, P)
                ||
                P <- Packages ++ Items, P =/= binary_to_list(Prefix)
            ]);
        {continue, NewToken} ->
            list_bucket(Client, Prefix, NewToken, Packages ++ Items)
    end.

continue(Response) ->
    Path = "/ListBucketResult/NextContinuationToken/child::text()",
    case simplify(xpath(Response, Path)) of
        [] -> eof;
        [Token] -> {continue, Token}
    end.

decode(Bin) ->
    {XML, _Rest} = xmerl_scan:string(binary_to_list(Bin), [{space, normalize}]),
    [Clean] = xmerl_lib:remove_whitespace([XML]),
    Clean.

simplify(Elements) when is_list(Elements) -> [simplify(E) || E <- Elements];
simplify(XML) -> xmerl_lib:simplify_element(XML).

xpath(Element, Path) -> xmerl_xpath:string(Path, Element).

clean(Prefix, Path) ->
    "grisp_otp_build_" ++ VsnHash = string:prefix(Path, Prefix),
    [Vsn, HashExt] = string:split(VsnHash, <<"_">>),
    Hash = filename:basename(HashExt, <<".tar.gz">>),
    #{version => Vsn, hash => Hash}.

version_sort(#{version := Vsn, hash := H1}, #{version := Vsn, hash := H2}) ->
    H1 =< H2;
version_sort(#{version := V1}, #{version := V2}) ->
    V1 =< V2.
