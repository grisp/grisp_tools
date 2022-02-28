-module(grisp_tools_package).

% API
-export([list/1]).

%--- API -----------------------------------------------------------------------

list(#{type := Type, platform := Platform}) ->
    parse(Type, list_bucket(Type, [<<"platforms/">>, atom_to_binary(Platform)])).

%--- Internal ------------------------------------------------------------------

list_bucket(Type, Root) ->
    Bucket = <<"grisp.s3.amazonaws.com">>,
    {ok, Client} = hackney:connect(hackney_ssl, Bucket, 443, [with_body]),
    Prefix = iolist_to_binary([Root, $/, atom_to_binary(Type), $/]),
    list_bucket(Client, Type, [<<"https://">>, Bucket], Prefix, undefined, []).

list_bucket(Client, Type, Bucket, Prefix, Token, Items) ->
    TokenQS = [{<<"continuation-token">>, Token} || Token =/= undefined],
    URL = hackney_url:make_url(<<>>, <<"">>, [
        {<<"list-type">>, <<"2">>},
        {<<"max-keys">>, <<"100">>},
        {<<"prefix">>, Prefix}
    ] ++ TokenQS),
    Body = request(Client, URL),
    XML = decode(Body),
    Contents = xpath(XML, "/ListBucketResult/Contents"),
    Files = files(simplify(Contents), Bucket, Client),
    case continue(XML) of
        eof ->
            hackney:close(Client),
            Files ++ Items;
        {continue, NewToken} ->
            list_bucket(Client, Type, Bucket, Prefix, NewToken, Files ++ Items)
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

files(Contents, Bucket, Client) ->
    Opts = #{bucket => Bucket, client => Client},
    lists:foldl(fun({'Contents', _, Content}, Files) ->
        try
            [file(Content, Opts)|Files]
        catch
            skip -> Files
        end
    end, [], Contents).

file(Content, Opts) ->
    lists:foldl(
        fun(C, F) -> file_attr(C, F, Opts) end,
        #{},
        Content
    ).

file_attr({'Key', _, [Key]}, File, #{bucket := Bucket, client := Client}) ->
    case lists:last(Key) of
        $/ ->
            throw(skip);
        _Other ->
            URL = iolist_to_binary([Bucket, $/, Key]),
            Extra = case string:slice(Key, max(0, length(Key) - 6)) of
                "LATEST" ->
                    #{latest => string:trim(request(Client, URL))};
                _ -> #{}
            end,
            maps:merge(
                File#{
                    name => list_to_binary(filename:basename(Key)),
                    url => URL
                },
                Extra
            )
    end;
file_attr({'LastModified', _, [Date]}, File, _Opts) ->
    File#{last_modified => calendar:rfc3339_to_system_time(Date)};
file_attr({'ETag', _, [ETag]}, File, _Opts) ->
    File#{etag => list_to_binary(ETag)};
file_attr({'Size', _, [SSize]}, File, _Opts) ->
    {Size, []} = string:to_integer(SSize),
    File#{size => Size};
file_attr(_, File, _Opts) ->
    File.

request(Client, URL) ->
    Request = {get, URL, [], <<>>},
    {ok, 200, _Headers, Body} = hackney:send_request(Client, Request),
    Body.

parse(otp, Files) -> parse_otp(Files);
parse(toolchain, Files) -> parse_toolchain(Files).

parse_otp(Files) ->
    lists:map(fun(#{name := Name} = File) ->
        [<<"grisp">>, <<"otp">>, <<"build">>, Vsn, Hash] = components(Name),
        File#{version => Vsn, hash => Hash}
    end, Files).

parse_toolchain(Files) ->
    {Parsed, Latest} = lists:foldl(fun parse_toolchain/2, {[], #{}}, Files),
    flag_latest(Parsed, Latest).

parse_toolchain(#{name := Name} = File, {Acc, Latest}) ->
    case File of
        #{latest := Filename} ->
            [_Prefix, OS|_] = components(Name),
            {Acc, Latest#{OS => Filename}};
        _Other ->
            [_Prefix, OS, OSVsn, Revision] = components(Name),
            NewFile = File#{
                os => OS,
                os_version => OSVsn,
                revision => Revision
            },
            {[NewFile|Acc], Latest}
    end.

flag_latest([], _Latest) ->
    [];
flag_latest(Toolchains, Latest) when map_size(Latest) == 0 ->
    Toolchains;
flag_latest([#{os := OS, name := Name} = Toolchain|Toolchains], Latest) ->
    case maps:find(OS, Latest) of
        {ok, Name} ->
            NewToolchain = Toolchain#{latest => true},
            [NewToolchain|flag_latest(Toolchains, maps:without([OS], Latest))];
        _Other ->
            [Toolchain|flag_latest(Toolchains, Latest)]
    end.

components(Package) ->
    Basename = filename:basename(Package, <<".tar.gz">>),
    string:split(Basename, <<"_">>, all).
