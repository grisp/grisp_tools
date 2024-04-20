-module(grisp_tools_io).

-export([
         ask/2,
         ask/3
        ]).

ask(Prompt, Type) ->
    ask_convert(Prompt, fun get/2, Type, none).

ask(Prompt, Type, Default)  ->
    ask_convert(Prompt, fun get/2, Type, Default).

ask_convert(Prompt, TransFun, Type,  Default) ->
    DefaultPrompt = erlang:iolist_to_binary([Prompt, default(Default), "> "]),
    NewPrompt = erlang:binary_to_list(DefaultPrompt),
    Data = trim(trim(io:get_line(NewPrompt)), both, [$\n]),
    case TransFun(Type, Data)  of
        no_data ->
            maybe_continue(Prompt, TransFun, Type, Default);
        no_clue ->
            continue(Prompt, TransFun, Type, Default);
        Ret ->
            Ret
    end.

maybe_continue(Prompt, TransFun, Type, Default) ->
    case Default of
        none ->
            continue(Prompt, TransFun, Type, Default);
        Default ->
            TransFun(Type, Default)
    end.

continue(Prompt, TransFun, Type, Default) ->
    say("I didn't understand that. A ~p is expected.~n", [Type]),
    ask_convert(Prompt, TransFun, Type, Default).

default(none) ->
    [];
default(Default) ->
    [" (", io_lib:format("~p", [Default]) , ")"].

get(boolean, []) ->
    no_data;
get(boolean, [In | _]) when In =:= $Y orelse In =:= $y ->
    true;
get(boolean, [In | _]) when In =:= $N orelse In =:= $n ->
    false;
get(boolean, _) ->
    no_clue;
get(integer, []) ->
    no_data;
get(number, String) ->
    get(integer, String);
get(integer, String) ->
    case (catch list_to_integer(String)) of
        {'Exit', _} ->
            no_clue;
        Integer ->
            Integer
    end;
get(string, []) ->
    no_data;
get(string, String) ->
    case is_list(String) of
        true ->
            String;
        false ->
            no_clue
    end.

-ifdef(unicode_str).
trim(Str, right, Chars) -> string:trim(Str, trailing, Chars);
trim(Str, left, Chars) -> string:trim(Str, leading, Chars);
trim(Str, both, Chars) -> string:trim(Str, both, Chars).
-else.
trim(Str) -> string:strip(rebar_utils:to_list(Str)).
trim(Str, Dir, [Chars|_]) -> string:strip(rebar_utils:to_list(Str), Dir, Chars).
-endif.

say(Say) ->
    io:format(lists:flatten([Say, "~n"])).

-spec say(string(), [term()] | term()) -> ok.
say(Say, Args) when is_list(Args) ->
    io:format(lists:flatten([Say, "~n"]), Args);
say(Say, Args) ->
    io:format(lists:flatten([Say, "~n"]), [Args]).