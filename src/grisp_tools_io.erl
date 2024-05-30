-module(grisp_tools_io).

-export([
         ask/3,
         ask/4
        ]).

ask(State, Prompt, Type) ->
    ask_convert(State, Prompt, fun get/2, Type, none).

ask(State, Prompt, Type, Default)  ->
    ask_convert(State, Prompt, fun get/2, Type, Default).

ask_convert(State, Prompt, TransFun, Type,  Default) ->
    DefaultPrompt = erlang:iolist_to_binary([Prompt, default(Default), "> "]),
    NewPrompt = erlang:binary_to_list(DefaultPrompt),
    Data = trim(trim(io:get_line(NewPrompt)), both, [$\n]),
    case TransFun(Type, Data)  of
        no_data ->
            maybe_continue(State, Prompt, TransFun, Type, Default);
        no_clue ->
            continue(State, Prompt, TransFun, Type, Default);
        Ret ->
            Ret
    end.

maybe_continue(State, Prompt, TransFun, Type, Default) ->
    case Default of
        none ->
            continue(State, Prompt, TransFun, Type, Default);
        Default ->
            TransFun(Type, Default)
    end.

continue(State, Prompt, TransFun, Type, Default) ->
    say(State, "Wrong input type. A ~p is expected.~n", [Type]),
    ask_convert(State, Prompt, TransFun, Type, Default).

default(none) ->
    [];
default(false) ->
    "(y/N)";
default(true) ->
    "(Y/n)";
default(Default) when is_list(Default) ->
    [" (", Default , ")"];
default(Default) ->
    [" (", io_lib:format("~p", [Default]) , ")"].

get(boolean, []) ->
    no_data;
get(boolean, [In | _]) when In =:= $Y orelse In =:= $y ->
    true;
get(boolean, [In | _]) when In =:= $N orelse In =:= $n ->
    false;
get(boolean, true) ->
    true;
get(boolean, false) ->
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
get(latin1, []) ->
    no_data;
get(latin1, Data) ->
    case io_lib:latin1_char_list(Data) of
        true ->
            Data;
        false ->
            no_clue
    end;
get(trim_string, []) ->
    no_data;
get(trim_string, String) ->
    case is_list(String) of
        true ->
            Trimmed = string:trim(String),
            unicode:characters_to_binary(Trimmed);
        false ->
            no_clue
    end;
get(string, []) ->
    no_data;
get(string, String) ->
    case is_list(String) of
        true ->
            unicode:characters_to_binary(String);
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

say(State, Say, Args) when is_list(Args) ->
    Event = {say, io_lib:format(lists:flatten([Say, "~n"]), Args)},
    grisp_tools_util:event(State, Event);
say(State, Say, Args) ->
    Event = {say, io_lib:format(lists:flatten([Say, "~n"]), [Args])},
    grisp_tools_util:event(State, Event).
