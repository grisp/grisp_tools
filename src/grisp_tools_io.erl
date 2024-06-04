-module(grisp_tools_io).

-export([
         ask/3,
         ask/4,
         ask/5
        ]).

%--- API -----------------------------------------------------------------------

ask(State, Prompt, Type) ->
    ask_convert(State, Prompt, fun get/2, Type, none, "").

ask(State, Prompt, Type, Default) ->
    ask_convert(State, Prompt, fun get/2, Type, Default, "").

ask(State, Prompt, Type, Default, Hint)  ->
    ask_convert(State, Prompt, fun get/2, Type, Default, Hint).

%--- Internal ------------------------------------------------------------------

ask_convert(State0, Prompt, TransFun, Type,  Default, Hint) ->
    DefaultPrompt = erlang:iolist_to_binary(
        ["\n", hint(Hint), Prompt, default(Default), "> "]
    ),
    NewPrompt = erlang:binary_to_list(DefaultPrompt),
    Event = {ask, NewPrompt},
    {RawUserInput, State1} = grisp_tools_util:event_with_result(State0, Event),
    Data = trim(trim(RawUserInput), both, [$\n]),
    case TransFun(Type, Data)  of
        no_data ->
            maybe_continue(State1, Prompt, TransFun, Type, Default, Hint);
        no_clue ->
            continue(State1, Prompt, TransFun, Type, Default, Hint);
        Ret ->
            Ret
    end.

maybe_continue(State, Prompt, TransFun, Type, Default, Hint) ->
    case Default of
        none ->
            continue(State, Prompt, TransFun, Type, Default, Hint);
        _ ->
            TransFun(Type, Default)
    end.

continue(State, Prompt, TransFun, Type, Default, Hint) ->
    Text = io_lib:format("Wrong input type. A ~p is expected", [Type]),
    say(State, color(208, Text)),
    ask_convert(State, Prompt, TransFun, Type, Default, Hint).

default(none) ->
    [];
default(false) ->
    color(51, " (y/N)");
default(true) ->
    color(51, " (Y/n)");
default(Default) when is_list(Default) ->
    DefaultText = io_lib:format(" (default: ~p)", [Default]),
    color(51, DefaultText);
default(Default) ->
    color(51, io_lib:format("(~p)", [Default])).

hint("") ->
    "";
hint(Hint) ->
    [color(242, "# " ++ Hint), "\n"].

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

say(State, Say) ->
    Event = {say, Say},
    grisp_tools_util:event(State, Event).


% @doc Add foreground coloring to the given text using ANSI ecape code
% See: https://en.wikipedia.org/wiki/ANSI_escape_code#8-bit
% @end
-spec color(Code, Text) -> string() when
      Code :: 0..255,
      Text :: string().
color(Code, Text) ->
    io_lib:format("\033[38;5;~pm~s\033[m", [Code, Text]).
