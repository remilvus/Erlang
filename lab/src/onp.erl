%%%-------------------------------------------------------------------
%%% @author toot
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2020 11:06 AM
%%%-------------------------------------------------------------------
-module(onp).
-author("toot").

%% API
-export([onp/1, realOnp/2]).

realOnp([], [Head]) -> Head;
realOnp(["+" | Tail], [A, B | STail]) -> realOnp(Tail, [B + A | Tail]);
realOnp(["-" | Tail], [A, B | STail]) -> realOnp(Tail, [B - A | Tail]);
realOnp(["*" | Tail], [A, B | STail]) -> realOnp(Tail, [B * A | Tail]);
realOnp(["/" | Tail], [A, B | STail]) -> realOnp(Tail, [B / A | Tail]);
realOnp([Head | Tail], Stack) -> realOnp(Tail, [list_to_integer(Head) | Stack]).

onp(List) when is_list(List) -> realOnp(string:tokens(List, " "), []).
