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
-export([onp/1]).

to_num(Str) ->
  case string:to_float(Str) of
    {error,no_float} -> list_to_integer(Str);
    {F,_Rest} -> F
  end.

realOnp([], [Head]) -> Head;
realOnp(["+" | Tail], [A, B | STail]) -> realOnp(Tail, [B + A | STail]);
realOnp(["-" | Tail], [A, B | STail]) -> realOnp(Tail, [B - A | STail]);
realOnp(["*" | Tail], [A, B | STail]) -> realOnp(Tail, [B * A | STail]);
realOnp(["/" | Tail], [A, B | STail]) -> realOnp(Tail, [B / A | STail]);
realOnp(["sqrt" | Tail], [A | STail]) -> realOnp(Tail, [math:sqrt(A) | STail]);
realOnp(["pow" | Tail], [A, B | STail]) -> realOnp(Tail, [math:pow(B, A) | STail]);
realOnp(["sin" | Tail], [A | STail]) -> realOnp(Tail, [math:sin(A)| STail]);
realOnp(["cos" | Tail], [A | STail]) -> realOnp(Tail, [math:cos(A) | STail]);
realOnp(["covid-19" | Tail], [A | STail]) -> realOnp(Tail, STail); % variable dies
realOnp([], []) -> erlang:error('variables died');
realOnp(["+-*" | Tail], [A, B | STail]) -> realOnp(Tail, [(B+A)*(B-A) | STail]);
realOnp([Head | Tail], Stack) -> realOnp(Tail, [to_num(Head) | Stack]).

onp(List) when is_list(List) -> realOnp(string:tokens(List, " "), []).

%% examples:
%% onp:onp("3 cos 2 pow 3 sin 2 pow +").
%% onp:onp("3 covid-19 3 1 +-*").