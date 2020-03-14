%%%-------------------------------------------------------------------
%%% @author toot
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2020 10:48 AM
%%%-------------------------------------------------------------------
-module('MyLists').
-author("toot").

%% API
-export([contains/2, duplicate/1, sumFloats/1]).

contains([], Value) -> false;
contains([Head | Tail], Head) -> true;
contains([Head | Tail], Value) -> contains(Tail, Value).

duplicate([]) -> [];
duplicate([Head | Tail]) -> [Head, Head | duplicate(Tail)].

realSumFloats([], Acc) -> Acc;
realSumFloats([Head | Tail], Acc) when is_float(Head) -> realSumFloats(Tail, Head+Acc);
realSumFloats([Head | Tail], Acc) -> realSumFloats(Tail, Acc).

sumFloats(List) when is_list(List) -> realSumFloats(List, 0).