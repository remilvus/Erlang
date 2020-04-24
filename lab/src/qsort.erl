%%%-------------------------------------------------------------------
%%% @author toot
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Mar 2020 4:41 PM
%%%-------------------------------------------------------------------
-module(qsort).
-author("toot").

%% API
-export([randomElems/3, qs/1, compareSpeeds/3]).

lessThan(List, Arg) -> lists:filter(fun(X) -> X < Arg end , List).
grtEqThan(List, Arg) -> lists:filter(fun(X) -> X >= Arg end, List).

qs([]) -> [];
qs([Pivot|Tail]) -> qs( lessThan(Tail,Pivot) ) ++ [Pivot] ++ qs( grtEqThan(Tail,Pivot) ).


randomElems(N,Min,Max)-> [Min - 1 + rand:uniform(Max + 1 - Min) || I <- lists:seq(1,N)].

% example: qsort:compareSpeeds(qsort:randomElems(1000, 1, 100), fun qsort:qs/1, fun lists:sort/1).
compareSpeeds(List, Fun1, Fun2) ->
  {Time1, _} = timer:tc(Fun1, [List]),
  {Time2, _} = timer:tc(Fun2, [List]),
  io:format("Fun1 time: ~b(ms) ~nFun2 time: ~b(ms) ~n", [Time1, Time2]).


% Fun exercises
% Map = fun (Fun, List) -> [Fun(X) || X <- List] end.
% Filter = fun (Fun, List) -> [X || X <- List, Fun(X)] end.
% CountDigits1 = fun(X) -> lists:foldl(fun (_, Acc) -> Acc + 1 end, 0, integer_to_list(X)) end.
% lists:filter(fun(X) -> X rem 3 == 0 end, qsort:randomElems(1000000,1,100)).