%%%-------------------------------------------------------------------
%%% @author toot
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Mar 2020 10:29 AM
%%%-------------------------------------------------------------------
-module(erlan).
-author("toot").

%% API
-export([power/2, foo/0]).

power(A, 0) -> 1;
power(A,B) -> power(A, B-1)*A.

foo() -> 23.
