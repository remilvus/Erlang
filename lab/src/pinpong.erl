%%%-------------------------------------------------------------------
%%% @author toot
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. Apr 2020 7:44 PM
%%%-------------------------------------------------------------------
-module(pinpong).
-author("toot").

%% API
-compile(export_all).

play(N) -> pingo ! {start ,N}.

stop() -> pingo ! stop, pongo ! stop.

ping() -> ping(0).

ping(State) ->
  receive
    0 -> ok;
    stop -> ok;
    {start ,N} when is_number(N) -> timer:sleep(300), io:format("ping (communicate sum: ~B)~n", [State]), pongo)! N-1, ping(State);
    N when is_number(N) -> timer:sleep(300), io:format("ping (communicate sum: ~B) ~n",[State + N]), pongo ! N-1, ping(State + N)
  after
    20000 -> io:format("ping died of old age ~n")
  end.

pong() ->
  receive
    0 -> ok;
    stop -> ok;
    N when is_number(N) -> timer:sleep(100), io:format("pong ~n"), whereis(pingo) ! N-1, pong()
  after
    20000 -> io:format("pong died of old age ~n")
  end.


start() ->
  register(pingo, spawn(?MODULE, ping, [])),
  register(pongo, spawn(?MODULE, pong, [])).