%%%-------------------------------------------------------------------
%%% @author toot
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2020 11:45 AM
%%%-------------------------------------------------------------------
-module(test_pollution_server).
-author("Adam Kania").
-include_lib("eunit/include/eunit.hrl").

-define(HOURt1, 17).
-define(HOURt2, 15).
-define(DAY1, {2020,4,7}).
-define(DAY1t1, {{2020,4,7},{17,55,35}}).
-define(DAY1t2, {{2020,4,7},{15,12,22}}).
-define(DAY2, {2020,4,6}).
-define(DAY2t1, {{2020,4,6},{17,55,35}}).
-define(DAY2t2, {{2020,4,6},{15,12,22}}).

%% API
-compile(export_all).

basic_test_() ->
  {setup,
    fun() -> pollution_server:start() end,
    fun(_) -> pollution_server:stop() end,
    fun(_) ->
      [ ?_assertEqual(ok, pollution_server:addStation("kono", {2,1})),
        ?_assertEqual(ok, pollution_server:addStation("dio", {1,2})),
        ?_assertEqual(ok, pollution_server:addValue("kono", ?DAY1t1, "pm2", 12)),
        ?_assertEqual(12, pollution_server:getOneValue("kono", "pm2", ?DAY1)),
        ?_assertEqual(ok, pollution_server:removeValue("kono", ?DAY1t1, "pm2")),
        ?_assertEqual('no data', pollution_server:getOneValue("kono", "pm2",  ?DAY1t1))
      ]
    end
  }.

data_manipulation_test_() ->
  {setup,
    fun() -> pollution_server:start(),
      pollution_server:addStation("kono", {1, 1}),
      pollution_server:addValue("kono", ?DAY2t2, "pm2", 55),
      pollution_server:addValue("kono", ?DAY1t1, "pm2", 100),
      pollution_server:addValue({1, 1}, ?DAY1t2, "pm2", 10),
      pollution_server:addValue({1, 1}, ?DAY1t2, "pm10", 501),
      pollution_server:addStation("dioda", {1,2}),
      pollution_server:addValue("dioda", ?DAY1t1, "pm2", 200),
      pollution_server:addValue("dioda", ?DAY1t2, "pm10", 0)   
    end,
    fun(_) -> pollution_server:stop() end,
    fun(_) ->
    [ ?_assert(55.0 =:= pollution_server:getStationMean("kono", "pm2")),
      ?_assert(310/3 =:= pollution_server:getDailyMean("pm2", ?DAY1)),
      ?_assert(65/2 =:= pollution_server:getHourlyMean("kono", "pm2", ?HOURt2)),
      ?_assert(2.0 =:= pollution_server:getDailyAverageDataCount("kono")),
      ?_assert(1 =:= pollution_server:getDailyOverLimit("pm10", 500, ?DAY1)),
      ?_assertMatch({_, _, 501.0}, pollution_server:getMaximumGradientStations())
     ]
    end
  }.