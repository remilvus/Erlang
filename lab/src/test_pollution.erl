%%%-------------------------------------------------------------------
%%% @author toot
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. Apr 2020 11:45 AM
%%%-------------------------------------------------------------------
-module(test_pollution).
-author("toot").
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

make_example() ->
  pollution:addValue("dioda", ?DAY1t2, "pm10", 0,
    pollution:addValue("dioda", ?DAY1t1, "pm2", 200,
      pollution:addStation("dioda", {1,2},
        pollution:addValue({1, 1}, ?DAY1t2, "pm10", 501,
          pollution:addValue({1, 1}, ?DAY1t2, "pm2", 10,
            pollution:addValue("kono", ?DAY1t1, "pm2", 100,
              pollution:addValue("kono", ?DAY2t2, "pm2", 55,
                pollution:addStation("kono", {1, 1},
                  pollution:createMonitor())))))))).

% DATA MANIPULATION TESTS
adding_station_name_test() ->
  ?assertEqual(pollution:addStation("kono", {0,0}, pollution:createMonitor()),
    pollution:addStation("kono", {1,1}, pollution:addStation("kono", {0,0}, pollution:createMonitor()))).

adding_station_location_test() ->
  ?assertEqual(pollution:addStation("kono", {1,1}, pollution:createMonitor()),
    pollution:addStation("dio", {1,1}, pollution:addStation("kono", {1,1}, pollution:createMonitor()))).

removing_test() ->
  'no data' = pollution:getOneValue("kono", "pm10",  ?DAY1t2,
                pollution:removeValue("kono", ?DAY1t2, "pm10",
                  test_pollution:make_example())).

%% todo add value test


% INFORMATION GETTING TESTS
station_mean_test() ->  ?assert(55.0 =:= pollution:getStationMean("kono", "pm2", make_example())).

station_daily_mean_test() ->  ?assert(310/3 =:= pollution:getDailyMean("pm2", ?DAY1,  make_example())).

station_hourly_mean_test() ->  ?assert(65/2 =:= pollution:getHourlyMean("kono", "pm2", ?HOURt2,  make_example())).

data_count_test() -> ?assert(2.0 =:= pollution:getDailyAverageDataCount("kono",  make_example())).

over_limit_test() -> ?assert(1 =:= pollution:getDailyOverLimit("pm10", 500, ?DAY1, make_example())).

gradient_test() -> ?assertMatch({_, _, 501.0}, pollution:getMaximumGradientStations(make_example())).

% ERROR TESTS
error_gradient_test() -> ?assertThrow("no data",
  pollution:getMaximumGradientStations(pollution:createMonitor())).

error_data_count_test() -> ?assertThrow("Monitor is empty",
  pollution:getDailyAverageDataCount("kono", pollution:createMonitor())).

error_station_mean_test() -> ?assertThrow("Station doesn't exist",
  pollution:getStationMean("jojo", "pm2", make_example())).