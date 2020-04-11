%%%-------------------------------------------------------------------
%%% @author toot
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 06. Apr 2020 6:24 PM
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("toot").
-define(SERVER, poll_serv).

%% API
-compile(export_all).

start() ->
  case whereis(?SERVER) of
    undefined -> register(?SERVER, spawn(?MODULE, init, []));
    Pid -> ok
  end.

stop() ->  ?SERVER ! stop, unregister(?SERVER).

init() ->
  Monitor = pollution:createMonitor(),
  server(Monitor).

server(Monitor) ->
  receive
    {Pid, Function, Args} ->
      Result = apply(pollution, Function, Args ++ [Monitor]),
      io:format("~w ~n", [Pid]),
      Pid ! {result, Result},
      server(Monitor);
    {Pid, askMonitor} ->
      Pid ! {result, Monitor},
      server(Monitor);
    {Function, Args} ->
      NewMonitor = apply(pollution, Function, Args ++ [Monitor]),
      server(NewMonitor);
    stop -> ok
  end.



askStation(Function, Args) ->
  ?SERVER ! {self(), Function, Args},
  receive
    {result, Result} -> Result
  after
    120000 -> 'no result'
  end.

askMonitor() ->
  ?SERVER ! {self(), askMonitor},
  receive
    {result, Result} -> Result
  after
    120000 -> 'no result'
  end.

% checks whether a station exists and adds it if it doesn't
addStation(Name, Location) ->
  ?SERVER ! {addStation, [Name, Location]}.

% adds a measurement to station's list of measurements
addValue(Identifier, Time, MeasurementType, Measurement) ->
  ?SERVER ! {addValue, [Identifier, Time, MeasurementType, Measurement]}.

% removes a measurement from  station's list of measurements based on type and time
removeValue(Identifier, Time, Type) ->
  ?SERVER ! {removeValue, [Identifier, Time, Type]}.

% finds measurement in station's data based on statin's name/location, type and time
getOneValue(Identifier, Type, Time) ->
  ?SERVER ! {getOneValue, [Identifier, Type, Time]}.

% returns a station by name or location
getStation(Identifier) ->
  askStation(getStation, [Identifier]).

% calculates mean value of measurements of certain type of certain station
getStationMean(Idetifier, Type) ->
  askStation(getStationMean, [Idetifier, Type]).

% Calculates global daily mean of a measurement
getDailyMean(Type, Date) ->
  askStation(getDailyMean, [Type, Date]).

% calculates mean value of measurements from a station at specific hour
getHourlyMean(Id, Type, Hour) ->
  askStation(getHourlyMean, [Id, Type, Hour]).

% daily average count of measurements, but only from days with data
getDailyAverageDataCount(Id, Monitor) ->
  askStation(getDailyAverageDataCount, [Id, Monitor]).

% counts all station that on a given day at least once were over limit of a given parameter
getDailyOverLimit(Type, Limit, Date) ->
  askStation(getDailyOverLimit, [Type, Limit, Date]).

% finds two stations with maximum gradient (pollution difference/distance), returns a tuple {NameA, NameB, Gradient}
getMaximumGradientStations() ->
  askStation(getMaximumGradientStations, []).

% finds maximum value of a Measurement with given type
getGlobalMax(Type) ->
  askStation(getGlobalMax, [Type]).

% finds minimum value of a Measurement with given type
getGlobalMin(Type) ->
  askStation(getGlobalMin, [Type]).
