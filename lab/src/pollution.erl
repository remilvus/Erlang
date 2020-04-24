%%%-------------------------------------------------------------------
%%% @author toot
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 17. Mar 2020 5:39 PM
%%%-------------------------------------------------------------------
-module(pollution).
-author("toot").

-record(station, {location, name, data=[]}).

%% API
%%-export_all.
-export([getGlobalMin/2, getGlobalMax/2 ,getMaximumGradientStations/1, getDailyOverLimit/4, getStation/2, createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getDailyAverageDataCount/2, getHourlyMean/4]).

createMonitor() -> []. %% data in format of a list

%getExample() -> [#station{name="kono", location={42,42}, data=[{pm2, {{1},{1}}, 10}, {pm2, {{1},{2}}, 20}, {pm10, {{1},{1}}, 300}]}, #station{name="dio", location={40,40}, data=[{pm2, {{1},{1}}, 20}]}, #station{name="da", location={1,1}, data=[]}].

% returns a station by name or location
getStation(Identifier, []) -> throw("Station doesn't exist");
getStation(Identifier, [Station=#station{location=Loc, name=Identifier, data=Data} | Monitor]) -> Station;
getStation(Identifier, [Station=#station{location=Identifier, name=Name, data=Data} | Monitor]) -> Station;
getStation(Identifier, [_ | Monitor]) -> getStation(Identifier, Monitor).

% checks whether a station exists and adds it if it doesn't
addStation(Name, _, [#station{location=Location, name=Name, data=Data} | Monitor]) -> [#station{location=Location, name=Name, data=Data} | Monitor];
addStation(_, Location, [#station{location=Location, name=Name, data=Data} | Monitor]) -> [#station{location=Location, name=Name, data=Data} | Monitor];
addStation(Name, Location, [Station | Monitor]) -> [Station | addStation(Name, Location, Monitor)];
addStation(Name, Location, []) -> [#station{name=Name, location=Location}].

% adds an entry to list with station's measurements
addToData(MeasurementType, Time, Measurement, []) -> [{MeasurementType, Time, Measurement}];
addToData(MeasurementType, Time, Measurement, [{MeasurementType, Time, Value} | Tail]) -> [{MeasurementType, Time, Value} | Tail];
addToData(MeasurementType, Time, Measurement, [Head | Tail]) -> [Head | addToData(MeasurementType, Time, Measurement, Tail)].

% adds a measurement to station's list of measurements
addValue(Identifier, Time, MeasurementType, Measurement, []) -> throw("Station doesn't exist");
addValue(Identifier, Time, MeasurementType, Measurement, [#station{location=Identifier, name=Name, data=Data} | Monitor]) ->
        [#station{data=addToData(MeasurementType, Time, Measurement, Data), name=Name, location=Identifier}  | Monitor];
addValue(Identifier, Time, MeasurementType, Measurement, [#station{location=Location, name=Identifier, data=Data} | Monitor]) ->
        [#station{data=addToData(MeasurementType, Time, Measurement, Data), name=Identifier, location=Location}  | Monitor];
addValue(Identifier, Time, MeasurementType, Measurement, [Station | Monitor]) ->
        [Station | addValue(Identifier, Time, MeasurementType, Measurement, Monitor)].

% removes a measurement from list of measurements based on type and time
removeEntry(Type, Time, []) -> [];
removeEntry(Type, Time, [{Type, Time, _} | Data]) -> Data;
removeEntry(Type, Time, [Entry | Data]) -> [Entry | removeEntry(Type, Time, Data)].

% removes a measurement from  station's list of measurements based on type and time
removeValue(Identifier, Time, Type, []) -> throw("Station doesn't exist");
removeValue(Identifier, Time, Type, [Station | Monitor]) when is_list(Identifier) ->
  case Station#station.name of
    Identifier -> Data = Station#station.data,
                  [Station#station{data=removeEntry(Type, Time, Data)}  | Monitor];
    _ -> [Station | removeValue(Identifier, Time, Type, Monitor)]
  end;
removeValue(Identifier, Time, Type, [Station | Monitor]) ->
  case Station#station.location of
    Identifier -> Data = Station#station.data,
      [Station#station{data=removeEntry(Type, Time, Data)}  | Monitor];
    _ -> [Station | removeValue(Identifier, Time, Type, Monitor)]
  end.

% finds measurement from list based on type and time
getEntry(_, _, []) -> 'no data';
getEntry(Type, Time, [{Type, {Time, _}, Value} | Data]) -> Value;
getEntry(Type, Time, [_ | Data]) -> getEntry(Type, Time, Data).

% finds measurement in station's data based on statin's name/location, type and time
getOneValue(Idetifier, Type, Time, []) -> throw("Station doesn't exist");
getOneValue(Idetifier, Type, Time, [#station{name=Idetifier, data=Data} | Monitor]) -> getEntry(Type, Time, Data);
getOneValue(Idetifier, Type, Time, [#station{location=Idetifier, data=Data} | Monitor]) -> getEntry(Type, Time, Data);
getOneValue(Idetifier, Type, Time, [Station | Monitor]) -> getOneValue(Idetifier, Type, Time, Monitor).

% calculates mean from list of values
mean([]) -> 'no data';
mean(List) -> {S, M} = lists:foldl(fun(Val, {Sum, N}) -> {Sum + Val, N+1} end, {0, 0}, List), S/M.

% calculates mean value of measurements of certain type from list of all measurements
getTypeMean(Type, Data) -> mean(lists:filtermap(fun ({EnType, _, Value}) ->
                                                      case EnType of
                                                        Type -> {true, Value};
                                                        _ -> false
                                                      end end, Data)).

% calculates mean value of measurements of certain type of certain station
getStationMean(Idetifier, Type, []) -> throw("Station doesn't exist");
getStationMean(Idetifier, Type, [#station{name=Idetifier, data=Data} | Monitor]) -> getTypeMean(Type, Data);
getStationMean(Idetifier, Type, [#station{location=Idetifier, data=Data} | Monitor]) -> getTypeMean(Type, Data);
getStationMean(Idetifier, Type, [_ | Monitor]) -> getStationMean(Idetifier, Type, Monitor).


% gets all measurements with given type and date from measurements list
getDateType(Type, Date, Data) -> lists:filtermap(fun ({EnType, {EnDate, _}, Value}) ->
                                                      case {EnType, EnDate} of
                                                        {Type, Date} -> {true, Value};
                                                          _ -> false
                                                      end end, Data).

% gets all measurements with given type and date from monitor
getFilterData(Type, Date, Monitor) -> lists:map(fun (#station{data=Data}) -> getDateType(Type, Date, Data) end, Monitor).

% Calculates global daily mean of a measurement
getDailyMean(Type, Date, []) -> throw("No stations");
getDailyMean(Type, Date, Monitor) -> mean(lists:concat(getFilterData(Type, Date, Monitor))).

% filters all measurements from Data based on their type and hour
getHourType(Type, Hour, Data) -> lists:filtermap(fun ({EnType, {_, {EnHour, _, _}}, Value}) ->
  case {EnType, EnHour} of
    {Type, Hour} -> {true, Value};
    _ -> false
  end end, Data).

% calculates mean value of measurements from a station at specific hour
getHourlyMean(Id, Type, Hour, []) -> throw("No stations");
getHourlyMean(Id, Type, Hour, [#station{name=Id, data=Data}| Monitor]) -> mean(lists:concat([ [] | [getHourType(Type, Hour, Data)]]));
getHourlyMean(Id, Type, Hour, [#station{location=Id, data=Data}| Monitor]) -> mean(lists:concat([ [] | [getHourType(Type, Hour, Data)]]));
getHourlyMean(Id, Type, Hour, [_ | Monitor]) -> getHourlyMean(Id, Type, Hour, Monitor).

% counter for foldl from `getDailyAverageDataCount`
incDate({_, {Date, _}, _}, []) -> [{Date, 1}];
incDate({_, {Date, _}, _}, [{Date, Count} | DayCount]) -> [{Date, Count+1} | DayCount];
incDate({_, {Date, _}, _}, [Count | DayCount]) -> [Count | incDate({0, {Date, 0}, 0}, DayCount)].

% daily average count of measurements, but only from days with data
getDailyAverageDataCount(Id, []) -> throw("Monitor is empty");
getDailyAverageDataCount(Id, Monitor) -> Station = getStation(Id, Monitor),
  Count = lists:foldl(fun incDate/2, [], Station#station.data),
  lists:foldl(fun({_, Num}, Acc) -> Acc + Num end, 0, Count) / length(Count).

% counts all station that on a given day at least once were over limit of a given parameter
getDailyOverLimit(Type, Limit, Date, Monitor) -> length(lists:filter(fun (#station{data=Data}) ->
  TypeData = getDateType(Type, Date, Data),
  Over = lists:filter( fun(Val) -> Val > Limit end, TypeData),
 length(Over) > 0 end,
  Monitor)).

% finds maximum value of a measurement from a list of measurements
getMax(List) -> getMax(0, List).
getMax(Base, []) -> Base;
getMax(Base, [{_,_,Value} | Data]) -> getMax(max(Base,Value), Data).

% finds minimum value of a measurement from a list of measurements
getMin(List) -> getMin(9999999, List).
getMin(Base, []) -> Base;
getMin(Base, [{_,_,Value} | Data]) -> getMin(min(Base,Value), Data).

% calculates distance between two locations
getDist({XA, YA}, {XB, YB}) -> math:sqrt(math:pow(XA-XB, 2) + math:pow(YA-YB, 2)).

% from two pairs {_, Val} returns one with bigger Val
getMaxPair({NameA, ValA}, {NameB, ValB}) -> if ValA > ValB -> {NameA, ValA}; ValA =< ValB -> {NameB, ValB} end.

% lowest level search for stations with maximum gradient; given a station and min measurement finds second station with max gradient
getMaxGradientWithMax(Base, Name, LocA, MaxA, [#station{name=NameB, location=LocB, data=[]} | Monitor]) ->
  getMaxGradientWithMax(Base, Name, LocA, MaxA, Monitor);
getMaxGradientWithMax(Base, Name, LocA, MinA, [#station{name=NameB, location=LocB, data=DataB} | Monitor]) ->
  MaxB = getMax(DataB),
  Dist = getDist(LocA, LocB),
  Grad = abs(MaxB - MinA)/Dist,
  {NextName, NextBase} = getMaxPair({Name, Base}, {NameB, Grad}),
  getMaxGradientWithMax(NextBase, NextName, LocA, MinA, Monitor);
getMaxGradientWithMax(Base, Name, LocA, MaxA, []) -> {Base, Name}.

% lowest level search for stations with maximum gradient; given a station and max measurement finds second station with max gradient
getMaxGradientWithMin(Base, Name, LocA, MaxA, [#station{name=NameB, location=LocB, data=[]} | Monitor]) ->
  getMaxGradientWithMin(Base, Name, LocA, MaxA, Monitor);
getMaxGradientWithMin(Base, Name, LocA, MaxA, [#station{name=NameB, location=LocB, data=DataB} | Monitor]) ->
  MinB = getMin(DataB),
  Dist = getDist(LocA, LocB),
  Grad = abs(MinB - MaxA)/Dist,
  {NextName, NextBase} = getMaxPair({Name, Base}, {NameB, Grad}),
  getMaxGradientWithMin(NextBase, NextName, LocA, MaxA, Monitor);
getMaxGradientWithMin(Base, Name, LocA, MaxA, []) -> {Base, Name}.

% lower level search for stations with maximum gradient;
getMaximumGradientStations(BaseNameA, BaseNameB, BaseGrad, [#station{name=Name, location=Loc, data=[]} | Monitor]) ->
  getMaximumGradientStations(BaseNameA, BaseNameB, BaseGrad , Monitor);
getMaximumGradientStations(BaseNameA, BaseNameB, BaseGrad, [#station{name=Name, location=Loc, data=Data} | Monitor]) ->
  {GradA, NameA} = getMaxGradientWithMax(0, "", Loc, getMin(Data), Monitor),
  {GradB, NameB} = getMaxGradientWithMin(0, "", Loc, getMax(Data), Monitor),
  {GradName, Grad} = getMaxPair({NameA, GradA}, {NameB, GradB}),
  {{NextNameA, NextNameB}, NextGrad} = getMaxPair({{BaseNameA, BaseNameB}, BaseGrad}, {{Name, GradName}, Grad}),
  {NextNameA, NextNameB, NextGrad};%getMaximumGradientStations(NextNameA, NextNameB, NextGrad , Monitor);
getMaximumGradientStations(BaseNameA, BaseNameB, BaseGrad, []) -> {BaseNameA, BaseNameB, BaseGrad}.

% finds two stations with maximum gradient (pollution difference/distance), returns a tuple {NameA, NameB, Gradient}
getMaximumGradientStations([#station{name=Name, location=Loc, data=[]} | Monitor]) ->
  getMaximumGradientStations(Monitor);
getMaximumGradientStations([]) -> throw("no data");
getMaximumGradientStations([#station{name=Name, location=Loc, data=Data} | Monitor]) ->
  {GradA, NameA} = getMaxGradientWithMax(0, "", Loc, getMin(Data), Monitor),
  {GradB, NameB} = getMaxGradientWithMin(0, "", Loc, getMax(Data), Monitor),
  {GradName, Grad} = getMaxPair({NameA, GradA}, {NameB, GradB}),
  getMaximumGradientStations(Name, GradName, Grad, Monitor).

% finds max value of a measurement with given type from a list od Measurements
getTypeMax(Type, List) -> getTypeMax(Type, 0, List).
getTypeMax(Type, Base, []) -> Base;
getTypeMax(Type, Base, [{Type,_,Value} | Data]) -> getMax(max(Base,Value), Data);
getTypeMax(Type, Base, [_ | Data]) -> getTypeMax(Type, Base, Data).

% finds min value of a measurement with given type from a list od Measurements
getTypeMin(Type, List) -> getTypeMin(Type, 999999, List).
getTypeMin(Type, Base, []) -> Base;
getTypeMin(Type, Base, [{Type,_,Value} | Data]) -> getMin(min(Base,Value), Data);
getTypeMin(Type, Base, [_ | Data]) -> getTypeMin(Type, Base, Data).

% finds maximum value of a Measurement with given type
getGlobalMax(Type, []) -> 0;
getGlobalMax(Type, [#station{data=Data} | Monitor]) -> max(getTypeMax(Type, Data),  getGlobalMax(Type, Monitor)).

% finds minimum value of a Measurement with given type
getGlobalMin(Type, []) -> 999999;
getGlobalMin(Type, [#station{data=Data} | Monitor]) -> min(getTypeMin(Type, Data),  getGlobalMin(Type, Monitor)).
