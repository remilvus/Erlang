%%%-------------------------------------------------------------------
%%% @author toot
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. Apr 2020 11:47 AM
%%%-------------------------------------------------------------------
-module(parcellockerfinder).
-author("toot").
-define(CORES, 4).

%% API
-compile(export_all).


% UTILITY
getDist({XA, YA}, {XB, YB}) -> math:sqrt(math:pow(XA-XB, 2) + math:pow(YA-YB, 2)).

getLocations(N) -> [{rand:uniform(10001)-1, rand:uniform(10001)-1} || _ <- lists:seq(1, N)].

findClosest(MinLoc, LocA, [LocB | Locations]) ->
  DistMin = getDist(MinLoc, LocA),
  DistB = getDist(LocB, LocA),
  if
    DistMin > DistB -> findClosest(LocB, LocA, Locations);
    true -> findClosest(MinLoc, LocA, Locations)
  end;
findClosest(MinLoc, LocA, []) -> MinLoc.

% finds Locker for one person
findMyParcelLocker(PersonLocation, [LockerLocation | LockerLocations]) ->
  findClosest(LockerLocation, PersonLocation, LockerLocations).

compare(Args) ->
  {T1, _} = timer:tc(?MODULE, findOurParcelLockers, Args),
  io:format("sequential time: ~f s ~n", [T1/1000000]),
  {T2, _} = timer:tc(?MODULE, ultraParallelFindOurParcelLockers, Args),
  io:format("parallel time: ~f s ~n", [T2/1000000]),
  {T3, _} = timer:tc(?MODULE, parallelFindOurParcelLockers, Args),
  io:format("partially parallel time: ~f s  ~n", [T3/1000000]).

% SEQUENTIAL
% finds lockers for list of people (sequential)
findOurParcelLockers(PeopleLocations, LockerLocations) ->
  lists:map(fun(PersonLoc) -> {PersonLoc ,findMyParcelLocker(PersonLoc, LockerLocations)} end, PeopleLocations).

% TOO PARALLEL
% finds Locker for one person
ultraParallelFindMyParcelLocker(PersonLocation, [LockerLocation | LockerLocations]) ->
  ClosestLocker = findClosest(LockerLocation, PersonLocation, LockerLocations),
  receive
    Pid when(is_pid(Pid)) -> Pid ! {result, {PersonLocation, ClosestLocker}}
  after
    120000 -> throw("お前はもう死んでいる")
  end.

% waits for results for 2 minutes or for number of results equal to Expect
getResults(State, Expect) when length(State)==Expect -> State;
getResults(State, Expect) ->
  receive
    {result, Result} -> getResults([Result | State], Expect)
  after
    120000 -> io:format("not all results were calculated"), State
  end.

% finds lockers for list of people
ultraParallelFindOurParcelLockers(PeopleLocations, LockerLocations) ->
  Kiddos = lists:map(fun(Person) ->
                      spawn(?MODULE, ultraParallelFindMyParcelLocker, [Person, LockerLocations])
                     end,
                     PeopleLocations),
  lists:foreach(fun(KidPid) -> KidPid ! self() end, Kiddos),
  getResults([], length(PeopleLocations)).


% PARALLEL
% finds lockers for list of people (sequential) and report it
findLockersAndReport(PeopleLocations, LockerLocations) ->
  Result = findOurParcelLockers(PeopleLocations, LockerLocations),
  receive
    Pid when(is_pid(Pid)) -> Pid ! {result, Result}
  after
    120000 -> throw("Parallel calculation lost")
  end.

% finds lockers for list of people (parallel)
parallelFindOurParcelLockers(PeopleLocations, LockerLocations) ->
  SublistLen = length(PeopleLocations) div ?CORES,
  PeopleLocationsDivided = [lists:sublist(PeopleLocations, I * SublistLen + 1, SublistLen) || I <- lists:seq(0, ?CORES-1)],
  Kiddos = lists:map(fun(People) ->
    spawn(?MODULE, findLockersAndReport, [People, LockerLocations])
                     end,
    PeopleLocationsDivided),
  lists:foreach(fun(KidPid) -> KidPid ! self() end, Kiddos),
  getResults([], ?CORES).



