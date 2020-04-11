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

%% API
-compile(export_all).

getDist({XA, YA}, {XB, YB}) -> math:sqrt(math:pow(XA-XB, 2) + math:pow(YA-YB, 2)).


findClosest(MinLoc, LocA, [LocB | Locations]) ->
  DistMin = getDist(MinLoc, LocA),
  DistB = getDist(LocB, LocA),
  if
    DistMin > DistB -> findClosest(LocB, LocA, Locations);
    true -> findClosest(MinLoc, LocA, Locations)
  end;
findClosest(MinLoc, LocA, []) -> MinLoc.


findMyParcelLocker(PersonLocation, [LockerLocation | LockerLocations]) ->
  findClosest(LockerLocation, PersonLocation, LockerLocations).


findOurParcelLockers(PeopleLocations, LockerLocations) ->
  lists:map(fun(PersonLoc) -> {PersonLoc ,findMyParcelLocker(PersonLoc, LockerLocations)} end, PeopleLocations).


getLocations(N) -> [{rand:uniform(10001)-1, rand:uniform(10001)-1} || _ <- lists:seq(1, N)].
