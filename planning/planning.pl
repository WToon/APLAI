:-lib(ic_edge_finder).
:-lib(ic).
:-lib(branch_and_bound).

% The meeting/9 predicate.
meeting(NbOfPersons,Durations,OnWeekend,Rank,Precs,StartingDay,Start,EndTime,Viol):-
    initStart(Start,NbOfPersons),
    maxNbOfViols(NbOfPersons,MaxViols),
    findUpperTimeLimit(Durations,Limit),
    Start :: 0 .. Limit,    
    disjunctive(Start,Durations),
    setPrecConstraints(Precs, Start),
    setWeekendConstraints(StartingDay,Start,Durations,OnWeekend),
    MaxStart #= Start[NbOfPersons],
    maxlist(Start,MaxStart),
    EndTime #= Start[NbOfPersons] + Durations[NbOfPersons],
    nbOfViols(Rank,Start,NbOfPersons,Viol),
    Cost #= EndTime * MaxViols + Viol,
    minimize(labeling(Start),Cost).
    
% The meeting/9 predicate, this time with self-made disjunctive predicate.
selfMeeting(NbOfPersons,Durations,OnWeekend,Rank,Precs,StartingDay,Start,EndTime,Viol):-
    initStart(Start,NbOfPersons),
    maxNbOfViols(NbOfPersons,MaxViols),
    findUpperTimeLimit(Durations,Limit),
    Start :: 0 .. Limit,    
    self_disjunctive(Start,Durations,NbOfPersons),
    setPrecConstraints(Precs, Start),
    setWeekendConstraints(StartingDay,Start,Durations,OnWeekend),
    MaxStart #= Start[NbOfPersons],
    maxlist(Start,MaxStart),
    EndTime #= Start[NbOfPersons] + Durations[NbOfPersons],
    nbOfViols(Rank,Start,NbOfPersons,Viol),
    Cost #= EndTime * MaxViols + Viol,
    minimize(labeling(Start),Cost).

% Find the upper time limit for the starting times. Sum number of weeks needed for individual meetings.
findUpperTimeLimit(Durations,Limit):-
    array_list(Durations,DurationsList),
    findUpperTimeLimit(DurationsList,0,Limit).
    
findUpperTimeLimit([],Acc,Acc).
findUpperTimeLimit([H|T],Acc,Result):-
    NewAcc is Acc + (H // 7 + 1)*7,
    findUpperTimeLimit(T,NewAcc,Result).

% Make the start term the correct length and structure
initStart(Start,NbOfPersons) :-
    length(StartList,NbOfPersons),
    array_list(Start,StartList).

% Set the precedes constraints
setPrecConstraints(Precs,Start):-
    array_list(Precs,PrecList),
    (foreach([](A,B),PrecList),param(Start) do
        Start[A] #< Start[B]).

% Calculate (the maximum number of violations) + 1
maxNbOfViols(NbOfPersons,Max) :-
    N1 is NbOfPersons - 1,
    Max is NbOfPersons * N1 // 2 + 1.

% Get the correct number of violations for the given ranks and start times
nbOfViols(Rank,Start,NbOfPersons,NbOfViols):-
    (multifor([I,J],1,NbOfPersons),foreach(L,List), param(Rank,Start) do 
        and(Rank[I] #< Rank[J],Start[I] #> Start[J],L)
    ),
    NbOfViols #= sum(List).
    
% Convert the day to a weekday with the starting day 
toWeekDay(StartingDay,Day,WeekDay):-
    WeekDay :: 0 .. 6,
    WeekDay + _ * 7 #= Day + StartingDay.

% Set the weekend constraint for one start and duration
setWeekendConstrait(Start,Duration,AllowedInWeekend):-
    #<(5 - Start,Duration,InWeekend),
    InWeekend #=< AllowedInWeekend.

% Set all weekend constraints
setWeekendConstraints(StartingDay,Starts,Durations,OnWeekends):-
    array_list(Starts,StartsList),
    array_list(Durations,DurationsList),
    array_list(OnWeekends,OnWeekendsList),
    (foreach(Start,StartsList),foreach(Duration,DurationsList),foreach(OnWeekend,OnWeekendsList), param(StartingDay) do
        toWeekDay(StartingDay,Start,RealStartDay),
        setWeekendConstrait(RealStartDay,Duration,OnWeekend)
    ).

% Self-made disjunctive constraint
self_disjunctive(Start,Duration,NbOfPersons):-
    (for(I,1,NbOfPersons), param(Start,Duration,NbOfPersons) do
        I1 is I + 1,
        (for(J,I1,NbOfPersons), param(I,Start,Duration) do
            or(Start[I] #>= Start[J] + Duration[J],Start[J] #>= Start[I] + Duration[I])
        )
    ).
    