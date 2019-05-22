:-lib(ic_edge_finder).
:-lib(ic).
:-lib(branch_and_bound).
:-lib(listut).

% The meeting/9 predicate.
meeting(NbOfPersons,Durations,OnWeekend,Rank,Precs,StartingDay,Start,EndTime,Viol):-
    initStart(Start,NbOfPersons),
    array_list(Rank,RankList),
    maxNbOfViols(RankList,1,0,MaxViols),
    %maxNbOfViols(NbOfPersons,MaxViols),
    findUpperTimeLimit(Durations,OnWeekend,StartingDay,Limit),
    Start :: 0 .. Limit,    
    disjunctive(Start,Durations),
    setPrecConstraints(Precs, Start),
    setWeekendConstraints(StartingDay,Start,Durations,OnWeekend),
    setSymmetryBreakingConstraints(Durations,Start,Rank,Precs,OnWeekend),
    MaxStart #= Start[NbOfPersons],
    maxlist(Start,MaxStart),
    nbOfViols(Rank,Start,NbOfPersons,Viol),
    Cost #= MaxStart * MaxViols + Viol,
    minimize(labeling(Start),Cost),
    EndTime is Start[NbOfPersons] + Durations[NbOfPersons].
    
% The meeting/9 predicate, this time with self-made disjunctive predicate.
selfMeeting(NbOfPersons,Durations,OnWeekend,Rank,Precs,StartingDay,Start,EndTime,Viol):-
    initStart(Start,NbOfPersons),
    maxNbOfViols(NbOfPersons,MaxViols),
    findUpperTimeLimit(Durations,OnWeekend,StartingDay,Limit),
    Start #:: 0 .. Limit,    
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
findUpperTimeLimit(Durations,OnWeekend,StartingDay,Limit):-
    array_list(Durations,DurationsList),
    array_list(OnWeekend,OnWeekendsList),
    findUpperTimeLimit(DurationsList,0,OnWeekendsList,StartingDay,Limit).
    
findUpperTimeLimit([],Acc,[],_,Acc).
findUpperTimeLimit([Duration|DurationTail],Acc,[Weekend|OnWeekendsTail],StartingDay,Result):-
    Weekend = 0,
    Duration < 6,
    5 - StartingDay < Duration,
    NewAcc is Acc + 7 - StartingDay,
    findUpperTimeLimit([Duration|DurationTail],NewAcc,[Weekend|OnWeekendsTail],0,Result).
findUpperTimeLimit([Duration|DurationTail],Acc,[Weekend|OnWeekendsTail],StartingDay,Result):-
    Weekend = 0,
    Duration < 6,
    NewAcc is Acc + Duration,
    5 - StartingDay >= Duration,
    NewStartingDay is (StartingDay + Duration) mod 7,
    findUpperTimeLimit(DurationTail,NewAcc,OnWeekendsTail,NewStartingDay,Result).
findUpperTimeLimit([Duration|DurationTail],Acc,[Weekend|OnWeekendsTail],StartingDay,Result):-
    Weekend = 1,
    NewAcc is Acc + Duration,
    NewStartingDay is (StartingDay + Duration) mod 7,
    findUpperTimeLimit(DurationTail,NewAcc,OnWeekendsTail,NewStartingDay,Result).

% Make the start term the correct length and structure
initStart(Start,NbOfPersons) :-
    length(StartList,NbOfPersons),
    array_list(Start,StartList).

% Set the precede constraints
setPrecConstraints(Precs,Start):-
    array_list(Precs,PrecList),
    (foreach([](A,B),PrecList),param(Start) do
        Start[A] #< Start[B]).
        
maxNbOfViols(NbOfPersons,Max) :-
    N1 is NbOfPersons - 1,
Max is NbOfPersons * N1 // 2 + 1.
% Calculate (the maximum number of violations) + 1
maxNbOfViols(Ranks,I,Acc,Max) :-
    length(Ranks,Len),
    Len >= I,
    nth_value(Ranks,I,Own),
    writeln(Own),
    writeln(I),
    writeln(Ranks),
    getNbLarger(Ranks,Own,0,Nb),
    NewI is I + 1,
    NewAcc is Acc + Nb,
    maxNbOfViols(Ranks,NewI,NewAcc,Max).
    
maxNbOfViols(Ranks,I,Acc,Acc) :-
    length(Ranks,Len),
    Len < I.
    
getNbLarger([],_,Acc,Acc).
getNbLarger([L|Tail],E,Acc,Nb):-
    L > E,
    NewAcc is Acc + 1,
    getNbLarger(Tail,E,NewAcc,Nb).
getNbLarger([L|Tail],E,Acc,Nb):-
    L =< E,
    getNbLarger(Tail,E,Acc,Nb).



% Get the correct number of violations for the given ranks and start times
nbOfViols(Rank,Start,NbOfPersons,NbOfViols):-
    (multifor([I,J],1,NbOfPersons),foreach(L,List), param(Rank,Start) do 
        (Rank[I] < Rank[J] ->
            #>(Start[I],Start[J],L)
        ;
            L = 0
        )
    ),
    NbOfViols #= sum(List).
  

% Set the weekend constraint for one start and duration
setWeekendConstrait(Start,Duration,StartingDay,0):-
    WeekDay :: 0 .. 6,
    WeekDay #= Start + StartingDay - _ * 7,
    5 - WeekDay #>= Duration.
setWeekendConstrait(_,_,_,1).

% Set all weekend constraints
setWeekendConstraints(StartingDay,Starts,Durations,OnWeekends):-
    array_list(Starts,StartsList),
    array_list(Durations,DurationsList),
    array_list(OnWeekends,OnWeekendsList),
    (foreach(Start,StartsList),foreach(Duration,DurationsList),foreach(OnWeekend,OnWeekendsList), param(StartingDay) do
        setWeekendConstrait(Start,Duration,StartingDay,OnWeekend)
    ).

setSymmetryBreakingConstraints(Durations,Start,Ranks,Precs,OnWeekend):-
    arity(Durations,Len),
    array_list(Precs,PrecList),
    (for(J,1,Len), param(Durations,Start,PrecList,OnWeekend,Ranks)do setSymmetryBreakingConstraint(Durations,Start,Ranks,PrecList,OnWeekend,J)).
setSymmetryBreakingConstraint(Durations,Start,Ranks,Precs,OnWeekend,Element):-
    \+(member([](_,Element),Precs);member([](Element,_),Precs)),
    arity(Durations,Len),
    St is Element + 1,
    (for(I,St,Len), param(Start,Ranks,OnWeekend,Element,Durations) do
        (Ranks[Element] =:= Ranks[I], Durations[Element] =:= Durations[I],OnWeekend[Element] =:= OnWeekend[I]->
            Start[Element] #< Start[I],
            writeln("Set symmetry breaking constraint!")
            ;
            true
        )
    ).
setSymmetryBreakingConstraint(_,_,_,Precs,_,Element):-
    member([](_,Element),Precs);member([](Element,_),Precs).

getWeekends(StartingDay,LastDay,Acc,Weekends):-
    Acc = [LastWeekend|_],
    NewWeekend is LastWeekend + 7,
    NewWeekend =< LastDay,
    NewAcc = [NewWeekend|Acc],
    getWeekends(StartingDay,LastDay,NewAcc,Weekends).
    
getWeekends(_,LastDay,Acc,Acc):-
    Acc = [LastWeekend|_],
    LastWeekend + 7 > LastDay.
        
    
% Self-made disjunctive constraint
self_disjunctive(Start,Duration):-
    arity(Start,NbOfPersons),
    (for(I,1,NbOfPersons), param(Start,Duration,NbOfPersons) do
        I1 is I + 1,
        (for(J,I1,NbOfPersons), param(I,Start,Duration) do
            or(Start[I] #>= Start[J] + Duration[J],Start[J] #>= Start[I] + Duration[I])
        )
    ).
    