:-[planning].
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Some examples to test your programs with
% not to be included in the benchmarks
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    
test1(Start,EndTime,Viol) :-
	     NbofPersons = 5,
             Durations = [](1,2,3,4,5),
	     OnWeekend = [](0,0,0,0,0),
	     Precs= []( [](1,3)), 
	     StartingDay = 5,
	     Rank = [](1,2,3,4,5),  
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).
/*Start = [](2, 9, 11, 3, 16)
EndTime = 21
Viol = 2
*/

test1b(Start,EndTime , Viol) :-
	     NbofPersons = 5,
             Durations = [](7,2,3,4,5),
	     OnWeekend = [](1,0,0,0,0),
	     Precs= []( [](1,3)), 
	     StartingDay = 5,
	     Rank = [](1,2,3,4,5), 
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).
/*Start = [](0, 9, 11, 16, 23)
EndTime = 28
Viol = 0
*/

test1c(Start,EndTime , Viol) :-
	     NbofPersons = 5,
             Durations = [](7,2,3,4,5),
	     OnWeekend = [](0,0,0,0,0),
	     Precs= []( [](1,3)), 
	     StartingDay = 5,
	     Rank = [](1,2,3,4,5), 
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).
/* fails */


test2(Start,EndTime , Viol) :-
	     NbofPersons = 5,
             Durations = [](1,2,3,4,5),
	     OnWeekend = [](1,1,1,1,1),
	     Precs= []( [](3,1)),
	     StartingDay = 5,
	     Rank = [](1,2,3,4,5),  
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).
/*
Start = [](3, 4, 0, 6, 10)
EndTime = 15
Viol = 2
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Benchmarks
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
benchmarkAll(ProblemPreds,Times):-
    test2(_,_,_),
    (foreach(ProblemPred,ProblemPreds),foreach(Time,Times) do
        benchmark(ProblemPred,Time)
    ).
        
benchmarks([bench1a,bench1b,bench1c,bench2a,bench2b,bench2c,bench3a,bench3b,bench3c,bench3d,bench3e,bench3f,bench3g]).
benchmark(ProblemPred,Time):-
    statistics(hr_time,T1),
    call(ProblemPred,_,_,_),
    statistics(hr_time,T2),
    Time is T2 - T1.
   
    
bench1a(Start,EndTime , Viol) :-
	     NbofPersons = 10,
             Durations = [](1,2,3,4,5,1,1,1,1,1),
	     OnWeekend = [](0,0,0,0,0,0,0,0,0,0),
	     Precs= []( [](1,3),[](3,7), [](7,9)),
	     StartingDay = 6,
	     Rank = [](1,2,3,4,1,2,3,4,1,5), 
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).


bench1b(Start,EndTime , Viol) :-
	     NbofPersons = 10,
             Durations = [](1,2,3,4,5,1,1,1,1,1),
	     OnWeekend = [](1,0,1,0,1,0,1,0,1,0),
	     Precs= []( [](1,3),[](3,7), [](7,9)),
	     StartingDay = 6,
	     Rank = [](1,2,3,4,1,2,3,4,1,5),  
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).

bench1c(Start,EndTime , Viol) :-
	     NbofPersons = 10,
             Durations = [](1,2,3,4,5,1,1,1,1,1),
	     OnWeekend = [](1,1,1,1,1,1,1,1,1,1),
	     Precs= []( [](1,3),[](3,7), [](7,9)),
	     StartingDay = 6,
	     Rank = [](1,2,3,4,1,2,3,4,1,5),  
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).


bench2a(Start,EndTime , Viol) :-
	     NbofPersons = 15,
             Durations = [](1,2,3,4,5,1,1,1,1,1, 1,2,3,4,5  ),
	     OnWeekend = [](0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0  ),
	     Precs= []( [](1,3),[](3,7), [](7,9),   [](9, 12), [](12,13), [](13,14)   ),
	     StartingDay = 6,
	     Rank = [](1,2,3,4,1,2,3,4,1,4, 2,3,4,1,5),  
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).


bench2b(Start,EndTime , Viol) :-
	     NbofPersons = 15,
             Durations = [](1,2,3,4,5,1,1,1,1,1, 1,2,3,4,5  ),
	     OnWeekend = [](1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1  ),
	     Precs= []( [](1,3),[](3,7), [](7,9),   [](9, 12), [](12,13), [](13,14)   ),
	     StartingDay = 6,
	     Rank = [](1,2,3,4,1,2,3,4,1,4, 2,3,4,1,5    ),  
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).


bench2c(Start,EndTime , Viol) :-
	     NbofPersons = 15,
             Durations = [](1,2,3,4,5,1,1,1,1,1, 1,2,3,4,5  ),
	     OnWeekend = [](0,0,0,0,0,1,1,1,1,1, 1,1,1,1,1  ),
	     Precs= []( [](1,3),[](3,7), [](7,9),   [](9, 12), [](12,13), [](13,14)   ),
	     StartingDay = 6,
	     Rank = [](1,2,3,4,1,2,3,4,1,4, 2,3,4,1,5    ),  
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).

bench3a(Start,EndTime , Viol) :-
	     NbofPersons = 10,
             Durations = [](1,1,1,1,1,1,1,1,1,1 ),
	     OnWeekend = [](0,0,0,0,0,0,0,0,0,0 ),
	     Precs= []( [](1,3),[](3,7)),
	     StartingDay = 2,
	     Rank = [](1,1,1,1,2,2,2,3,3,5),
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).

bench3b(Start,EndTime , Viol) :-
	     NbofPersons = 10,
             Durations = [](1,1,1,1,1,1,1,1,1,1 ),
	     OnWeekend = [](0,0,0,0,0,0,0,0,0,0 ),
	     Precs= []( [](1,9),[](3,8)),
	     StartingDay = 2,
	     Rank = [](2,2,2,3,3,1,1,1,1,5),
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).

bench3c(Start,EndTime , Viol) :-
	     NbofPersons = 10,
             Durations = [](1,1,1,1,1,1,1,1,1,1 ),
	     OnWeekend = [](0,0,0,0,0,0,0,0,0,0 ),
	     Precs= []( [](1,9),[](3,8)),
	     StartingDay = 2,
	     Rank = [](9,8,7,6,5,4,3,2,1,10),
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).
bench3d(Start,EndTime , Viol) :-
	     NbofPersons = 12,
             Durations = [](1,1,1,1,1,1,1,1,1,1, 1,1  ),
	     OnWeekend = [](0,0,0,0,0,0,0,0,0,0, 0,0 ),
	     Precs= []( [](1,9),[](3,8)),
	     StartingDay = 2,
	     Rank = [](2,2,2,3,3,1,1,1,1,5, 2,10),
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).
bench3e(Start,EndTime , Viol) :-
	     NbofPersons = 14,
             Durations = [](1,1,1,1,1,1,1,1,1,1, 1,1,1,1  ),
	     OnWeekend = [](0,0,0,0,0,0,0,0,0,0, 0,0,0,0 ),
	     Precs= []( [](1,9),[](3,8)),
	     StartingDay = 2,
	     Rank = [](2,2,2,3,3,1,1,1,1,5, 2,2,3,10),
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).
bench3f(Start,EndTime , Viol) :-
	     NbofPersons = 16,
             Durations = [](1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1  ),
	     OnWeekend = [](0,0,0,0,0,0,0,0,0,0, 0,0,0,0,0,0  ),
	     Precs= []( [](1,9),[](3,8)),
	     StartingDay = 2,
	     Rank = [](2,2,2,3,3,1,1,1,1,5, 2,2,3,3,1,10),
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).
bench3g(Start,EndTime , Viol) :-
	     NbofPersons = 16,
             Durations = [](1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1  ),
	     OnWeekend = [](1,1,1,1,1,1,1,1,1,1, 1,1,1,1,1,1  ),
	     Precs= []( [](1,9),[](3,8)),
	     StartingDay = 2,
	     Rank = [](2,2,2,3,3,1,1,1,1,5, 2,2,3,3,1,10),
	     meeting(NbofPersons, Durations, OnWeekend, Rank, Precs,StartingDay,Start, EndTime , Viol).

