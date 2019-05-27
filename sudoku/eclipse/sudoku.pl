%
% The solving part of the classic viewpoint comes from the examples on the eclipseclp website:
% http://www.eclipseclp.org/examples/sudoku.ecl.txt
% Author: Joachim Schimpf, IC-Parc
%

:- lib(ic).
%:- import alldifferent/1 from ic_global.
%:- set_flag(coroutine,on).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% RUNTIME BENCHMARKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
benchmarkAll(_,[],[],[]).

benchmarkAll(Method,[Prob|Tail],Times, BackTracks):-
    Method = classic,
    benchmarkClassic(Prob,Time),
    benchmarkAll(Method,Tail,SmallerTimes, SmallerBackTracks),
    backtrackClassic(Prob,BT),
    Times = [Time|SmallerTimes],
    BackTracks = [BT|SmallerBackTracks].
    
benchmarkAll(Method,[Prob|Tail],Times, BackTracks):-
    Method = new,
    benchmarkNew(Prob,Time),
    benchmarkAll(Method,Tail,SmallerTimes, SmallerBackTracks),
    backtrackNew(Prob,BT),
    Times = [Time|SmallerTimes],
    BackTracks = [BT|SmallerBackTracks]. 
        
benchmarkAll(Method,[Prob|Tail],Times, BackTracks):-
    Method = both,
    benchmarkBoth(Prob,Time),
    benchmarkAll(Method,Tail,SmallerTimes, SmallerBackTracks),
    backtrackBoth(Prob,BT),
    Times = [Time|SmallerTimes],
    BackTracks = [BT|SmallerBackTracks].
    
benchmarkClassic(ProblemPred,Time):-
    call(ProblemPred,P),
    statistics(runtime,_),
    solveClassic(P),
    statistics(runtime,[_,Time]).
    
benchmarkNew(ProblemPred,Time):-
    call(ProblemPred,P),
    statistics(runtime,_),
    solveNew(P,_),
    statistics(runtime,[_,Time]).
    
benchmarkBoth(ProblemPred,Time):-
    call(ProblemPred,P),
    statistics(runtime,_),
    solveBoth(P),
    statistics(runtime,[_,Time]).
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% BACKTRACK BENCHMARKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
backtrackClassic(ProblemPred,BT):-
    call(ProblemPred,Board),
	setClassicConstraints(Board),
    search(Board,0,input_order,indomain,complete,[backtrack(BT)]).

backtrackNew(ProblemPred,BT) :-
    call(ProblemPred,Xs),
	setNewConstraints(Xs,_),
    search(Xs,0,input_order,indomain,complete,[backtrack(BT)]).
    
backtrackBoth(ProblemPred,BT) :-
    call(ProblemPred,Board),
    setClassicConstraints(Board),
    channeling(Board,Xs),
    setNewConstraints(Xs,_),
    search(Board,0,input_order,indomain,complete,[backtrack(BT)]).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOLVERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solveClassic(Board) :-
    setClassicConstraints(Board),
    search(Board,0,input_order,indomain,complete,[]).
    
solveNew(Xs) :-
    setNewConstraints(Xs),
    search(Xs,0,input_order,indomain,complete,[]).

solveBoth(Board) :-
    setClassicConstraints(Board),
    channeling(Board,Xs),
    setNewConstraints(Xs,_),
    search(Board,0,input_order,indomain,complete,[]).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setClassicConstraints(Board) :-
	dim(Board, [N,N]),
	Board :: 1..N,
	( for(I,1,N), param(Board) do
	    alldifferent(Board[I,*]),
	    alldifferent(Board[*,I])
	),
	NN is integer(sqrt(N)),
	( multifor([I,J],1,N,NN), param(Board,NN) do
	    alldifferent(concat(Board[I..I+NN-1, J..J+NN-1]))
	).

setNewConstraints(Xs) :-
    dim(Xs, [N,N]),
    Xs :: 1..N,
    NN is integer(sqrt(N)),
    dim(Ys, [N,N]),
    Ys :: 0..NN - 1,
    (for(I,1,N), param(Xs) do
        alldifferent(Xs[*,I]), % all numbers in a row have a different col.
        alldifferent(Xs[I,*])  % no 2 rows have the same number at the same column
    ),
	( multifor([I,J],1,N,1), param(Ys,Xs,NN) do
        Xs[I,J] - NN * Ys[I,J] #=< 3,
        Xs[I,J] - NN * Ys[I,J] #>= 1
        
	),
    (for(I,1,N), param(Ys,NN,N) do
        (for(J,1,N - NN + 1,NN), param(Ys,NN, I) do
                alldifferent(Ys[I,J..J + NN - 1])
        )
    ).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHANNELING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

channeling(Board,Xs):-
    (var(Board) ->
        dim(Xs, [N,N]),
        dim(Board,[N,N])
    ;
        dim(Board,[N,N]),
        dim(Xs, [N,N])       
    ),
    (multifor([I,J,K],1,N), param(Board,Xs) do
        #=(Board[I,J],K,B),
        #=(Xs[K,I],J,B)
    ).
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

printBoard(Board) :-
	dim(Board, [N,N]),
	( for(I,1,N), param(Board,N) do
	    ( for(J,1,N), param(Board,I) do
		X is Board[I,J],
		( var(X) -> write("  _") ; printf(" %2d", [X]) )
	    ), nl
	), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLE PUZZLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


lambda(P) :- P = [](
         [](1,_,_, _,_,_, _,_,_),
         [](_,_,2, 7,4,_, _,_,_),
         [](_,_,_, 5,_,_, _,_,4),

         [](_,3,_, _,_,_, _,_,_),
         [](7,5,_, _,_,_, _,_,_),
         [](_,_,_, _,_,9, 6,_,_),

         [](_,4,_, _,_,6, _,_,_),
         [](_,_,_, _,_,_, _,7,1),
         [](_,_,_, _,_,1, _,3,_)).


hard17(P) :- P = [](
	[](_,_,2,_,9,_,3,_,_),
	[](8,_,5,_,_,_,_,_,_),
	[](1,_,_,_,_,_,_,_,_),
	[](_,9,_,_,6,_,_,4,_),
	[](_,_,_,_,_,_,_,5,8),
	[](_,_,_,_,_,_,_,_,1),	
	[](_,7,_,_,_,_,2,_,_),
	[](3,_,_,5,_,_,_,_,_),
	[](_,_,_,1,_,_,_,_,_)).

eastermonster(P) :- P = [](
    [](1,_,_,_,_,_,_,_,2),
    [](_,9,_,4,_,_,_,5,_),
    [](_,_,6,_,_,_,7,_,_),
    [](_,5,_,9,_,3,_,_,_),
    [](_,_,_,_,7,_,_,_,_),
    [](_,_,_,8,5,_,_,4,_),
    [](7,_,_,_,_,_,6,_,_),
    [](_,3,_,_,_,9,_,8,_),
    [](_,_,2,_,_,_,_,_,1)).


tarek_052(P) :- P = [](
    [](_,_,1,_,_,4,_,_,_),
    [](_,_,_,_,6,_,3,_,5),
    [](_,_,_,9,_,_,_,_,_),
    [](8,_,_,_,_,_,7,_,3),
    [](_,_,_,_,_,_,_,2,8),
    [](5,_,_,_,7,_,6,_,_),
    [](3,_,_,_,8,_,_,_,6),
    [](_,_,9,2,_,_,_,_,_),
    [](_,4,_,_,_,1,_,_,_)).

goldennugget(P) :- P = [](
    [](_,_,_,_,_,_,_,3,9),
    [](_,_,_,_,_,1,_,_,5),
    [](_,_,3,_,5,_,8,_,_),
    [](_,_,8,_,9,_,_,_,6),
    [](_,7,_,_,_,2,_,_,_),
    [](1,_,_,4,_,_,_,_,_),
    [](_,_,9,_,8,_,_,5,_),
    [](_,2,_,_,_,_,6,_,_),
    [](4,_,_,7,_,_,_,_,_)).

coloin(P) :- P = [](
    [](_,2,_,4,_,3,7,_,_),
    [](_,_,_,_,_,_,_,3,2),
    [](_,_,_,_,_,_,_,_,4),
    [](_,4,_,2,_,_,_,7,_),
    [](8,_,_,_,5,_,_,_,_),
    [](_,_,_,_,_,1,_,_,_),
    [](5,_,_,_,_,_,9,_,_),
    [](_,3,_,9,_,_,_,_,7),
    [](_,_,1,_,_,8,6,_,_)).


extra2(P) :- P = [](
    [](_,_,1,_,2,_,7,_,_),
    [](_,5,_,_,_,_,_,9,_),
    [](_,_,_,4,_,_,_,_,_),
    [](_,8,_,_,_,5,_,_,_),
    [](_,9,_,_,_,_,_,_,_),
    [](_,_,_,_,6,_,_,_,2),
    [](_,_,2,_,_,_,_,_,_),
    [](_,_,6,_,_,_,_,_,5),
    [](_,_,_,_,_,9,_,8,3)).


extra3(P) :-  P = [](
    [](1,_,_,_,_,_,_,_,_),
    [](_,_,2,7,4,_,_,_,_),
    [](_,_,_,5,_,_,_,_,4),
    [](_,3,_,_,_,_,_,_,_),
    [](7,5,_,_,_,_,_,_,_),
    [](_,_,_,_,_,9,6,_,_),
    [](_,4,_,_,_,6,_,_,_),
    [](_,_,_,_,_,_,_,7,1),
    [](_,_,_,_,_,1,_,3,_)).

% extra4 = extra3 + 1  hint different
extra4(P) :- P = [](
    [](1,_,4,_,_,_,_,_,_),
    [](_,_,2,7,4,_,_,_,_),
    [](_,_,_,5,_,_,_,_,_),
    [](_,3,_,_,_,_,_,_,_),
    [](7,5,_,_,_,_,_,_,_),
    [](_,_,_,_,_,9,6,_,_),
    [](_,4,_,_,_,6,_,_,_),
    [](_,_,_,_,_,_,_,7,1),
    [](_,_,_,_,_,1,_,3,_)).


% inkara2012
% from: http://www.telegraph.co.uk/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html
inkara2012(P) :- P = [](
    [](8,_,_,_,_,_,_,_,_),
    [](_,_,3,6,_,_,_,_,_),
    [](_,7,_,_,9,_,2,_,_),
    [](_,5,_,_,_,7,_,_,_),
    [](_,_,_,_,4,5,7,_,_),
    [](_,_,_,1,_,_,_,3,_),
    [](_,_,1,_,_,_,_,6,8),
    [](_,_,8,5,_,_,_,1,_),
    [](_,9,_,_,_,_,4,_,_)).


clue18(P) :- P = [](
    [](7,_,8,_,_,_,3,_,_),
    [](_,_,_,2,_,1,_,_,_),
    [](5,_,_,_,_,_,_,_,_),
    [](_,4,_,_,_,_,_,2,6),
    [](3,_,_,_,8,_,_,_,_),
    [](_,_,_,1,_,_,_,9,_),
    [](_,9,_,6,_,_,_,_,4),
    [](_,_,_,_,7,_,5,_,_),
    [](_,_,_,_,_,_,_,_,_)).

% from:
% http://school.maths.uwa.edu.au/~gordon/sudokumin.php

clue17(P) :- P = [](
    [](_,_,_,_,_,_,_,1,_),
    [](4,_,_,_,_,_,_,_,_),
    [](_,2,_,_,_,_,_,_,_),
    [](_,_,_,_,5,_,4,_,7),
    [](_,_,8,_,_,_,3,_,_),
    [](_,_,1,_,9,_,_,_,_),
    [](3,_,_,4,_,_,2,_,_),
    [](_,5,_,1,_,_,_,_,_),
    [](_,_,_,8,_,6,_,_,_)).

%http://www.sudokuwiki.org/Weekly_Sudoku.asp?puz=28
sudowiki_nb28(P) :- P = [](
    [](6,_,_,_,_,8,9,4,_),
    [](9,_,_,_,_,6,1,_,_),
    [](_,7,_,_,4,_,_,_,_),
    [](2,_,_,6,1,_,_,_,_),
    [](_,_,_,_,_,_,2,_,_),
    [](_,8,9,_,_,2,_,_,_),
    [](_,_,_,_,6,_,_,_,5),
    [](_,_,_,_,_,_,_,3,_),
    [](8,_,_,_,_,1,6,_,_)).

sudowiki_nb49(P) :- P = [](
    [](_,_,2,8,_,_,_,_,_),
    [](_,3,_,_,6,_,_,_,7),
    [](1,_,_,_,_,_,_,4,_),
    [](6,_,_,_,9,_,_,_,_),
    [](_,5,_,6,_,_,_,_,9),
    [](_,_,_,_,5,7,_,6,_),
    [](_,_,_,3,_,_,1,_,_),
    [](_,7,_,_,_,6,_,_,8),
    [](4,_,_,_,_,_,_,2,_)).
   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLE PUZZLES (2ND VIEWPOINT)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
alt_lambda(P) :- P = [](
    [](1,_,_,_,_,_,_,9,6),
    [](_,3,_,_,_,_,_,_,_),
    [](_,_,_,2,_,_,_,_,8),
    [](_,5,9,_,_,_,2,_,_),
    [](_,_,4,_,2,_,_,_,_),
    [](_,_,_,_,_,7,6,_,_),
    [](_,4,_,_,1,_,_,8,_),
    [](_,_,_,_,_,_,_,_,_),
    [](_,_,_,_,_,6,_,_,_)).
    
alt_hard17(P) :- P = [](
    [](_,_,1,_,_,9,_,_,4),
    [](3,_,_,_,_,_,7,_,_),
    [](7,_,_,_,_,_,_,1,_),
    [](_,_,_,8,_,_,_,_,_),
    [](_,3,_,_,8,_,_,4,_),
    [](_,_,_,5,_,_,_,_,_),
    [](_,_,_,_,_,_,2,_,_),
    [](_,1,_,_,9,_,_,_,_),
    [](5,_,_,2,_,_,_,_,_)).

alt_eastermonster(P) :- P = [](
    [](1,_,_,_,_,_,_,_,9),
    [](9,_,_,_,_,_,_,_,3),
    [](_,_,_,6,_,_,_,2,_),
    [](_,4,_,_,_,8,_,_,_),
    [](_,8,_,2,_,5,_,_,_),
    [](_,_,3,_,_,_,7,_,_),
    [](_,_,7,_,5,_,1,_,_),
    [](_,_,_,_,_,4,_,8,_),
    [](_,2,_,4,_,_,_,6,_)).
    

alt_tarek_052(P) :- P = [](
    [](3,_,_,_,_,_,_,_,6),
    [](_,_,_,_,8,_,_,4,_),
    [](_,7,_,9,_,_,1,_,_),
    [](6,_,_,_,_,_,_,_,2),
    [](_,9,_,_,_,1,_,_,_),
    [](_,5,_,_,_,7,9,_,_),
    [](_,_,_,7,_,5,_,_,_),
    [](_,_,_,1,9,_,5,_,_),
    [](_,_,4,_,_,_,_,3,_)).
    
alt_goldennugget(P) :- P = [](
    [](_,6,_,_,_,1,_,_,_),
    [](_,_,_,_,6,_,_,2,_),
    [](8,_,3,_,_,_,_,_,_),
    [](_,_,_,_,_,4,_,_,1),
    [](_,9,5,_,_,_,8,_,_),
    [](_,_,_,9,_,_,_,7,_),
    [](_,_,_,_,2,_,_,_,4),
    [](_,_,7,3,_,_,5,_,_),
    [](9,_,_,5,_,_,3,_,_)).

alt_coloin(P) :- P = [](
    [](_,_,_,_,_,6,_,_,3),
    [](2,9,_,4,_,_,_,_,_),
    [](6,8,_,_,_,_,_,2,_),
    [](4,_,9,2,_,_,_,_,_),
    [](_,_,_,_,5,_,1,_,_),
    [](_,_,_,_,_,_,_,_,7),
    [](7,_,_,8,_,_,_,9,_),
    [](_,_,_,_,1,_,_,_,6),
    [](_,_,_,_,_,_,7,4,_)).
    
alt_extra2(P) :- P = [](
    [](3,_,_,_,_,_,_,_,_),
    [](5,_,_,_,_,9,3,_,_),
    [](_,_,_,_,_,_,_,_,9),
    [](_,_,4,_,_,_,_,_,_),
    [](_,2,_,6,_,_,_,9,_),
    [](_,_,_,_,_,5,_,3,_),
    [](7,_,_,_,_,_,_,_,_),
    [](_,_,_,2,_,_,_,_,8),
    [](_,8,_,_,2,_,_,_,6)).
        
alt_extra3(P) :- P = [](
    [](1,_,_,_,_,_,_,9,6),
    [](_,3,_,_,_,_,_,_,_),
    [](_,_,_,2,_,_,_,_,8),
    [](_,5,9,_,_,_,2,_,_),
    [](_,_,4,_,2,_,_,_,_),
    [](_,_,_,_,_,7,6,_,_),
    [](_,4,_,_,1,_,_,8,_),
    [](_,_,_,_,_,_,_,_,_),
    [](_,_,_,_,_,6,_,_,_)).
    
alt_extra4(P) :- P = [](
    [](1,_,_,_,_,_,_,9,6),
    [](_,3,_,_,_,_,_,_,_),
    [](_,_,_,2,_,_,_,_,8),
    [](3,5,_,_,_,_,2,_,_),
    [](_,_,4,_,2,_,_,_,_),
    [](_,_,_,_,_,7,6,_,_),
    [](_,4,_,_,1,_,_,8,_),
    [](_,_,_,_,_,_,_,_,_),
    [](_,_,_,_,_,6,_,_,_)).
    
alt_inkara2012(P) :- P = [](
    [](_,_,_,_,_,4,3,8,_),
    [](_,_,7,_,_,_,_,_,_),
    [](_,3,_,_,_,8,_,_,_),
    [](_,_,_,_,5,_,_,_,7),
    [](_,_,_,2,6,_,_,4,_),
    [](_,4,_,_,_,_,8,_,_),
    [](_,_,2,6,7,_,_,_,_),
    [](1,_,_,_,_,_,9,3,_),
    [](_,_,5,_,_,_,_,_,2)).
    
alt_clue18(P) :- P = [](
    [](_,6,_,_,_,4,_,_,_),
    [](_,4,_,8,_,_,_,_,_),
    [](7,_,_,_,1,_,_,_,_),
    [](_,_,_,2,_,_,9,_,_),
    [](_,_,1,_,_,_,_,7,_),
    [](_,_,_,9,_,_,4,_,_),
    [](1,_,_,_,_,_,_,5,_),
    [](3,_,_,_,5,_,_,_,_),
    [](_,_,_,_,_,8,2,_,_)).
    
alt_clue17(P) :- P = [](
    [](8,_,_,_,_,3,_,4,_),
    [](_,_,2,_,_,_,7,_,_),
    [](_,_,_,_,7,_,1,_,_),
    [](_,1,_,7,_,_,4,_,_),
    [](_,_,_,5,_,_,_,2,_),
    [](_,_,_,_,_,_,_,_,6),
    [](_,_,_,9,_,_,_,_,_),
    [](_,_,_,_,3,_,_,_,4),
    [](_,_,_,_,_,5,_,_,_)).
    
alt_sudowiki_nb28(P) :- P = [](
    [](_,7,_,5,_,_,_,_,6),
    [](_,_,_,1,7,6,_,_,_),
    [](_,_,_,_,_,_,_,8,_),
    [](8,_,5,_,_,_,_,_,_),
    [](_,_,_,_,_,_,9,_,_),
    [](1,6,_,4,_,_,5,_,7),
    [](_,_,2,_,_,_,_,_,_),
    [](6,_,_,_,_,2,_,_,1),
    [](7,1,_,_,_,3,_,_,_)).
    
alt_sudowiki_nb49(P) :- P = [](
    [](_,_,1,_,_,_,7,_,_),
    [](3,_,_,_,_,_,_,_,8),
    [](_,2,_,_,_,_,4,_,_),
    [](_,_,8,_,_,_,_,_,1),
    [](_,_,_,_,2,5,_,_,_),
    [](_,5,_,1,4,8,_,6,_),
    [](_,9,_,_,_,6,_,2,_),
    [](4,_,_,_,_,_,_,9,_),
    [](_,_,_,5,9,_,_,_,_)).

