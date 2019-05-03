:- use_module(library(chr)).
:- op( 700,xfx,in).
:- op( 700,xfx,ne).
:- op( 700,xfx,eq).
:- chr_constraint in/2, ne/2,enum/1, indomain/1, inBox/3, channelToXs/4, channelToBoard/4.
:- set_prolog_flag(chr_toplevel_show_store, false).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHR CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The not equals constraint.
X ne Y <=> nonvar(X),nonvar(Y) | X \== Y.
inBox(X,Y,NN) <=> nonvar(X) | Y is (X - 1) // NN + 1.

_ in [] <=> fail.
X in [Y] <=> X = Y.
enum([]) <=> true.
X in L <=> nonvar(X) | memberchk(X,L).
X in L, X ne Y <=> nonvar(Y) |delete(L,Y,L_New), X in L_New.
X in L, Y ne X <=> nonvar(Y) |delete(L,Y,L_New), X in L_New.
X in L, X in F <=> intersection(L,F,L_New), X in L_New.


enum([X|L]) <=> indomain(X), enum(L).

indomain(X) <=> nonvar(X) | true.
indomain(X) , X in [V|L] <=> L = [_|_] | (X in [V]; X in L, indomain(X)).



inBox(X,Y,NN) <=> nonvar(Y) | Min is (Y-1)*NN + 1, Max is (Y-1)*NN + NN, findall(E,between(Min,Max,E),Domain),X in Domain.

channelToXs(Xs,X,Y,K) <=> nonvar(K) | get(Xs,Y,K,X).
% Meaning X_IJ = K => Board_IK = J
channelToBoard(Board,I,J,K) <=> nonvar(K) | get(Board,K,I,J).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOLVERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solveClassic(Board) :-
    flatten(Board,FlatBoard),
    length(Board,N),
    findall(E,between(1,N,E),Domain),
    setDomainConstraints(FlatBoard,Domain),
    setClassicConstraints(Board),
    enum(FlatBoard),
    !.
    
solveNew(Xs,Ys):-
    length(Xs,N),
    createTemplate(N,Ys),
    NN is floor(sqrt(N)),
    findall(E,between(1,N,E),Domain),
    flatten(Xs,FlatBoard),   
    setDomainConstraints(FlatBoard,Domain),
    findall(E,between(1,NN,E),YsDomain),
    flatten(Ys,YsFlat),
    setDomainConstraints(YsFlat,YsDomain),
    setRowColallDifferentConstraints(Xs,N),
    setInBoxConstraints(Xs,Ys,N,NN),
    setNewBoxConstraints(Ys,N,NN),
    writeln("enumerating..."),
    enum(FlatBoard),
    !.

solveChanneling(Board,Xs):-
    length(Board,N),
    length(Xs,N),
    NN is floor(sqrt(N)),
    createTemplate(N,Board),
    createTemplate(N,Xs),
    createTemplate(N,Ys),
    setChannelings(Board,Xs),
    flatten(Board,FlatBoard),
    findall(E,between(1,N,E),Domain),
    setDomainConstraints(FlatBoard,Domain),
    setClassicConstraints(Board),
    findall(E,between(1,N,E),Domain),
    flatten(Xs,FlatXs),   
    setDomainConstraints(FlatXs,Domain),
    findall(E,between(1,NN,E),YsDomain),
    flatten(Ys,YsFlat),
    setDomainConstraints(YsFlat,YsDomain),
    setRowColallDifferentConstraints(Xs,N),
    setInBoxConstraints(Xs,Ys,N,NN),
    setNewBoxConstraints(Ys,N,NN),
    enum(FlatBoard).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setClassicConstraints(Board) :-
    length(Board,N),
    setRowColallDifferentConstraints(Board,N),
    setBoxConstraints(Board,N).

setDomainConstraints([],_).
setDomainConstraints([X|Rest],Domain) :-
    X in Domain,
    setDomainConstraints(Rest,Domain).
    
setRowColallDifferentConstraints(Board, N) :-
    for(1,N,1,setColumnConstraint(Board,N)),
    for(1,N,1,setRowConstraint(Board,N)).
        
setColumnConstraint(Board,N,Col):-
    bagof(E,Y^(between(1,N,Y),get(Board,Col,Y,E)),L),
    allDifferent(L).
    
setRowConstraint(Board,N,Row):-
    bagof(E,X^(between(1,N,X),get(Board,X,Row,E)),L),
    allDifferent(L).
    
setBoxConstraints(Board,N):-
    NN is floor(sqrt(N)),
    EndBox is N - NN + 1,
    for(1,EndBox,NN,
        for(1,EndBox,NN,
            setBoxConstraint(Board,NN)
        )
    ).
    
setBoxConstraint(Board,NN,StartRow,StartCol):-
    EndCol is StartCol + NN - 1,
    EndRow is StartRow + NN - 1,
    bagof(E,(X,Y)^(between(StartRow,EndRow,Y),between(StartCol,EndCol,X),get(Board,X,Y,E)),L),
    allDifferent(L).

allDifferent([_]).    
allDifferent([X|Rest]) :-
    allDifferent(Rest,X),
    allDifferent(Rest).
    
allDifferent([],_).
allDifferent([X|Rest],E) :-
    X ne E,
    allDifferent(Rest,E).
    
setNewBoxConstraints(Ys,N,NN):-
    EndBox is N - NN + 1,
    for(1,N,1,
        for(1,EndBox,NN,
            setNewBoxConstraint(Ys,NN)
        )
    ).
        
setNewBoxConstraint(Ys,NN,Row,StartCol):-
    EndCol is StartCol + NN - 1,
    bagof(E,Col^(between(StartCol,EndCol,Col),get(Ys,Col,Row,E)),L),
    allDifferent(L).

setInBoxConstraints(Xs,Ys,N,NN):-
    for(1,N,1,
        for(1,N,1,
            setInBoxConstraint(Xs,Ys,NN)
        )
    ).
    
setInBoxConstraint(Xs,Ys,NN,Row,Col):-
    get(Xs,Col,Row,X),
    get(Ys,Col,Row,Y),
    inBox(X,Y,NN).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHANNELING
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setChannelings(Board,Xs):-
    length(Board,N),
    for(1,N,1,
        for(1,N,1,
            setChanneling(Board,Xs)
        )
    ).

setChanneling(Board,Xs,X,Y):-
    get(Board,X,Y,K),
    channelToXs(Xs,X,Y,K),
    get(Xs,Y,X,E), % X_YX = E
    % Meaning X_IJ = K
    channelToBoard(Board,Y,X,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% HELPER PREDICATES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get(Board,X,Y,E) :-
    nth1(Y,Board,Row),
    nth1(X,Row,E).

createTemplate(N,Out):-
    length(Out,N),
    createRows(N,Out).

createRows(_,[]).
createRows(N,[H|T]):-
    length(H,N),
    createRows(N,T).

printBoard([]).
printBoard([H|T]) :- printList(H), nl, printBoard(T).

printList([]).
printList([H|T]) :-
    (var(H) ->
        write("_ ")
    ;
        write(H),
        write(" ")),
    printList(T).

for(Start,End,Step,Pred):-
    for(Start,End,Step,Pred,[]).
for(Start,End,Step,Pred,Args):-
    is_list(Args),
    Iterator is Start,
    forRec(Iterator,End,Step,Pred,Args),
    !.
for(Start,End,Step,Pred,Arg):-
    \+(is_list(Arg)),
    for(Start,End,Step,Pred,[Arg]).
    
    
for(Start,End,Step,Pred,Args,Arg2):-
    Iterator is Start,
    append(Args,[Arg2],TotArgs),
    forRec(Iterator,End,Step,Pred,TotArgs),
    !.
    
forRec(Iterator,End,Step,_,_):-
    Iterator is End + Step.
forRec(Iterator,End,Step,Pred,Args):-
    Iterator =\= End + Step,
    append(Args,[Iterator],TotArgs),
    apply(Pred,TotArgs),
    NewIndex is Iterator + Step,
    forRec(NewIndex,End,Step,Pred,Args).
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% EXAMPLE PUZZLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

lambda(P) :- P =
        [[1,_,_, _,_,_, _,_,_],
         [_,_,2, 7,4,_, _,_,_],
         [_,_,_, 5,_,_, _,_,4],

         [_,3,_, _,_,_, _,_,_],
         [7,5,_, _,_,_, _,_,_],
         [_,_,_, _,_,9, 6,_,_],

         [_,4,_, _,_,6, _,_,_],
         [_,_,_, _,_,_, _,7,1],
         [_,_,_, _,_,1, _,3,_]].


hard17(P) :- P = 
	[[_,_,2,_,9,_,3,_,_],
	[8,_,5,_,_,_,_,_,_],
	[1,_,_,_,_,_,_,_,_],

	[_,9,_,_,6,_,_,4,_],
	[_,_,_,_,_,_,_,5,8],
	[_,_,_,_,_,_,_,_,1],
	
	[_,7,_,_,_,_,2,_,_],
	[3,_,_,5,_,_,_,_,_],
	[_,_,_,1,_,_,_,_,_]].

eastermonster(P) :- P = 
	[[1,_,_,_,_,_,_,_,2],
	[_,9,_,4,_,_,_,5,_],
	[_,_,6,_,_,_,7,_,_],

	[_,5,_,9,_,3,_,_,_],
	[_,_,_,_,7,_,_,_,_],
	[_,_,_,8,5,_,_,4,_],
	
	[7,_,_,_,_,_,6,_,_],
	[_,3,_,_,_,9,_,8,_],
	[_,_,2,_,_,_,_,_,1]].


tarek_052(P) :- P = 
	[[_,_,1,_,_,4,_,_,_],
	[_,_,_,_,6,_,3,_,5],
	[_,_,_,9,_,_,_,_,_],

	[8,_,_,_,_,_,7,_,3],
	[_,_,_,_,_,_,_,2,8],
	[5,_,_,_,7,_,6,_,_],

	[3,_,_,_,8,_,_,_,6],
	[_,_,9,2,_,_,_,_,_],
	[_,4,_,_,_,1,_,_,_]].

goldennugget(P) :- P = 
	[[_,_,_,_,_,_,_,3,9],
	[_,_,_,_,_,1,_,_,5],
	[_,_,3,_,5,_,8,_,_],

	[_,_,8,_,9,_,_,_,6],
	[_,7,_,_,_,2,_,_,_],
	[1,_,_,4,_,_,_,_,_],
	
	[_,_,9,_,8,_,_,5,_],
	[_,2,_,_,_,_,6,_,_],
	[4,_,_,7,_,_,_,_,_]].

coloin(P) :- P = 
	[[_,2,_,4,_,3,7,_,_],
	[_,_,_,_,_,_,_,3,2],
	[_,_,_,_,_,_,_,_,4],

	[_,4,_,2,_,_,_,7,_],
	[8,_,_,_,5,_,_,_,_],
	[_,_,_,_,_,1,_,_,_],
	
	[5,_,_,_,_,_,9,_,_],
	[_,3,_,9,_,_,_,_,7],
	[_,_,1,_,_,8,6,_,_]].


extra2(P) :- P = [ 
    [_, _, 1, _, 2, _, 7, _, _],
    [_, 5, _, _, _, _, _, 9, _],
    [_, _, _, 4, _, _, _, _, _],
    [_, 8, _, _, _, 5, _, _, _],
    [_, 9, _, _, _, _, _, _, _],
    [_, _, _, _, 6, _, _, _, 2],
    [_, _, 2, _, _, _, _, _, _],
    [_, _, 6, _, _, _, _, _, 5],
    [_, _, _, _, _, 9, _, 8, 3]].


extra3(P) :-  P = [
    [1, _, _, _, _, _, _, _, _],
    [_, _, 2, 7, 4, _, _, _, _],
    [_, _, _, 5, _, _, _, _, 4],
    [_, 3, _, _, _, _, _, _, _],
    [7, 5, _, _, _, _, _, _, _],
    [_, _, _, _, _, 9, 6, _, _],
    [_, 4, _, _, _, 6, _, _, _],
    [_, _, _, _, _, _, _, 7, 1],
    [_, _, _, _, _, 1, _, 3, _]].

% extra4 = extra3 + 1  hint different
extra4(P) :- P = [
    [1, _, 4, _, _, _, _, _, _],
    [_, _, 2, 7, 4, _, _, _, _],
    [_, _, _, 5, _, _, _, _, _],
    [_, 3, _, _, _, _, _, _, _],
    [7, 5, _, _, _, _, _, _, _],
    [_, _, _, _, _, 9, 6, _, _],
    [_, 4, _, _, _, 6, _, _, _],
    [_, _, _, _, _, _, _, 7, 1],
    [_, _, _, _, _, 1, _, 3, _]].


% inkara2012
% from: http://www.telegraph.co.uk/science/science-news/9359579/Worlds-hardest-sudoku-can-you-crack-it.html
inkara2012(P) :- P = [
	[8,_,_,   _,_,_,   _,_,_],
	[_,_,3,   6,_,_,   _,_,_],
	[_,7,_,   _,9,_,   2,_,_],

	[_,5,_,   _,_,7,   _,_,_],
	[_,_,_,   _,4,5,   7,_,_],
	[_,_,_,   1,_,_,   _,3,_],
	
	[_,_,1,   _,_,_,   _,6,8],
	[_,_,8,   5,_,_,   _,1,_],
	[_,9,_,   _,_,_,   4,_,_]].


clue18(P) :- P = [
	[7,_,8,   _,_,_,   3,_,_],
	[_,_,_,   2,_,1,   _,_,_],
	[5,_,_,   _,_,_,   _,_,_],

	[_,4,_,   _,_,_,   _,2,6],
	[3,_,_,   _,8,_,   _,_,_],
	[_,_,_,   1,_,_,   _,9,_],
	
	[_,9,_,   6,_,_,   _,_,4],
	[_,_,_,   _,7,_,   5,_,_],
	[_,_,_,   _,_,_,   _,_,_]].

% from:
% http://school.maths.uwa.edu.au/~gordon/sudokumin.php

clue17(P) :- P = [
	[_,_,_,   _,_,_,   _,1,_],
	[4,_,_,   _,_,_,   _,_,_],
	[_,2,_,   _,_,_,   _,_,_],

	[_,_,_,   _,5,_,   4,_,7],
	[_,_,8,   _,_,_,   3,_,_],
	[_,_,1,   _,9,_,   _,_,_],
	
	[3,_,_,   4,_,_,   2,_,_],
	[_,5,_,   1,_,_,   _,_,_],
	[_,_,_,   8,_,6,   _,_,_]].

%http://www.sudokuwiki.org/Weekly_Sudoku.asp?puz=28
sudowiki_nb28(P) :- P = [
	[6,_,_,_,_,8,9,4,_],
	[9,_,_,_,_,6,1,_,_],
	[_,7,_,_,4,_,_,_,_],

	[2,_,_,6,1,_,_,_,_],
	[_,_,_,_,_,_,2,_,_],
	[_,8,9,_,_,2,_,_,_],
	
	[_,_,_,_,6,_,_,_,5],
	[_,_,_,_,_,_,_,3,_],
	[8,_,_,_,_,1,6,_,_]].

sudowiki_nb49(P) :- P = [
	[_,_,2,8,_,_,_,_,_],
	[_,3,_,_,6,_,_,_,7],
	[1,_,_,_,_,_,_,4,_],

	[6,_,_,_,9,_,_,_,_],
	[_,5,_,6,_,_,_,_,9],
	[_,_,_,_,5,7,_,6,_],
	
	[_,_,_,3,_,_,1,_,_],
	[_,7,_,_,_,6,_,_,8],
	[4,_,_,_,_,_,_,2,_]].
    
alt_lambda(P) :- P = [
    [1,_,_,_,_,_,_,9,6],
    [_,3,_,_,_,_,_,_,_],
    [_,_,_,2,_,_,_,_,8],
    [_,5,9,_,_,_,2,_,_],
    [_,_,4,_,2,_,_,_,_],
    [_,_,_,_,_,7,6,_,_],
    [_,4,_,_,1,_,_,8,_],
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,6,_,_,_]].
    
alt_hard17(P) :- P = [
    [_,_,1,_,_,9,_,_,4],
    [3,_,_,_,_,_,7,_,_],
    [7,_,_,_,_,_,_,1,_],
    [_,_,_,8,_,_,_,_,_],
    [_,3,_,_,8,_,_,4,_],
    [_,_,_,5,_,_,_,_,_],
    [_,_,_,_,_,_,2,_,_],
    [_,1,_,_,9,_,_,_,_],
    [5,_,_,2,_,_,_,_,_]].

alt_eastermonster(P) :- P = [
    [1,_,_,_,_,_,_,_,9],
    [9,_,_,_,_,_,_,_,3],
    [_,_,_,6,_,_,_,2,_],
    [_,4,_,_,_,8,_,_,_],
    [_,8,_,2,_,5,_,_,_],
    [_,_,3,_,_,_,7,_,_],
    [_,_,7,_,5,_,1,_,_],
    [_,_,_,_,_,4,_,8,_],
    [_,2,_,4,_,_,_,6,_]].
    

alt_tarek_052(P) :- P = [
    [3,_,_,_,_,_,_,_,6],
    [_,_,_,_,8,_,_,4,_],
    [_,7,_,9,_,_,1,_,_],
    [6,_,_,_,_,_,_,_,2],
    [_,9,_,_,_,1,_,_,_],
    [_,5,_,_,_,7,9,_,_],
    [_,_,_,7,_,5,_,_,_],
    [_,_,_,1,9,_,5,_,_],
    [_,_,4,_,_,_,_,3,_]].
    
alt_goldennugget(P) :- P = [
    [_,6,_,_,_,1,_,_,_],
    [_,_,_,_,6,_,_,2,_],
    [8,_,3,_,_,_,_,_,_],
    [_,_,_,_,_,4,_,_,1],
    [_,9,5,_,_,_,8,_,_],
    [_,_,_,9,_,_,_,7,_],
    [_,_,_,_,2,_,_,_,4],
    [_,_,7,3,_,_,5,_,_],
    [9,_,_,5,_,_,3,_,_]].

alt_coloin(P) :- P = [
    [_,_,_,_,_,6,_,_,3],
    [2,9,_,4,_,_,_,_,_],
    [6,8,_,_,_,_,_,2,_],
    [4,_,9,2,_,_,_,_,_],
    [_,_,_,_,5,_,1,_,_],
    [_,_,_,_,_,_,_,_,7],
    [7,_,_,8,_,_,_,9,_],
    [_,_,_,_,1,_,_,_,6],
    [_,_,_,_,_,_,7,4,_]].
    
alt_extra2(P) :- P = [
    [3,_,_,_,_,_,_,_,_],
    [5,_,_,_,_,9,3,_,_],
    [_,_,_,_,_,_,_,_,9],
    [_,_,4,_,_,_,_,_,_],
    [_,2,_,6,_,_,_,9,_],
    [_,_,_,_,_,5,_,3,_],
    [7,_,_,_,_,_,_,_,_],
    [_,_,_,2,_,_,_,_,8],
    [_,8,_,_,2,_,_,_,6]].
        
alt_extra3(P) :- P = [
    [1,_,_,_,_,_,_,9,6],
    [_,3,_,_,_,_,_,_,_],
    [_,_,_,2,_,_,_,_,8],
    [_,5,9,_,_,_,2,_,_],
    [_,_,4,_,2,_,_,_,_],
    [_,_,_,_,_,7,6,_,_],
    [_,4,_,_,1,_,_,8,_],
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,6,_,_,_]].
    
alt_extra4(P) :- P = [
    [1,_,_,_,_,_,_,9,6],
    [_,3,_,_,_,_,_,_,_],
    [_,_,_,2,_,_,_,_,8],
    [3,5,_,_,_,_,2,_,_],
    [_,_,4,_,2,_,_,_,_],
    [_,_,_,_,_,7,6,_,_],
    [_,4,_,_,1,_,_,8,_],
    [_,_,_,_,_,_,_,_,_],
    [_,_,_,_,_,6,_,_,_]].
    
alt_inkara2012(P) :- P = [
    [_,_,_,_,_,4,3,8,_],
    [_,_,7,_,_,_,_,_,_],
    [_,3,_,_,_,8,_,_,_],
    [_,_,_,_,5,_,_,_,7],
    [_,_,_,2,6,_,_,4,_],
    [_,4,_,_,_,_,8,_,_],
    [_,_,2,6,7,_,_,_,_],
    [1,_,_,_,_,_,9,3,_],
    [_,_,5,_,_,_,_,_,2]].
    
alt_clue18(P) :- P = [
    [_,6,_,_,_,4,_,_,_],
    [_,4,_,8,_,_,_,_,_],
    [7,_,_,_,1,_,_,_,_],
    [_,_,_,2,_,_,9,_,_],
    [_,_,1,_,_,_,_,7,_],
    [_,_,_,9,_,_,4,_,_],
    [1,_,_,_,_,_,_,5,_],
    [3,_,_,_,5,_,_,_,_],
    [_,_,_,_,_,8,2,_,_]].
    
alt_clue17(P) :- P = [
    [8,_,_,_,_,3,_,4,_],
    [_,_,2,_,_,_,7,_,_],
    [_,_,_,_,7,_,1,_,_],
    [_,1,_,7,_,_,4,_,_],
    [_,_,_,5,_,_,_,2,_],
    [_,_,_,_,_,_,_,_,6],
    [_,_,_,9,_,_,_,_,_],
    [_,_,_,_,3,_,_,_,4],
    [_,_,_,_,_,5,_,_,_]].
    
alt_sudowiki_nb28(P) :- P = [
    [_,7,_,5,_,_,_,_,6],
    [_,_,_,1,7,6,_,_,_],
    [_,_,_,_,_,_,_,8,_],
    [8,_,5,_,_,_,_,_,_],
    [_,_,_,_,_,_,9,_,_],
    [1,6,_,4,_,_,5,_,7],
    [_,_,2,_,_,_,_,_,_],
    [6,_,_,_,_,2,_,_,1],
    [7,1,_,_,_,3,_,_,_]].
    
alt_sudowiki_nb49(P) :- P = [
    [_,_,1,_,_,_,7,_,_],
    [3,_,_,_,_,_,_,_,8],
    [_,2,_,_,_,_,4,_,_],
    [_,_,8,_,_,_,_,_,1],
    [_,_,_,_,2,5,_,_,_],
    [_,5,_,1,4,8,_,6,_],
    [_,9,_,_,_,6,_,2,_],
    [4,_,_,_,_,_,_,9,_],
    [_,_,_,5,9,_,_,_,_]].
    
example1(P) :- P = [
    [1,3,4,_],
    [4,2,3,_],
    [_,1,_,_],
    [2,4,_,_]].

       