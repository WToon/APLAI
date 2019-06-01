:- use_module(library(chr)).
:- op( 700,xfx,in).
:- op( 700,xfx,ne).
:- op( 700,xfx,eq).
:- chr_constraint in/2, ne/2,enum/1, indomain/1, channelToXs/4, channelToBoard/4, inDifferentBox/3.

:- set_prolog_flag(chr_toplevel_show_store, false).

:- chr_option(debug,off).
:- chr_option(optimize,full).
:- [example_puzzles].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CHR CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

channelToXs(Xs,X,Y,K) <=> nonvar(K) | get(Xs,Y,K,X).
% Meaning X_IJ = K => Board_IK = J
channelToBoard(Board,I,J,K) <=> nonvar(K) | get(Board,K,I,J).

% The not equals constraint.
X ne Y <=> nonvar(X),nonvar(Y) | X \== Y.

_ in [] <=> fail.
X in [Y] <=> X = Y.
X in L <=> nonvar(X) | memberchk(X,L).
X in L, X ne Y <=> nonvar(Y) |delete(L,Y,L_New), X in L_New.
X in L, Y ne X <=> nonvar(Y) |delete(L,Y,L_New), X in L_New.

inDifferentBox(X1,X2,NN), X2 in L <=> nonvar(X1) | groupCols(X1,NN,GroupCols), subtract(L,GroupCols,LNew),X2 in LNew.
inDifferentBox(X2,X1,NN), X2 in L <=> nonvar(X1) | groupCols(X1,NN,GroupCols), subtract(L,GroupCols,LNew),X2 in LNew.

indomain(X) <=> nonvar(X) | true.
indomain(X) , X in [V|L] <=> L = [_|_] | (X in [V]; X in L, indomain(X)).

enum([X|L]) <=> indomain(X), enum(L).
enum([]) <=> true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% SOLVERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solveClassic(Board) :-
    flatten(Board,FlatBoard), 
    length(Board,N),
    numlist(1,N,Domain),
    setDomainConstraints(FlatBoard,Domain),
    setRowColAllDifferentConstraints(Board,N),
    setBoxConstraints(Board,N),
    enum(FlatBoard),
    !.
    
solveNew(Xs):-
    length(Xs,N),
    NN is floor(sqrt(N)),
    numlist(1,N,Domain),
    flatten(Xs,FlatBoard),   
    setDomainConstraints(FlatBoard,Domain),
    setRowColAllDifferentConstraints(Xs,N),
    setNewBoxConstraints(Xs,N,NN),
    enum(FlatBoard),
    !.

solveChanneling(Board,Xs):-
    length(Board,N),
    length(Xs,N),
    NN is floor(sqrt(N)),
    createTemplate(N,Board),
    createTemplate(N,Xs),
    flatten(Board,FlatBoard),
    numlist(1,N,Domain),
    setDomainConstraints(FlatBoard,Domain),
    setRowColAllDifferentConstraints(Board,N),
    setBoxConstraints(Board,N),
    flatten(Xs,FlatXs),   
    setDomainConstraints(FlatXs,Domain),
    setNewBoxConstraints(Xs,N,NN),
    setChannelings(Board,Xs),
    enum(FlatBoard),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% CONSTRAINTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

setClassicConstraints(Board) :-
    length(Board,N),
    setRowColAllDifferentConstraints(Board,N),
    setBoxConstraints(Board,N).

setDomainConstraints([],_).
setDomainConstraints([X|Rest],Domain) :-
    X in Domain,
    setDomainConstraints(Rest,Domain).
    
setRowColAllDifferentConstraints(Board, N) :-
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
    
setNewBoxConstraints(Xs,N,NN):-
    EndBox is N - NN + 1,
    for(1,N,1,
        for(1,EndBox,NN,
            setDifferentBoxConstraints(Xs,NN)
        )
    ).

setDifferentBoxConstraints(Xs,NN,Row,StartCol):-
    EndCol is StartCol + NN - 1,
    bagof(E,Col^(between(StartCol,EndCol,Col),get(Xs,Col,Row,E)),L),
    allDifferentBox(L,NN).
    
allDifferentBox([_],_).
allDifferentBox([X|Rest],NN):-
    allDifferentBox(Rest,X,NN),
    allDifferent(Rest).
    
allDifferentBox([],_,_).
allDifferentBox([X2|Rest],X1,NN):-
    inDifferentBox(X1,X2,NN),
    allDifferentBox(Rest,X1,NN).
    
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
    
groupCols(Col,NN,ColList):-
    Lower is ((Col-1) // NN)* NN + 1,
    Higher is ((Col-1) // NN) * NN + NN,
    numlist(Lower,Higher,ColList).
    

    
example1(P) :- P = [
    [1,3,4,_],
    [4,2,3,_],
    [_,1,_,_],
    [_,_,_,_]].

       