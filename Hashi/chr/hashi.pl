
:- use_module(library(chr)).
:- consult("benchmarks").

:- chr_option(debug, off).
:- chr_option(optimize, full).

:- chr_constraint add/3, in/2, eq/2.

:- op(700, xfx, in).
:- op(700, xfx, eq).
:- op(600, xfx, '..').

:- chr_constraint border/1, board/7, sink/3.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                 Solvers and experiments                %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint solve/1.

solve(Id) <=>
    load_puzzle(Id),
    write("Loaded puzzle "), write(Id), write("."), nl,
    bridge_constraints,
    make_domains,
    writeln("Applied bridge constraints and made domains."),
    writeln("Board state: U = undefined"),
    print_board,
    writeln("searching"),
    search,
    print_board,
    empty_constraint_store.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                   Bridge  constraints                  %%%%%%%%%%%%%
%%%%%%%%%%%%%          The same as the ECLiPSe implementation        %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint bridge_constraints/0.

% The total number of connections must equal the island number.
% Each side of an island supports at maximum 2 connections.
bridge_constraints, board(_, _, Sum, BN, BE, BS, BW) ==>
    Sum > 0 |
    S in 0..4,
    add(BN, BE, S),
    Ss in 0..4,
    add(BS, BW, Ss),
    add(S, Ss, Sum).

% A non-island tile has the same amount of connections on adjacent sides.
bridge_constraints, board(_, _, 0, BN, BE, BS, BW) ==> 
    BN = BS,
    BE = BW.

% A non-island tile has either vertical or horizontal connections, not both.
board(_, _, 0, BN, BE, _, _) ==>
    number(BN), BN > 0 |
    BE = 0.
board(_, _, 0, BN, BE, _, _) ==>
    number(BE), BE > 0 |
    BN = 0.

% Adjacent cells have the same number of connections on opposite sides.
bridge_constraints,
    board(X, Y, _, BN, _, _, _),
    board(X, Yy, _, _, _, BS, _) ==>
    Yy is Y-1|
    BN eq BS.

bridge_constraints, 
    board(X, Y, _, _, BE, _, _),
    board(Xx, Y, _, _, _, _, BW) ==>
    Xx is X+1 |
    BE eq BW.

% Connections to the border are not possible.
bridge_constraints, 
    board(_, 1, _, BN, _, _, _)
    ==> BN = 0.
bridge_constraints, 
    board(Border, _, _, _, BE, _, _), 
    border(Border) 
    ==> BE = 0.
bridge_constraints,
    board(_, Border, _, _, _, BS, _),
    border(Border)
    ==> BS = 0.
bridge_constraints,
    board(1, _, _, _, _, _, BW)
    ==> BW = 0.

% Deactivate bridge_constraints.
bridge_constraints <=> true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%    Additional constraints based on segment isolation   %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint additional_constraints/0, island/3, neighbours/4.

% Create the neighbour relation 'n(Orientation, ConstPos, Xsmall, Xbig)'.
% The neighbour relation follows (topleft -> bottomright)
island(X,Y,_), island(Xx,Y,_) ==> X < Xx | neighbours('H', Y, X, Xx).
island(X,Y,_), island(X,Yy,_) ==> Y < Yy | neighbours('V', X, Y, Yy).
% Remove interrupted neighbour relations.
neighbours('H', Y, X, Xx) \ neighbours('H', Y, X, Xxx) <=> Xx < Xxx | true.
neighbours('V', X, Y, Yy) \ neighbours('V', X, Y, Yyy) <=> Yy < Yyy | true.

% 1-1 connections are impossible
additional_constraints,
    neighbours('H', Y, X, Xx),board(X, Y, 1,_,_,_,_),island(Xx, Y,1) \ BN in _.._ <=> BN = 0.
additional_constraints,
    neighbours('V', X, Y, Yy),board(X, Y, 1,_,_,_,_),island(X, Yy,1) \ BE in _.._ <=> BE = 0.

% 2=2 connections are impossible
additional_constraints,
    neighbours('H', Y, X, Xx),board(X, Y, 2,_,_,_,_),island(Xx, Y,2) \ BN in A.._ <=> BN in A..1.
additional_constraints,
    neighbours('V', X, Y, Yy),board(X, Y, 2,_,_,_,_),island(X, Yy,2) \ BE in A.._ <=> BE in A..1.



% Deactivate additional_constraints.
additional_constraints <=> true.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                      Make Domains                      %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint make_domains/0.


% These need to be in the constraint store (see 'add(X,Y,Z)' for explanation why)
make_domains ==>
    0 in 0..0,
    1 in 1..1,
    2 in 2..2,
    3 in 3..3,
    4 in 4..4,
    5 in 5..5,
    6 in 6..6,
    7 in 7..7,
    8 in 8..8.

% Maximum 2 connections per cardinal direction.
make_domains,
    board(_, _, _, BN, _, _, _)
    ==> var(BN) | BN in 0..2.
make_domains,
    board(_, _, _, _, BE, _, _)
    ==> var(BE) | BE in 0..2.
make_domains,
    board(_, _, _, _, _, BS, _)
    ==> var(BS) | BS in 0..2.
make_domains,
    board(_, _, _, _, _, _, BW)
    ==> var(BW) | BW in 0..2.



% Remove make_domains from the constraint store.
make_domains <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%              Constraint solving expressions            %%%%%%%%%%%%%
%%%%%%%%%%%%%         Inspired by FD solver from slides 6.64..       %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% If X lies in [A,A], X must be A.
X in A..A <=> var(X) |X = A.
X in A..B \ X in A..B <=> var(X) | true.


% Equality constraint (taken from Slide 6.66)
% For numbers this reduces to the simple equality.
X eq Y <=> number(X), number(Y) | X == Y.

% For intervals this potentially reduces the interval size.
% Update lower bound.
X eq Y \ X in A..B, Y in C..D <=>
    A \== C |
    L is max(A,C),
    X in L..B,
    Y in L..D.
% Update upper bound.
X eq Y \ X in A..B, Y in C..D <=>
    B \== D|
    U is min(B,D),
    X in A..U,
    Y in C..U.

% Addition constraint (taken from slide 6.66)
% For numbers this reduces to the simple addition.
add(X,Y,Z) <=> 
    number(X), number(Y), number(Z) | 
    Z is X+Y.

% For intervals this is a little more complex. 
% For this to support numbers we added the appropriate 'nb in nb..nb' to the constraint store.
% E.g. "3 in 3..3, Y in 0..5, Z in -10..10, add(3, Y, Z)" gives "Z in 3..8".
%      "Y in 0..5, Z in -10..10, add(3, Y, Z)" Does not fire the add rule.
add(X,Y,Z) \ X in A..B, Y in C..D, Z in E..F <=>
    not(( A>=E-D, B=<F-C, C>=E-B, D=<F-A, E>=A+C, F=<B+D)) |
    Lx is max(A,E-D), Ux is min(B,F-C), X in Lx..Ux,
    Ly is max(C,E-B), Uy is min(D,F-A), Y in Ly..Uy,
    Lz is max(E,A+C), Uz is min(F,B+D), Z in Lz..Uz.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                           Search                       %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint search/0, enum/1.

enum(X) <=> number(X) | write(X),true.

% Note that the between predicate selects the lowest value first.
% TODO Maybe selecting the highest value first will lead to faster backtracks?
enum(X), X in A..B <=> between(A, B, X).

search, X in _.._ ==> var(X) | enum(X).
search <=> true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%       Load puzzle by Id: Empty -> Islands -> Sink      %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint load_puzzle/1, load_islands/1, generate_empty_board/2, assign_sink/2.

% Load the puzzle.
% This generates all the necessary board/7 facts into the constraint store.
% It then updates island facts (Sum of connections).
% Finally it generates the sink/3 fact used for the flow algorithm.
load_puzzle(Id) <=> 
    puzzle(Id, Size, Islands) |
    border(Size),
    generate_empty_board(1,1),
    load_islands(Islands),
    assign_sink(Islands, Islands).

% Generate an empty board of (Size, Size).
border(Border) \ generate_empty_board( _ ,Col) <=> 
    Col > Border | true.
border(Border) \ generate_empty_board(Row,Col) <=> 
    Row > Border, NextCol is Col+1 |
    generate_empty_board(1, NextCol).
border(Border) \ generate_empty_board(Row,Col) <=> 
    Row =< Border, NextRow is Row+1 |
    board(Row,Col,0,_,_,_,_), 
    generate_empty_board(NextRow, Col).

% Load the islands into the board.
load_islands([]) <=> true.
load_islands([(X,Y,Sum)| Islands]), board(X,Y,_,BN,BE,BS,BW) <=> 
    board(X,Y,Sum,BN,BE,BS,BW),
    load_islands(Islands),
    % Needed for neighbour relations
    island(X,Y,Sum).

% Assign a sink value.
assign_sink([(X,Y,_)| _],Islands) <=> 
    length(Islands, Len),
    SinkFlow is Len-1,
    sink(X,Y,SinkFlow).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                         Print                          %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint empty_constraint_store/0, print_board/0, print_board/2.

print_board <=> print_board(1,1).

board(X,Y, Island, BN, BE, _, _) \ print_board(X,Y) <=> 
    Xx is X+1,
    (Island > 0 ->
    % Print the island number
        write(Island)
    ;
    % If not assigned yet
        ((var(BN) ; var(BE)) ->
            write("U")
        ;
            symbol(BN, BE, Char),
            write(Char)
        )
    ),
    print_board(Xx, Y),
    !.

board(_, Y, _, _, _, _, _) \ print_board(_, Y) <=> 
    Yy is Y+1, nl | print_board(1, Yy).

print_board(_, _) <=> nl.

symbol(0, 0, '  ').
symbol(0, 1, '--').
symbol(0, 2, '=').
symbol(1, 0, ' | ').
symbol(2, 0, '||').


empty_constraint_store \ add(_,_,_) <=> true.
empty_constraint_store \ board(_,_,_,_,_,_,_) <=> true.
empty_constraint_store \ sink(_,_,_) <=> true.
empty_constraint_store \ _ in _.._ <=> true.
empty_constraint_store \ _ eq _ <=> true.
empty_constraint_store \ border(_) <=> true.
empty_constraint_store <=> true.
