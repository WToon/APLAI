
:- use_module(library(chr)).
:- consult("benchmarks").

:- chr_option(debug, off).
:- chr_option(optimize, full).

:- chr_constraint add/3, in/2, eq/2.

:- op(700, xfx, in).
:- op(700, xfx, eq).
:- op(600, xfx, '..').

:- chr_constraint load_puzzle/1, load_islands/1, generate_empty_board/2, assign_sink/2.
:- chr_constraint border/1, board/7, island/3, sink/3.
:- chr_constraint bridge_constraints/0, make_domains/0.
:- chr_constraint solve/1.
:- chr_constraint print_board/0, print_board/2.
:- chr_constraint empty_constraint_store/0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                 Solvers and experiments                %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solve(Id) <=>
    load_puzzle(Id),
    bridge_constraints,
    print_board,
    make_domains,
    print_board
    % UnComment to have cleaner prints but useful for debugging/testing.
    ,empty_constraint_store
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                   Bridge  constraints                  %%%%%%%%%%%%%
%%%%%%%%%%%%%          The same as the ECLiPSe implementation        %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The total number of connections must equal the island number.
% Each side of an island supports at maximum 2 connections.
bridge_constraints, board(_, _, Sum, BN, BE, BS, BW) ==> Sum > 0 |
    P in 0..4,
    Pl in 0..4,
    add(BN, BE, P),
    add(BS, BW, Pl),
    add(P, Pl, Sum).

% A non-island tile has the same amount of connections on adjacent sides.
bridge_constraints, board(_, _, 0, BN, BE, BS, BW) ==> 
    BN = BS,
    BE = BW.

% A non-island tile has either vertical or horizontal connections, not both.
board(_, _, 0, BN, BE, _, _) ==> number(BN), BN > 0 | BE = 0.
board(_, _, 0, BN, BE, _, _) ==> number(BE), BE > 0 | BN = 0.

% Adjacent cells have the same number of connections on opposite sides.
bridge_constraints,
    board(X, Y, _, BN, _, _, _), 
    board(Xx, Y, _, _, _, BS, _) ==> 
    X > 1, Xx is X-1 |
    BN eq BS.

bridge_constraints, 
    board(X, Y, _, _, BE, _, _), 
    board(X, Yy, _, _, _, _, BW) ==> 
    Yy is Y+1 |
    BE eq BW.

% Connections to the border are not possible.
bridge_constraints, 
    board(1, _, _, BN, _, _, _)
    ==> BN = 0.
bridge_constraints, 
    board(_, Border, _, _, BE, _, _), 
    border(Border) 
    ==> BE = 0.
bridge_constraints,
    board(Border, _, _, _, _, BS, _),
    border(Border)
    ==> BS = 0.
bridge_constraints,
    board(_, 1, _, _, _, _, BW)
    ==> BW = 0.

% Deactivate bridge_constraints.
bridge_constraints <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                      Make Domains                      %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_domains ==>
    0 in 0..0, 1 in 1..1, 2 in 2..2, 3 in 3..3, 4 in 4..4, 5 in 5..5, 6 in 6..6, 7 in 7..7, 8 in 8..8.

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

% Deactivate make_domains.
make_domains <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%              Constraint solving expressions            %%%%%%%%%%%%%
%%%%%%%%%%%%%         Inspired by FD solver from slides 6.64..       %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Reduce the empty interval.
X in A..A <=> var(X) |X = A.
% Remove duplicates. TODO Not sure how these duplicates are generated :/
X in A..B \ X in A..B <=> var(X) | true.

% Equality constraint (taken from Slide 6.66)
X eq Y <=> number(X), number(Y) | X == Y.

X eq Y \ X in A..B, Y in C..D <=>
    A \== C |
    L is max(A,C),
    X in L..B,
    Y in L..D.

X eq Y \ X in A..B, Y in C..D <=>
    B \== D|
    U is min(B,D),
    X in A..U,
    Y in C..U.

% Addition constraint (taken from slide 6.66)
% TODO add support for negative intervals (for flow later on) IF using flow algorithm
% TODO U in -3..4, V in -1..2, W in -8..8, add(U,V,W).
add(X,Y,Z) <=> 
    number(X), number(Y), number(Z) | 
    Z is X+Y.

add(X,Y,Z) \ X in A..B, Y in C..D, Z in E..F <=>
    not(( A>=E-D, B=<F-C, C>=E-B, D=<F-A, E>=A+C, F=<B+D)) |
    Lx is max(A,E-D), Ux is min(B,F-C), X in Lx..Ux,
    Ly is max(C,E-B), Uy is min(D,F-A), Y in Ly..Uy,
    Lz is max(E,A+C), Uz is min(F,B+D), Z in Lz..Uz.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%       Load puzzle by Id: Empty -> Islands -> Sink      %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load_puzzle(Id) <=> puzzle(Id, Size, Islands) |
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
    island(X,Y,Sum),
    load_islands(Islands).

% Assign a sink value.
assign_sink([(X,Y,_)| _],Islands) <=> 
    length(Islands, Len),
    SinkFlow is Len-1,
    sink(X,Y,SinkFlow).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                         Print                          %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

print_board <=> print_board(1,1).

board(X,Y, Island, N, E, _, _) \ print_board(X, Y) <=> 
    Yy is Y+1,
    (Island > 0 ->
    % Print the island number
        write(Island)
    ;
    % If not assigned yet
        ((var(N) ; var(E)) ->
            write("U")
        ;
            symbol(N, E, Char),
            write(Char)
        )
    ),
    print_board(X, Yy),
    !.

board(X, _, _, _, _, _, _) \ print_board(X, _) <=> Xx is X + 1, nl | print_board(Xx, 1).
print_board(_, _) <=> nl.

symbol(0, 0, ' ').
symbol(0, 1, '-').
symbol(0, 2, '=').
symbol(1, 0, 'I').
symbol(2, 0, 'X').

empty_constraint_store \ island(_,_,_) <=> true.
empty_constraint_store \ add(_,_,_) <=> true.
empty_constraint_store \ board(_,_,_,_,_,_,_) <=> true.
empty_constraint_store \ sink(_,_,_) <=> true.
empty_constraint_store \ _ in _.._ <=> true.
empty_constraint_store \ _ eq _ <=> true.
empty_constraint_store \ border(_) <=> true.
empty_constraint_store <=> true.
