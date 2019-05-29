
:- use_module(library(chr)).
:- consult("benchmarks").

:- chr_option(debug, off).
:- chr_option(optimize, full).
:- op(700, xfx, in).
:- op(700, xfx, eq).
:- op(600, xfx, '..').
:- chr_constraint border/1, board/7, sink/3, flow/7.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                 Solvers and experiments                %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint solve/1.

solve(Id) <=>
    load_puzzle(Id),
    writeln("Loaded puzzle, storing constraints... "),
    bridge_constraints,
    additional_constraints,
    flow_constraints,
    writeln("Stored constraints, making domains..."),
    make_domains, 
    writeln("Made domains."),
    print_board,
    writeln("Searching..."),
    %search,
    %print_board,
    empty_constraint_store
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                   Bridge  constraints                  %%%%%%%%%%%%%
%%%%%%%%%%%%%          The same as the ECLiPSe implementation        %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint bridge_constraints/0.

% The total number of connections must equal the island number.
% Each side of an island supports at maximum 2 connections.
bridge_constraints, board(_,_,Sum,BN,BE,BS,BW) ==> Sum > 0 |
    S in 0..4, add(BN, BE, S),
    Ss in 0..4,add(BS, BW, Ss),
    add(S, Ss, Sum).

% A non-island tile has the same amount of connections on adjacent sides.
bridge_constraints, board(_,_,0,BN,BE,BS,BW) ==> BN = BS, BE = BW.

% Adjacent cells have the same number of connections on opposite sides.
bridge_constraints,
    board(X,Y,_,BN,_,_,_), board(Xx,Y,_,_,_,BS,_) ==> Xx is X-1 | BN eq BS.

bridge_constraints, 
    board(X,Y,_,_,BE,_,_), board(X,Yy,_,_,_,_,BW) ==> Yy is Y+1 | BE eq BW.

% Connections to the border are not possible.
bridge_constraints, board(1,_,_,BN,_,_,_) ==> BN = 0.
bridge_constraints, board(_,1,_,_,_,_,BW) ==> BW = 0.
bridge_constraints, board(_,Border,_,_,BE,_,_), border(Border) ==> BE = 0.
bridge_constraints, board(Border,_,_,_,_,BS,_), border(Border) ==> BS = 0.

% Deactivate bridge_constraints.
bridge_constraints <=> true.

% A non-island tile has either vertical or horizontal connections, not both.
% We always want these constraints in the store.
board(_,_,0,BN,BE,_,_) ==> number(BN), BN > 0 | BE = 0.
board(_,_,0,BN,BE,_,_) ==> number(BE), BE > 0 | BN = 0.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%    Additional constraints based on segment isolation   %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint additional_constraints/0, island/3, neighbours/4.

% Create the neighbour relation 'n(Orientation, ConstPos, Xsmall, Xbig)'.
% The neighbour relation follows (topleft -> bottomright)
island(X,Y,_), island(Xx,Y,_) ==> X < Xx | neighbours(X, Y, Xx, Y).
island(X,Y,_), island(X,Yy,_) ==> Y < Yy | neighbours(X, Y, X, Yy).
% Remove interrupted neighbour relations.
neighbours(X,Y,Xx,Y) \ neighbours(X,Y,Xxx,Y) <=> Xx < Xxx | true.
neighbours(X,Y,X,Yy) \ neighbours(X,Y,X,Yyy) <=> Yy < Yyy | true.

% 1-1 connections are impossible
additional_constraints, neighbours(X,Y,Xx,Y), island(Xx,Y,1), board(X,Y,1,_,_,BS,_) ==> var(BS) | BS = 0.
additional_constraints, neighbours(X,Y,X,Yy), island(X,Yy,1), board(X,Y,1,_,BE,_,_) ==> var(BE) | BE = 0.

% 2=2 connections are impossible
additional_constraints, neighbours(X,Y,Xx,Y), island(Xx,Y,2), board(X,Y,2,_,_,BS,_) ==> var(BS) | BS in 0..1.
additional_constraints, neighbours(X,Y,X,Yy), island(X,Yy,2), board(X,Y,2,_,BE,_,_) ==> var(BE) | BE in 0..1.

% Deactivate additional_constraints.
additional_constraints \ neighbours(_,_,_,_) <=> true.

additional_constraints <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                    Flow constraints                    %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint flow_constraints/0.

% Net flow arriving at the sink should equal the number of islands-1.
flow_constraints, sink(X,Y,Ft), flow(X,Y,_,FN,FE,FS,FW) ==>
    add(FN, FE, S), add(FS,FW,Ss), add(S,Ss, Ft).

% Every non-sink island generates 1 unit of flow.
flow_constraints, flow(_,_,-1,FN,FE,FS,FW) ==> 
    add(FN,FE, S), add(FS,FW,Ss), add(S,Ss, -1).

% Non-island cells generate no net flow
flow_constraints, flow(_,_,0,FN,_,FS,_) ==> add(FN, FS, 0).
flow_constraints, flow(_,_,0,_,FE,_,FW) ==> add(FE, FW, 0).

% Can't have any flow along the borders.
flow_constraints, flow(1,_,_,FN,_,_,_) ==> FN = 0.
flow_constraints, flow(_,1,_,_,_,_,FW) ==> FW = 0.
flow_constraints, flow(_,Border,_,_,FE,_,_), border(Border) ==> FE = 0.
flow_constraints, flow(Border,_,_,_,_,FS,_), border(Border) ==> FS = 0.

% Adjacent cells have the same number of connections on opposite sides.
flow_constraints, flow(X,Y,_,FN,_,_,_), flow(Xx,Y,_,_,_,FS,_) ==> Xx is X-1 | add(FN,FS,0).
flow_constraints, flow(X,Y,_,_,FE,_,_), flow(X,Yy,_,_,_,_,FW) ==> Yy is Y+1 | add(FE,FW,0).


% You can't have any flow in a cell that contains no connections. Flow implies connections.
% However, connections don't imply flow! (see report)
flow_constraints,
    board(X,Y,_,0,_,_,_), flow(X,Y,_,FN,_,_,_) ==> FN is 0.
flow_constraints,
    board(X,Y,_,_,0,_,_), flow(X,Y,_,_,FE,_,_) ==> FE is 0.
flow_constraints,
    board(X,Y,_,_,_,0,_), flow(X,Y,_,_,_,FS,_) ==> FS is 0.
flow_constraints,
    board(X,Y,_,_,_,_,0), flow(X,Y,_,_,_,_,FW) ==> FW is 0.


flow_constraints <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                      Make Domains                      %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint make_domains/0.


% These need to be in the constraint store (see 'add(X,Y,Z)' for explanation why)
make_domains ==>
    0 in 0..0, 1 in 1..1, 2 in 2..2, 3 in 3..3, 4 in 4..4, 5 in 5..5, 6 in 6..6, 7 in 7..7, 8 in 8..8, 9 in 9..9, 10 in 10..10,
    11 in 11..11, 12 in 12..12, 13 in 13..13, 14 in 14..14, 15 in 15..15, 16 in 16..16, 17 in 17..17, 18 in 18..18.

% Maximum 2 connections per cardinal direction.
make_domains, board(_,_,_,BN,_,_,_) ==> BN in 0..2.
make_domains, board(_,_,_,_,BE,_,_) ==> BE in 0..2.
make_domains, board(_,_,_,_,_,BS,_) ==> BS in 0..2.
make_domains, board(_,_,_,_,_,_,BW) ==> BW in 0..2.

% Remove make_domains from the constraint store.
make_domains <=> true.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%              Constraint solving expressions            %%%%%%%%%%%%%
%%%%%%%%%%%%%         Inspired by FD solver from slides 6.64..       %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint add/3, in/2, eq/2, neg/2, neg/1.

% If X lies in [A,A], X must be A.
X in A..A <=> var(X) |X = A.
X in A..B \ X in A..B <=> var(X) | true.

% Equality constraint (taken from Slide 6.66)
% For numbers this reduces to the simple equality.
X eq Y <=> number(X), number(Y) | X == Y.

% For intervals this potentially reduces the interval size.
% Update lower bound.
X eq Y \ X in A..B, Y in C..D <=> A \== C |
    L is max(A,C),
    X in L..B,
    Y in L..D.
% Update upper bound.
X eq Y \ X in A..B, Y in C..D <=> B \== D|
    U is min(B,D),
    X in A..U,
    Y in C..U.

% Addition constraint (taken from slide 6.66)
% For numbers this reduces to the simple addition.
add(X,Y,Z) <=> number(X), number(Y), number(Z) | Z is X+Y.

% For intervals this is a little more complex. 
% For this to support numbers we added the appropriate 'nb in nb..nb' to the constraint store.
% E.g. "3 in 3..3, Y in 0..5, Z in -10..10, add(3, Y, Z)" gives "Z in 3..8".
%      "Y in 0..5, Z in -10..10, add(3, Y, Z)" Does not fire the add rule.
add(X,Y,Z) \ X in A..B, Y in C..D, Z in E..F <=>
    not(( A>=E-D, B=<F-C, C>=E-B, D=<F-A, E>=A+C, F=<B+D)) |
    Lx is max(A,E-D), Ux is min(B,F-C), X in Lx..Ux,
    Ly is max(C,E-B), Uy is min(D,F-A), Y in Ly..Uy,
    Lz is max(E,A+C), Uz is min(F,B+D), Z in Lz..Uz.

add(X,Y,X) \ Y in _.._ <=> X = 0 | Y in 0..0.
add(Y,Y,X) \ Y in _.._ <=> X = 0 | Y in 0..0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                           Search                       %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint search/0, enum/1.

enum(X) <=> number(X) | true.

% Note that the between predicate selects the lowest value first.
enum(X), X in A..B <=> between(A, B, X).

search, X in _.._ ==> var(X) | enum(X).
search <=> writeln("Solution:"), true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%       Load puzzle by Id: Empty -> Islands -> Sink      %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint load_puzzle/1, load_islands/1, generate_empty_board/2, assign_sink/1.

% Load the puzzle.
% This generates all the necessary board/7 facts into the constraint store.
% It then updates island facts (Sum of connections).
% Finally it generates the sink/3 fact used for the flow algorithm.
load_puzzle(Id) <=> puzzle(Id,Size,Islands) |
    border(Size), generate_empty_board(1,1), load_islands(Islands), assign_sink(Islands).

% Generate an empty board of (Size, Size).
border(Border) \ generate_empty_board(_ ,Col) <=> Col > Border | true.

border(Border) \ generate_empty_board(Row,Col) <=> Row > Border, NextCol is Col+1 |
    generate_empty_board(1, NextCol).

border(Border) \ generate_empty_board(Row,Col) <=> Row =< Border, NextRow is Row+1 |
    board(Row,Col,0,_,_,_,_), flow(Row,Col,0,_,_,_,_), generate_empty_board(NextRow, Col).

% Load the islands into the board.
load_islands([]) <=> true.

load_islands([(X,Y,Sum) | Islands]), board(X,Y,_,BN,BE,BS,BW), flow(X,Y,_,_,_,_,_) <=> 
    board(X,Y,Sum,BN,BE,BS,BW),
    flow(X,Y,-1,_,_,_,_),
    load_islands(Islands),
    island(X,Y,Sum).

assign_sink([(X,Y,_) | Rest]) <=> length(Rest, NbIslands), sink(X,Y,NbIslands).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                         Print                          %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
:- chr_constraint empty_constraint_store/0, print_board/0, print_board/2.

print_board <=> print_board(1,1).

board(X,Y, Island, BN, BE,_,_) \ print_board(X,Y) <=> 
    Yy is Y+1,
    (Island > 0 ->
    % Print the island number
        write(" "),
        write(Island),
        write(" ")
    ;
    % If not assigned yet
        ((var(BN) ; var(BE)) ->
            write(" ?")
        ;
            symbol(BN, BE, Char),
            write(Char)
        )
    ),
    print_board(X, Yy),
    !.

board(X,_,_,_,_,_,_) \ print_board(X,_) <=> 
    Xx is X+1, nl | print_board(Xx, 1).

print_board(_,_) <=> nl.

symbol(0, 0, '    ').
symbol(0, 1, '---').
symbol(0, 2, '==').
symbol(1, 0, '  | ').
symbol(2, 0, ' || ').

%empty_constraint_store \ flow(_,_,_,_,_,_,_) <=> true.
empty_constraint_store \ island(_,_,_) <=> true.
empty_constraint_store \ add(_,_,_) <=> true.
empty_constraint_store \ board(_,_,_,_,_,_,_) <=> true.
empty_constraint_store \ _ in _.._ <=> true.
empty_constraint_store \ _ eq _ <=> true.
empty_constraint_store \ border(_) <=> true.
empty_constraint_store <=> true.
