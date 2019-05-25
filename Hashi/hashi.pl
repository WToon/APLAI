% Part of this solver is based on the partial solution by Stack Overflow user "jschimpf"
% https://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions

:- lib(ic).  
:- ensure_loaded('./benchmarks.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                      Hashiwokakero                     %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hashi(Id) :-
  statistics(runtime, [_ | [_]]),
  
  load_puzzle(Id, Board, NbIslands, Sink),
  Is is Sink[1], Js is Sink[2],
  dim(Board, [Imax,Jmax]),
  dim(NESW, [Imax,Jmax,8]),   % 8 variables for each field (Bridge connections on each side and flow on each side)
  ( foreachindex([I,J],Board),
    param(Board,NESW,Imax,Jmax, Is, Js, NbIslands) do
      Sum is Board[I,J],  % The total number of connections of a island (Sum > 0 for an island)
      BN is NESW[I,J,1],  % The nb of connections on the north edge of the field
      BE is NESW[I,J,2],  % The nb of connections on the east edge of the field
      BS is NESW[I,J,3],  % The nb of connections on the south edge of the field
      BW is NESW[I,J,4],  % The nb of connections on the west edge of the field
      FN is NESW[I,J,5],  % The flow on the north side of the field
      FE is NESW[I,J,6],  % The flow on the east side of the field
      FS is NESW[I,J,7],  % The flow on the south side of the field
      FW is NESW[I,J,8],  % The flow on the west side of the field

      % Connections on the side of a tile imply the same amount of connections on the connecting side of the adjacent tile.
      ( I > 1    -> BN #= NESW[I-1,J,3] ; BN = 0 ),
      ( I < Imax -> BS #= NESW[I+1,J,1] ; BS = 0 ),
      ( J > 1    -> BW #= NESW[I,J-1,2] ; BW = 0 ),
      ( J < Jmax -> BE #= NESW[I,J+1,4] ; BE = 0 ),
      % Outgoing/Incoming flow on one side of a tile implies the same amount of Incoming/Outgoing flow on the side of the adjacent tile.
      ( I > 1     -> FN #= -NESW[I-1,J,7]; FN = 0 ),
      ( I < Imax  -> FS #= -NESW[I+1,J,5]; FS = 0 ),
      ( J > 1     -> FW #= -NESW[I,J-1,6]; FW = 0 ),
      ( J < Jmax  -> FE #= -NESW[I,J+1,8]; FE = 0 ),

      % If a flow is present, there must be a connection.
      % If there is a connection, there may be a flow.
      (BN #\= 0) or (FN #= 0),
      (BE #\= 0) or (FE #= 0),
      (BW #\= 0) or (FW #= 0),
      (BS #\= 0) or (FS #= 0),

      % Islands
      ( Sum > 0 ->

        % Each side of an island supports at maximum 2 connections.
        [BN,BE,BS,BW] #:: 0..2,
        % The total number of connections must equal the island number.
        BN+BE+BS+BW #= Sum,

        [FE,FN,FS,FW] #:: -NbIslands+1..NbIslands-1,
        ( I =:= Is, J =:= Js ->
          % The sink island

          % The net incoming flow at the sink island should be 
          % equal to the total amount of flow generated (the number of islands minus one).
          FE+FN+FS+FW #= NbIslands-1
          ;
          % Every non-sink island generates 1 unit of flow, 
          % so the total amount of flow leaving any non-sink
          % island should be one higher than the amount 
          % of flow arriving on the island.
          FE+FN+FS+FW #= -1
        )
        ;
      % Non-islands

        % A non-island tile has the same amount of connections on adjacent sides.
        BN = BS, BE = BW,
        % A non-island tile has either vertical or horizontal connections, not both.
        (BN #= 0) or (BE #= 0),
        % A non-island tile generates no flow.
        FN #= -FS, FE #= -FW
      )
  ),
  statistics(runtime, [_ | [ConstraintTime]]),
  write('ConstraintTime: '), write(ConstraintTime),nl,

  statistics(runtime, [_ | [_]]),
  search(first_fail, NESW, NbBacktracks),
  statistics(runtime, [_ | [SearchTime]]),
  write('Backtracks:  '), write(NbBacktracks),nl, write('SearchTime:  '), write(SearchTime),
  !,          % We cut here as the flow is not deterministic. To verify this u can run 'run_extra_example' without this cut.
  print_board(Board, NESW),
  print_flow(Board, NESW),
  nl.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%       Search strategies (ordered by performance)       %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


% This is the best search strategy for the problem.
search(first_fail, List, NbBacktracks) :-
  search(List, 0, first_fail, indomain, complete, [backtrack(NbBacktracks)]).

% Also a viable strategy.
search(most_constrained, List, NbBacktracks) :-
  search(List, 0, most_constrained, indomain, complete, [backtrack(NbBacktracks)]).

% This search strategy solves the puzzles but does so inefficiently.
search(input_order, List, NbBacktracks) :-
  search(List, 0, input_order, indomain, complete, [backtrack(NbBacktracks)]).

% This is a terrible search strategy for the problem. It won't manage to solve puzzle 2.
search(anti_first_fail, List, NbBacktracks) :-
  search(List, 0, anti_first_fail, indomain, complete, [backtrack(NbBacktracks)]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%                    Helper predicates                   %%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Run the 6 provided benchmark puzzles.
run_benchmark_puzzles :- hashi(1), hashi(2), hashi(3), hashi(4), hashi(5), hashi(6).

% Run the example to show that flow is not deterministic.
run_extra_example :- hashi(7).

% Load the benchmark puzzle by Id.
load_puzzle(Id, Puzzle, NbIslands, Sink) :-
  % Load puzzle information
  puzzle(Id, Size, Islands),

  % Get some information for the flow algorithm (Sink, #Islands)
  length(Islands,NbIslands),
  Isink is Islands[1,1], Jsink is Islands[1,2,1],
  Sink = (Isink,Jsink),

  % Generate an empty board of the correct size
  dim(Puzzle, [Size, Size]),
  % Load island cells into board
  ( foreach(Isle, Islands), param(Puzzle) do 
    I is Isle[1], J is Isle[2,1], 
    Sum is Isle[2,2], Sum is Puzzle[I,J]
  ),
  % Load non-island cells into board
  ( foreachindex([I,J], Puzzle), param(Puzzle) do
    ( V is Puzzle[I,J], var(V) -> 0 is Puzzle[I,J]; true)
  ).

% Display the board state.
print_board(Board, NESW) :-
  ( foreachindex([I,J],Board), param(Board,NESW) do
    ( J > 1 -> true ; nl ),
    Sum is Board[I,J],
    ( Sum > 0 ->
        write(Sum)
    ; 
        NS is NESW[I,J,1],
        EW is NESW[I,J,2],
        symbol(NS, EW, Char),
        write(Char)
    ),
    write(' ')
  ),
  nl.

% Display the underlying flow.
print_flow(Board, NESW) :-
  ( foreachindex([I,J],Board), param(Board,NESW) do
      ( J > 1 -> true ; nl ),
      Sum is Board[I,J],
      ( Sum > 0 ->
          write(Sum)
      ; 
          NS is NESW[I,J,5],
          EW is NESW[I,J,6],
          ( NS =:= 0 -> 
            ( EW =:= 0 -> Char = ' '; Char = '-'); 
            ( EW =:= 0 -> Char = '|'; Char = 'X')
          ),
          write(Char)
      ),
      write(' ')
  ),
  nl.

symbol(0, 0, ' ').
symbol(0, 1, '-').
symbol(0, 2, '=').
symbol(1, 0, '|').
symbol(2, 0, 'X').
