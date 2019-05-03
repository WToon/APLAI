% Partial solution by Stack Overflow user "jschimpf"
% https://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions

:- lib(ic).  % uses the integer constraint library
:- ensure_loaded('./benchmarks.pl'). % load puzzles

% Get a board for puzzle Id
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
    ( foreach(Isle, Islands),
      param(Puzzle) do 
        I is Isle[1], J is Isle[2,1], 
        Sum is Isle[2,2], Sum is Puzzle[I,J]
    ),
    % Load non-island cells into board
    ( foreachindex([I,J], Puzzle),
      param(Puzzle) do
        ( V is Puzzle[I,J], var(V) -> 0 is Puzzle[I,J]; true)
    ).

% --------------------------------------------------------------------------------------------------------- %
% 1. bridges run horizontally or vertically
% 2. bridges run in one straight line
% 3. bridges cannot cross other bridges or islands
% 4. at most two bridges connect a pair of islands
% 5. sum constraint
% 6. connectedness


hashi(Name) :-
        load_puzzle(Name, Board, NbIslands, Sink),
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

            ( Sum > 0 ->
              % Islands

              % Each side of an island supports maximum 2 connections.
              [BN,BE,BS,BW] #:: 0..2,
              % The total number of connections must equal the island number.
              BN+BE+BS+BW #= Sum,
              [FN,FE,FS,FW] #:: -NbIslands..NbIslands,

              %[FE,FN,FS,FW] #:: -NbIslands+1..NbIslands-1,
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
              FN #= -FS, FE #= -FW,
              (nonvar(BN), BN =:= 0 -> FN #= 0; true),
              (nonvar(BE), BE =:= 0 -> FE #= 0; true),
              (nonvar(BS), BS =:= 0 -> FS #= 0; true),
              (nonvar(BW), BW =:= 0 -> FW #= 0; true)
              ),
              writeln([I,J])
          ),
          writeln(NESW),

        % find a solution
        writeln('labeling'),
        labeling(NESW),
        writeln('labeled'),
        print_board(Board, NESW),
        print_flow(Board, NESW).


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
