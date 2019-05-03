% Partial solution by Stack Overflow user "jschimpf"
% https://stackoverflow.com/questions/20337029/hashi-puzzle-representation-to-solve-all-solutions-with-prolog-restrictions

:- lib(ic).  % uses the integer constraint library

% 1. bridges run horizontally or vertically
% 2. bridges run in one straight line
% 3. bridges cannot cross other bridges or islands
% 4. at most two bridges connect a pair of islands
% 5. sum constraint
% 6. connectedness


hashi(Name) :-
        board(Name, Board),
        dim(Board, [Imax,Jmax]),
        dim(NESW, [Imax,Jmax,4]),   % 4 variables N,E,S,W for each field
        ( foreachindex([I,J],Board), param(Board,NESW,Imax,Jmax) do
            Sum is Board[I,J],
            N is NESW[I,J,1],
            E is NESW[I,J,2],
            S is NESW[I,J,3],
            W is NESW[I,J,4],

            % Constraints 1 and 2:
            % The combination of N=S etc on tiles without island
            % and f.i. north on current tile must equal south on
            % other northern tile, express that bridges can only run
            % in a straight line, horizontally or vertically.
            % They also express another constraint that was not stated
            % explicitly: that bridges must be on both sides connected
            % to islands.
            ( I > 1    -> N #= NESW[I-1,J,3] ; N = 0 ),
            ( I < Imax -> S #= NESW[I+1,J,1] ; S = 0 ),
            ( J > 1    -> W #= NESW[I,J-1,2] ; W = 0 ),
            ( J < Jmax -> E #= NESW[I,J+1,4] ; E = 0 ),
            ( Sum > 0 ->
              % Constraint 4
              [N,E,S,W] #:: 0..2,

              % Constraint 5
              N+E+S+W #= Sum
            ;
            N = S, E = W,

            % Constraint 3
            (N #= 0) or (E #= 0)
            )
        ),

        % find a solution
        labeling(NESW),
        print_board(Board, NESW).


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

symbol(0, 0, ' ').
symbol(0, 1, '-').
symbol(0, 2, '=').
symbol(1, 0, '|').
symbol(2, 0, 'X').


% Examples

board(stackoverflow,
     []([](4, 0, 6, 0, 0, 0, 6, 0, 3),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](0, 1, 0, 0, 0, 0, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](3, 0, 0, 0, 0, 1, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](1, 0, 3, 0, 0, 2, 0, 0, 0),
        [](0, 3, 0, 0, 0, 0, 4, 0, 1))
    ).
board(wikipedia,
     []([](2, 0, 4, 0, 3, 0, 1, 0, 2, 0, 0, 1, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 1),
        [](0, 0, 0, 0, 2, 0, 3, 0, 2, 0, 0, 0, 0),
        [](2, 0, 3, 0, 0, 2, 0, 0, 0, 3, 0, 1, 0),
        [](0, 0, 0, 0, 2, 0, 5, 0, 3, 0, 4, 0, 0),
        [](1, 0, 5, 0, 0, 2, 0, 1, 0, 0, 0, 2, 0),
        [](0, 0, 0, 0, 0, 0, 2, 0, 2, 0, 4, 0, 2),
        [](0, 0, 4, 0, 4, 0, 0, 3, 0, 0, 0, 3, 0),
        [](0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
        [](2, 0, 2, 0, 3, 0, 0, 0, 3, 0, 2, 0, 3),
        [](0, 0, 0, 0, 0, 2, 0, 4, 0, 4, 0, 3, 0),
        [](0, 0, 1, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0),
        [](3, 0, 0, 0, 0, 3, 0, 1, 0, 2, 0, 0, 2))
    ).
