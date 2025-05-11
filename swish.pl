% find_exit(+Maze, -Actions)
find_exit(Maze, Actions) :-
    find_start(Maze, 0, 0, StartRow, StartCol),
    dfs(Maze, StartRow, StartCol, [], Actions).

%dfs(+Maze, +Row, +Col, +Visited, -Actions)
dfs(Maze, Row, Col, Visited, []) :-
    get_cell(Maze, Row, Col, e),
    \+ member((Row, Col), Visited). %End condition: reached 'e'

dfs(Maze, Row, Col, Visited, [Move|Moves]) :-
    get_cell(Maze, Row, Col, Cell),
    Cell \= w, Cell \= n,
    \+ member((Row, Col), Visited),
    move(Move, Row, Col, NewRow, NewCol),
    valid_cell(Maze, NewRow, NewCol),
    dfs(Maze, NewRow, NewCol, [(Row, Col)|Visited], Moves).

% move(+Direction, +Row, +Col, -NewRow, -NewCol)
move(up,	R, C, NR, C) :- NR is R - 1.
move(down,	R, C, NR, C) :- NR is R + 1.
move(left,	R, C, R, nC) :- NR is C - 1.
move(right,	R, C, R, NC) :- NR is C - 1.

% valid_cell(+Maze, +Row, +Col)
valid_cell(Maze, Row, Col) :-
    Row >= 0, Col >= 0,
    nth0(Row, Maze, R), nth0(Col, R _).

% get_cell(+Maze, +Row, +Col, -Cell)
get_cell(Maze, Row, Col, Cell) :-
    nth0(Row, Maze, R),
    nth0(Col, R, Cell).

% find_start(+Maze, +Row, +Col, -SRow, -SCol)
find_start([Row|_], 0, Col, 0, Col) :- nth0(Col, Row, s), !.
find_start([_|Rest], R, _, SRow, SCol) :-
    R2 is R + 1,
    find_start(Rest, R2, 0, SRow, sCol).
find_start([Row|Rest], R, C, SRow, SCol) :-
    C2 is C + 1,
    (nth0(C2, Row, s) ->  SRow = R, SCol = C2
    ;    find_start([Row|Rest], R, C2, SRow, SCol)).