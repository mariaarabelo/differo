% Initialize the empty game board with a size of 5 on each side.
empty_board([
    [_, _, _, _, _],
    [_, _, _, _, _, _],
    [_, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _],
    [_, _, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _]
]).

% Initial game state
initial_state(state(Board, white, 13, 13)) :-
    empty_board(Board).

% Predicate to check if a position (Row, Col) is within the bounds of the board.
within_bounds(Row, Col) :-
    between(1, 10, Row),
    MaxCols is 6 + Row,
    between(1, MaxCols, Col).

% Predicate to get the piece at a specific position on the board.
get_piece(Board, Row, Col, Piece) :-
    within_bounds(Row, Col),
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Piece).

% Predicate to set a piece at a specific position on the board.
set_piece(Board, Row, Col, Piece, NewBoard) :-
    within_bounds(Row, Col),
    nth1(Row, Board, RowList, RestRows),
    nth1(Col, RowList, _, RestCols),
    nth1(Row, NewBoard, NewRowList, RestRows),
    nth1(Col, NewRowList, Piece, RestCols).

% Predicate to check if a move is valid.
valid_move(State, Move) :- % Move = move(FromRow, FromCol, ToRow, ToCol)
    state(Board, Player, WhiteCount, BlackCount) = State,
    get_piece(Board, FromRow, FromCol, Player), % Check if moving own piece
    within_bounds(ToRow, ToCol),                 % Check if destination is within bounds
    get_piece(Board, ToRow, ToCol, empty),       % Check if destination is empty
    FromRow \= ToRow,                           % Cannot stay in the same row
    % Calculate the number of steps
    Steps is WhiteCount - BlackCount,
    Steps > 0,                                  % Steps must be greater than 0
    % Check if the move follows the diagonal path
    diagonal_path(FromRow, FromCol, ToRow, ToCol),
    % Check if the move respects the step count
    count_steps(FromRow, FromCol, ToRow, ToCol, Steps).

% Predicate to check if a move follows the diagonal path.
diagonal_path(Row1, Col1, Row2, Col2) :-
    DiffRow is abs(Row1 - Row2),
    DiffCol is abs(Col1 - Col2),
    DiffRow = DiffCol.

% Predicate to check if a move respects the step count.
count_steps(Row1, Col1, Row2, Col2, Steps) :-
    DiffRow is abs(Row1 - Row2),
    DiffCol is abs(Col1 - Col2),
    TotalSteps is DiffRow + DiffCol,
    TotalSteps = Steps.

% Predicate to apply a valid move and update the game state.
apply_move(State, Move, NewState) :- % Move = move(FromRow, FromCol, ToRow, ToCol)
    state(Board, Player, WhiteCount, BlackCount) = State,
    get_piece(Board, FromRow, FromCol, Player), % Check if moving own piece
    within_bounds(ToRow, ToCol),                 % Check if destination is within bounds
    get_piece(Board, ToRow, ToCol, empty),       % Check if destination is empty
    FromRow \= ToRow,                           % Cannot stay in the same row
    % Calculate the number of steps
    Steps is WhiteCount - BlackCount,
    Steps > 0,                                  % Steps must be greater than 0
    % Check if the move follows the diagonal path
    diagonal_path(FromRow, FromCol, ToRow, ToCol),
    % Check if the move respects the step count
    count_steps(FromRow, FromCol, ToRow, ToCol, Steps),
    % Update the board
    set_piece(Board, ToRow, ToCol, Player, TempBoard),
    set_piece(TempBoard, FromRow, FromCol, empty, NewBoard),
    % Switch player
    switch_player(Player, NewPlayer),
    % Update counts
    NewWhiteCount is WhiteCount - 1,
    NewBlackCount is BlackCount - 1,
    % Create the new state
    NewState = state(NewBoard, NewPlayer, NewWhiteCount, NewBlackCount).

% Predicate to switch the player.
switch_player(white, black).
switch_player(black, white).
