:- use_module(library(between)).
:- use_module(library(lists)).

% Initialize the initial game board with a size of 5 on each side.
empty_board([
    [_, _, _, _, _],
    [_, black, black, black, black, _],
    [_, black, _, black, _, black, _],
    [_, black, black, black, black, black, black, _],
    [_, _, _, _, _, _, _, _, _],
    [_, white, white, white, white, white, white, _],
    [_,  white, _, white, _, white, _],
    [_, white, white, white, white, _],
    [_, _, _, _, _]
]).

%state(Board, Player, WhiteCount, BlackCount).

% Predicate to switch the player.
switch_player(white, black).
switch_player(black, white).

% Initial game state
initial_state(Board, white, 13, 13) :- empty_board(Board).


play :-
    nl,
    write('1. Play'), nl,
    write('2. Exit'), nl,
    read(Option),
    nl,
    (
        Option = 1 -> start_game;
        Option = 2 ->  write('Exiting the game.'), nl, halt
    ).

print_game_state(State) :-
    state(Board, Player, WhiteCount, BlackCount) = State,
    print_board(Board),
    nl,
    write('Current Player: '), write(Player), nl,
    write('White Pieces Remaining: '), write(WhiteCount), nl,
    write('Black Pieces Remaining: '), write(BlackCount), nl.

% Predicate to print the board.
print_board(Board) :-
    nl,
    write('   -------------------'), nl,
    print_board(Board, 1).

print_board([], _).

print_board([Row | RestRows], RowNumber) :-
    write(' '),
    write(RowNumber),
    write(' |'),
    print_row(Row),
    nl,
    NewRowNumber is RowNumber + 1,
    print_board(RestRows, NewRowNumber).

print_row(Row) :-
    length(Row, RowLength),
    print_cells(Row, 0, RowLength).

print_cells([], _, _).
print_cells([Cell | Rest], CellNumber, RowLength) :-
    print_cell(Cell),
    NextCellNumber is CellNumber + 1,
    (CellNumber < RowLength - 1 -> write(' '); true), % Add space between cells
    print_cells(Rest, NextCellNumber, RowLength).

print_cell(empty) :- write(' ').
print_cell(white) :- write('W').
print_cell(black) :- write('B').

% Predicate to play the game.
start_game :-
    initial_state(Board, white, 13, 13),
    game_loop(state(Board, white, 13, 13)).

% Inside your game loop
game_loop(State) :-
    print_game_state(State),
    get_move(State, Move),
    apply_move(State, Move, NewState),
    game_loop(NewState).


% check if position is within bound
within_bounds(Row, Col) :-
    between(1, 5, Row),        
    MaxCols is 4 + Row,                     
    % Para as primeiras 5 linhas
    between(1, MaxCols, Col).
    
within_bounds(Row, Col) :-
    between(5, 9, Row),                              % Para as linhas restantes
    MaxCols is 14 - Row,
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

%Predicate to get the move from the player.
get_move(State, Move) :-
    state(Board, Player, _, _) = State,
    write('Player '), write(Player), write(' move: (e.g., move(1,1,2,2)): '),
    read(Move),
    valid_move(State, Move).

