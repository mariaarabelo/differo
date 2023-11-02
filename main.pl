:- use_module(library(between)).
:- use_module(library(lists)).

% Initialize the initial game board with a size of 5 on each side.
initial_board([
    [none, none, none, none, none],
    [none, black, black, black, black, none],
    [none, black, none, black, none, black, none],
    [none, black, black, black, black, black, black, none],
    [none, none, none, none, none, none, none, none, none],
    [none, white, white, white, white, white, white, none],
    [none,  white, none, white, none, white, none],
    [none, white, white, white, white, none],
    [none, none, none, none, none]
]).

% ALL CHECKS FOR POSSIBLE MOVES:
% 1. Check if the row_index is above, in or below the middle line.
% 2. Check places above the current line.
% 3. Check places below the current line.
% 4. Check if the piece can jump other pieces.
% 5. Check how many moves you can do in a row.

% Predicate to switch the player.
switch_player(white, black).
switch_player(black, white).

% Initial game state
initial_state(Board, white) :- initial_board(Board).

play :-
    nl,
    write('1. Play'), nl,
    write('2. Exit'), nl,
    read(Option),
    nl,
    (
        Option = 1 -> start_game;
        Option = 2 ->  write('Exiting the game.'), nl
    ).

print_game_state(state(Board, Player)) :-
    print_board(Board),
    nl,
    write('Current Player: '), write(Player), nl.

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

print_cell(none) :- write(' ').
print_cell(white) :- write('W').
print_cell(black) :- write('B').

% Predicate to play the game.
start_game :-
    initial_state(Board, white),
    game_loop(state(Board, white)).

% Inside your game loop
game_loop(State) :-
    print_game_state(State),
    get_move(State, move(FromRow, FromCol, ToRow, ToCol)), 
    !,
    apply_move(State, move(FromRow, FromCol, ToRow, ToCol), NewState),
    game_loop(NewState).

%Predicate to get the move from the player.
get_move(state(Board, Player), Move) :-
    repeat,
    nl,
    write('Player '), write(Player), 
    write(' move: (e.g., move(1,1,2,2).)'), nl,
    read(Move),
    valid_move(state(Board, Player), Move),
    !.

% Check if the move is valid.
valid_move(state(Board, Player), move(FromRow, FromCol, ToRow, ToCol)) :-
    
    % Check if the piece belongs to the player
    get_piece(Board, FromRow, FromCol, Player),
    !,
    write('Valid piece!'), nl,

    % Check if destination is empty
    get_piece(Board, ToRow, ToCol, none), 
    !,
    write('Valid empty destination!'), nl, 

    % Check if move follows a diagonal path     
    diagonal_path(FromRow, FromCol, ToRow, ToCol),
    !, write('Valid move!'), nl.
    
    % count pieces in diagonal (ainda falta)
    
    %count_pieces_in_diagonal(Board, FromRow, FromCol, ToRow, ToCol, Player, PlayerCount),
    %write('PlayerCount pieces in diagonal: '), write(PlayerCount), nl,
    %count_pieces_in_diagonal(Board, FromRow, FromCol, ToRow, ToCol, Opponent, OpponentCount),
    %write('OpponentCount pieces in diagonal: '), write(Opponent), nl,
    
    %Steps is PlayerCount - OpponentCount,   %calculate steps
    %Steps > 0,    

    % Check if the move respects the step count
    %count_steps(FromRow,  ToRow,  Steps).


% check if position is within bound
within_bounds(Row, Col) :-
    between(1, 5, Row),        
    MaxCols is 4 + Row,                     
    % Para as primeiras 5 linhas
    between(1, MaxCols, Col).
    
within_bounds(Row, Col) :-
    between(5, 9, Row), 
    % Para as linhas restantes
    MaxCols is 14 - Row,
    between(1, MaxCols, Col).

% Predicate to get the piece at a specific position on the board.
get_piece(Board, Row, Col, Piece) :-
    within_bounds(Row, Col),
    nth1(Row, Board, RowList),
    nth1(Col, RowList, Piece).

% Error handle for get_piece
get_piece(_, _, _, _) :-
    write('Invalid position!'), nl,
    fail.

% Predicate to set a piece at a specific position on the board.
set_piece(Board, Row, Col, Piece, NewBoard) :-
    within_bounds(Row, Col),
    nth1(Row, Board, RowList, RestRows),
    nth1(Col, RowList, _, RestCols),
    nth1(Row, NewBoard, NewRowList, RestRows),
    nth1(Col, NewRowList, Piece, RestCols).

% Predicate para contar as peças na diagonal entre duas coordenadas.
count_pieces_in_diagonal(Board, Row1, Col1, Row2, Col2, Player, Count) :-
    count_pieces_in_diagonal(Board, Row1, Col1, Row2, Col2, Player, 0, Count).

% Caso base: Quando chegarmos à coordenada de destino.
count_pieces_in_diagonal(_, Row1, Col1, Row2, Col2, _, Count, Count) :-
    Row1 =:= Row2,
    Col1 =:= Col2.

% Movimente-se ao longo da diagonal e conte as peças.
count_pieces_in_diagonal(Board, Row1, Col1, Row2, Col2, Player, CurrentCount, Count) :-
    NewRow1 is (Row1 < Row2 -> Row1 + 1 ; Row1 - 1),
    NewCol1 is (Col1 < Col2 -> Col1 + 1 ; Col1 - 1),
    get_piece(Board, NewRow1, NewCol1, Piece),
    (Piece = Player -> NewCount is CurrentCount + 1 ; NewCount is CurrentCount),
    count_pieces_in_diagonal(Board, NewRow1, NewCol1, Row2, Col2, Player, NewCount, Count).

% Predicate to check if a move respects the step count.
count_steps(Row1, Row2, Steps) :-
    Steps >= abs(Row1 - Row2).

% top of board
diagonal_path(Row1, Col1, Row2, Col2) :-
    (Row1 =<5, Row2=<5),
    (((Col2-Col1)=:=(Row2-Row1));
    (Col2=:=Col1)),
    write('Valid diagonal!'), nl.

% bottom of board
diagonal_path(Row1, Col1, Row2, Col2) :-
    (Row1 >= 5, Row2 >= 5),
    (((Col2-Col1) =:= -(Row2-Row1));
    (Col2=:=Col1)),
    write('Valid diagonal!'), nl.

% crossing the middle of the board
diagonal_path(Row1, Col1, Row2, Col2) :-
    ((Row1=<5, Row2>5);
    (Row1>=5, Row2<5)),
    ((Col2==(Col1+(5-Row1)));
    (Col2==(Col1+(5-Row2)))),
    write('Valid diagonal!'), nl.

diagonal_path(_Row1, _Col1, _Row2, _Col2) :-
    write('Invalid diagonal! Try again.'), nl,
    fail.

% Predicate to apply a valid move and update the game state.
apply_move(state(Board, Player), move(FromRow, FromCol, ToRow, ToCol), NewState) :- 
    % Update the board
    set_piece(Board, ToRow, ToCol, Player, TempBoard),
    set_piece(TempBoard, FromRow, FromCol, none, NewBoard),

    % Switch player
    switch_player(Player, NewPlayer),
    
    % Create the new state
    NewState = state(NewBoard, NewPlayer).



