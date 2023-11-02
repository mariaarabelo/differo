:- use_module(library(between)).
:- use_module(library(lists)).

% Initialize the initial game board with a size of 5 on each side.
empty_board([
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

%state(Board, Player, WhiteCount, BlackCount).


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
initial_state(Board, white) :- empty_board(Board).

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

print_game_state(State) :-
    state(Board, Player) = State,
    print_board(Board),
    nl,
    write('Current Player: '), write(Player), nl.

print_board(Board) :-
    nl,
    maplist(print_row, Board).

print_spaces(0).
print_spaces(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    print_spaces(N1).

print_row(Row) :-
    write('    '), % spacing for visual clarity
    length(Row, Length),
    RowLength is (9 - Length),
    print_spaces(RowLength),
    print_cells(Row),
    nl.

print_cells([]).
print_cells([Cell]) :-
    print_cell(Cell).
print_cells([Cell | Rest]) :-
    print_cell(Cell),
    write(' '), % space between cells
    print_cells(Rest).

print_cell(none) :- write('.'). % use a dot for empty cells for better visibility
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
    apply_move(State, move(FromRow, FromCol, ToRow, ToCol), NewState),
    game_loop(NewState).

%Predicate to get the move from the player.
get_move(State, move(FromRow, FromCol, ToRow, ToCol)) :-
    repeat,
    state(Board, Player) = State,
    write('Player '), write(Player), write(' move: (e.g., move(1,1,2,2))'),
    read(move(FromRow, FromCol, ToRow, ToCol)),
    (
        valid_move(State,move(FromRow, FromCol, ToRow, ToCol)) ->
        write('Valid move!'), !; 
        write('Invalid move! Try again.'), nl,
        fail
    ).

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
valid_move(State, move(FromRow, FromCol, ToRow, ToCol)) :- 
    state(Board, Player) = State,
    get_piece(Board, FromRow, FromCol, Player), % Check if moving own piece
    within_bounds(ToRow, ToCol),                 % Check if destination is within bounds
    get_piece(Board, ToRow, ToCol, none),       % Check if destination is empty
    FromRow \= ToRow,                           % Cannot stay in the same row
        
    % Check if the move follows the diagonal path
    
    diagonal_path(FromRow, FromCol, ToRow, ToCol),
    write('that was a valid diagonal direction! :)'), nl, nl.

    % count pieces in diagonal (ainda falta)
    
    %count_pieces_in_diagonal(Board, FromRow, FromCol, ToRow, ToCol, Player, PlayerCount),
    %write('PlayerCount pieces in diagonal: '), write(PlayerCount), nl,
    %count_pieces_in_diagonal(Board, FromRow, FromCol, ToRow, ToCol, Opponent, OpponentCount),
    %write('OpponentCount pieces in diagonal: '), write(Opponent), nl,
    
    %Steps is PlayerCount - OpponentCount,   %calculate steps
    %Steps > 0,    

    % Check if the move respects the step count
    %count_steps(FromRow,  ToRow,  Steps).

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

% Define a predicate that checks a condition.
%above_mid_line(row_index) :- row_index < 5.
%in_mid_line(row_index) :- row_index = 5.
%below_mid_line(row_index) :-row_index > 5.
 
% Predicate to check if a move follows the diagonal path.
diagonal_path(Row1, Col1, Row2, Col2) :- 
    Row1 < 5,
    (
        (Row2 =:= Row1 - 1, Col2 =:= Col1 - 1); % Up and left
        (Row2 =:= Row1 - 1, Col2 =:= Col1);     % Up and right
        (Row2 =:= Row1 + 1, Col2 =:= Col1);     % Down and left
        (Row2 =:= Row1 + 1, Col2 =:= Col1 + 1)  % Down and right
    ),
    !.

diagonal_path(Row1, Col1, Row2, Col2) :- 
    Row1 = 5,
    (
        (Row2 =:= Row1 - 1, Col2 =:= Col1 - 1); % Up and left
        (Row2 =:= Row1 - 1, Col2 =:= Col1);     % Up and right
        (Row2 =:= Row1 + 1, Col2 =:= Col1 - 1); % Down and left
        (Row2 =:= Row1 + 1, Col2 =:= Col1)      % Down and right
    ),
    !.

diagonal_path(Row1, Col1, Row2, Col2) :- 
    Row1>5,  
    (
        (Row2 =:= Row1 - 1, Col2 =:= Col1);     % Up and left
        (Row2 =:= Row1 - 1, Col2 =:= Col1 + 1); % Up and right
        (Row2 =:= Row1 + 1, Col2 =:= Col1-1);   % Down and left
        (Row2 =:= Row1 + 1, Col2 =:= Col1)      % Down and right
    ),
    !.


% Predicate to apply a valid move and update the game state.
apply_move(State, move(FromRow, FromCol, ToRow, ToCol), NewState) :- % Move = move(FromRow, FromCol, ToRow, ToCol)
    state(Board, Player) = State,
    
     % Update the board
    set_piece(Board, ToRow, ToCol, Player, TempBoard),
    set_piece(TempBoard, FromRow, FromCol, none, NewBoard),
    % Switch player
    switch_player(Player, NewPlayer),
    
    % Create the new state
    NewState = state(NewBoard, NewPlayer).



