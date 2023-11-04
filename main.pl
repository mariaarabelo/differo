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

print_board(Board) :-
    nl,
    print_board_with_index(Board, 1).

print_board_with_index([], _). % Base case: no more rows
print_board_with_index([Row|Rest], Index) :-
    print_row(Row, Index),
    NextIndex is Index + 1,
    print_board_with_index(Rest, NextIndex).

print_spaces(0).
print_spaces(N) :-
    N > 0,
    write(' '),
    N1 is N - 1,
    print_spaces(N1).

print_row(Row, Index) :-
    write('     '), write(Index), % print the row index followed by a colon
    write('  '), % spacing for visual clarity
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
    State = state(Board, Player),
    print_game_state(State),
    get_move(State, move(FromRow, FromCol, ToRow, ToCol)), 
    apply_move(State, move(FromRow, FromCol, ToRow, ToCol), NewState),
    (
        game_over(NewState, Winner), % Check if game is over
        print_game_state(NewState), % Print final state
        write('Player '), write(Winner), write(' won!'), nl
    ;
        game_loop(NewState) % Continue game if not over
    ).

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
    %diagonal_path(FromRow, FromCol, ToRow, ToCol, Direction),
    !, write('Valid move!'), nl.
    
    % Count pieces on diagonal
%    count_pieces_on_diagonal(Board, FromRow, FromCol, ToRow, ToCol, Player, PlayerCount, OpponentCount),
%    Steps is PlayerCount - OpponentCount,
%    Steps > 0,
%    Steps =:= abs(ToRow-FromRow),
%    write('Valid move!'), nl.

% Predicate to count the number of player's and opponent's pieces on a diagonal.
count_pieces_on_diagonal(Board, FromRow, FromCol, ToRow, ToCol, Player, PlayerCount, OpponentCount) :-
    count_pieces_on_diagonal(Board, FromRow, FromCol, ToRow, ToCol, Player, 0, 0, PlayerCount, OpponentCount).

% Base case: When we reach the destination coordinates.
count_pieces_on_diagonal(_, Row, Col, Row, Col, _, _, PlayerCount, OpponentCount, PlayerCount, OpponentCount).

% Recursive case: Move along the diagonal and count the pieces.
count_pieces_on_diagonal(Board, FromRow, FromCol, ToRow, ToCol, Player, Opponent, CurrentPlayerCount, CurrentOpponentCount, PlayerCount, OpponentCount) :-
    NewRow is (ToRow > FromRow -> FromRow + 1 ; FromRow - 1),
    NewCol is (ToCol > FromCol -> FromCol + 1 ; FromCol - 1),
    get_piece(Board, NewRow, NewCol, Piece),
    (Piece = Player ->
        NewPlayerCount is CurrentPlayerCount + 1,
        NewOpponentCount is CurrentOpponentCount
    ;
        (Piece = Opponent ->
            NewPlayerCount is CurrentPlayerCount,
            NewOpponentCount is CurrentOpponentCount + 1
        ;
            NewPlayerCount is CurrentPlayerCount,
            NewOpponentCount is CurrentOpponentCount
        )
    ),
    count_pieces_on_diagonal(Board, NewRow, NewCol, ToRow, ToCol, Player, Opponent, NewPlayerCount, NewOpponentCount, PlayerCount, OpponentCount).


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
    ((Col2=:=(Col1+(5-Row1)));
    (Col2=:=(Col1+(5-Row2)));
    (Col2=:=(Col1-(5-Row1)));
    (Col2=:=(Col1-(5-Row2)))),
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

% Predicate to get all valid moves of a Player
valid_moves(state(Board, Player), ListOfMoves) :-
    findall(Move, valid_move(state(Board, Player), Move), ListOfMoves).

% Predicate that checks if the game is over
game_over(GameState, white) :-
    % Check if a white piece has reached row 1
    piece_reached_row(GameState, white, 1).

game_over(GameState, black) :-
    % Check if a black piece has reached row 9
    piece_reached_row(GameState, black, 9).

%game_over(state(Board, white), black) :-
%    % Check if there are no more valid moves for the current player
%    valid_moves(state(Board, white), Moves),
%    Moves == []. % No more valid moves 
% Current player is loser, Opponent is winner

%game_over(state(Board, black), white) :-
%    % Check if there are no more valid moves for the current player
%    valid_moves(state(Board, black), Moves),
%    Moves == []. % No more valid moves 
% Current player is loser, Opponent is winner
    

% Predicate to check if a piece of the given color has reached the specified row
piece_reached_row(state(Board, _), Player, Row) :-
    nth1(Row, Board, RowList),
    member(Player, RowList).
