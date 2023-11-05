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



% Predicate to play the game.
start_game :-
    initial_state(Board, white),
    game_loop(state(Board, white)).

% Inside your game loop
game_loop(state(Board, Player)) :-
    print_game_state(state(Board, Player)),
    %print_player_positions(Board, Player),
    valid_moves_for_current_player(state(Board, Player), ValidMoves),
    length(ValidMoves, NumberOfMoves),
    write(NumberOfMoves), nl, 
    %print_moves(ValidMoves),
    get_move(state(Board, Player), move(FromRow, FromCol, ToRow, ToCol)), 
    apply_move(state(Board, Player), move(FromRow, FromCol, ToRow, ToCol), NewState),
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

    % Check if destination is empty
    get_piece(Board, ToRow, ToCol, none), 

    % Check if move follows a diagonal path
    diagonal_path(FromRow, FromCol, ToRow, ToCol, Diagonal),
    % !, write('Valid move!'), nl,
    write('Valid move on diagonal: '), write(Diagonal), nl,

    % Count pieces on diagonal
    count_pieces_in_diagonal(Board, FromRow, FromCol, ToRow, ToCol, WhiteCount, BlackCount),
    
    write('Number of white pieces: '), write(WhiteCount), nl,
    write('Number of Black pieces: '), write(BlackCount), nl.

    Steps is PlayerCount - OpponentCount,
    Steps > 0,
    Steps =:= abs(ToRow-FromRow),
    write('Valid move!'), nl.

% Find all valid moves from a position
valid_moves_from_position(state(Board, Player), FromRow, FromCol, ValidMoves) :-
    findall(
        move(FromRow, FromCol, ToRow, ToCol),
        (
            valid_move(state(Board, Player), move(FromRow, FromCol, ToRow, ToCol))
        ),
        ValidMoves
    ). 

% Predicate to count the number of player's and opponent's pieces on a diagonal.
count_pieces_on_diagonal(Board, FromRow, FromCol, ToRow, ToCol, Player, PlayerCount, OpponentCount) :-
    count_pieces_on_diagonal(Board, FromRow, FromCol, ToRow, ToCol, Player, 0, 0, PlayerCount, OpponentCount).

% Count pieces on diagonal
count_pieces_in_diagonal(Board, FromRow, FromCol, ToRow, ToCol, WhiteCount, BlackCount) :-
    get_all_positions(Board, AllPositions), % Retrieve all positions from the board
    
    % Iterate over all positions and count the pieces on the diagonal path
    count_diagonal_pieces(AllPositions, FromRow, FromCol, ToRow, ToCol, 0, 0, WhiteCount, BlackCount).


% % Recursive predicate to count player and opponent pieces on the diagonal
% count_diagonal_pieces([], _, _, _, _, WhiteCount, BlackCount, WhiteCount, BlackCount).
% count_diagonal_pieces([(Row, Col, Piece)|Rest], FromRow, FromCol, ToRow, ToCol, CurrentWhiteCount, CurrentBlackCount, WhiteCount, BlackCount) :-

%     ( 
%         diagonal_path(FromRow, FromCol, Row, Col, _),
%         diagonal_path(ToRow, ToCol, Row, Col, _)
%     ) -> (
%         write('Position validated as a diagonal: '), write(Row), write(','), write(Col), nl,
%         (
%             Piece == white -> NewWhiteCount is CurrentWhiteCount + 1, NewBlackCount = CurrentBlackCount
%         ;
%             Piece == black -> NewBlackCount is CurrentBlackCount + 1, NewWhiteCount = CurrentWhiteCount
%         ;
%             NewWhiteCount = CurrentWhiteCount, NewBlackCount = CurrentBlackCount % No piece or some other piece
%         )
%     ) ; (
%         write('Position NOT validated as a diagonal: '), write(Row), write(','), write(Col), nl,
%         NewWhiteCount = CurrentWhiteCount, NewBlackCount = CurrentBlackCount
%     ),
    
%     % Recurse over the rest of the positions
%     count_diagonal_pieces(Rest, FromRow, FromCol, ToRow, ToCol, NewWhiteCount, NewBlackCount, WhiteCount, BlackCount).

count_diagonal_pieces([], _, _, _, _, WhiteCount, BlackCount, WhiteCount, BlackCount).
count_diagonal_pieces([(Row, Col, Piece)|Rest], FromRow, FromCol, ToRow, ToCol, CurrentWhiteCount, CurrentBlackCount, WhiteCount, BlackCount) :-
    ( 
        diagonal_path(FromRow, FromCol, Row, Col, _),
        diagonal_path(Row, Col, FromRow, FromCol, _),
        diagonal_path(ToRow, ToCol, Row, Col, _),
        diagonal_path(Row, Col, ToRow, ToCol, _)
    ),
    !, % Cut here ensures that we don't backtrack over the successful diagonal path.
    write('Position validated as a diagonal: '), write(Row), write(','), write(Col), nl,
    update_counts(Piece, CurrentWhiteCount, CurrentBlackCount, NewWhiteCount, NewBlackCount),
    count_diagonal_pieces(Rest, FromRow, FromCol, ToRow, ToCol, NewWhiteCount, NewBlackCount, WhiteCount, BlackCount).

count_diagonal_pieces([(Row, Col, _)|Rest], FromRow, FromCol, ToRow, ToCol, CurrentWhiteCount, CurrentBlackCount, WhiteCount, BlackCount) :-
    % For positions that are not on the diagonal path, we simply continue recursion without updating counts.
    count_diagonal_pieces(Rest, FromRow, FromCol, ToRow, ToCol, CurrentWhiteCount, CurrentBlackCount, WhiteCount, BlackCount).

update_counts(white, CurrentWhiteCount, CurrentBlackCount, NewWhiteCount, CurrentBlackCount) :-
    NewWhiteCount is CurrentWhiteCount + 1.

update_counts(black, CurrentWhiteCount, CurrentBlackCount, CurrentWhiteCount, NewBlackCount) :-
    NewBlackCount is CurrentBlackCount + 1.

update_counts(_, CurrentWhiteCount, CurrentBlackCount, CurrentWhiteCount, CurrentBlackCount).


%  (5,6,7,4)

% top of board
diagonal_path(Row1, Col1, Row2, Col2, right) :- % (4,4,5,4)
    (Row1 =<5, Row2=<5),
    (Col2=:=Col1).

% top of board
diagonal_path(Row1, Col1, Row2, Col2, left) :- % (4,2,3,1)
    (Row1 =<5, Row2=<5),
    ((Col2-Col1)=:=(Row2-Row1)).

% bottom of board
diagonal_path(Row1, Col1, Row2, Col2, right) :-
    (Row1 >= 5, Row2 >= 5),
    ((Col2-Col1) =:= -(Row2-Row1)).

% bottom of board
diagonal_path(Row1, Col1, Row2, Col2, left) :-
    (Row1 >= 5, Row2 >= 5),
    (Col2=:=Col1).

% crossing the middle of the board
diagonal_path(Row1, Col1, Row2, Col2, right) :-
    Row1<5,
    Row2>5,
    Col2=:=Col1+(5-Row1).

diagonal_path(Row1, Col1, Row2, Col2, right) :-
    Row1>5,
    Row2<5,
    Col2=:=Col1-(5-Row2).

% crossing the middle of the board
diagonal_path(Row1, Col1, Row2, Col2, left) :-
    Row1<5,
    Row2>5,
    Col2=:=Col1+(5-Row2).

diagonal_path(Row1, Col1, Row2, Col2, left) :-
    Row1>5,
    Row2<5,
    Col2=:=Col1-(5-Row1).


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

% Predicate to set a piece at a specific position on the board.
set_piece(Board, Row, Col, Piece, NewBoard) :-
    within_bounds(Row, Col),
    nth1(Row, Board, RowList, RestRows),
    nth1(Col, RowList, _, RestCols),
    nth1(Row, NewBoard, NewRowList, RestRows),
    nth1(Col, NewRowList, Piece, RestCols).

% Predicate to apply a valid move and update the game state.
apply_move(state(Board, Player), move(FromRow, FromCol, ToRow, ToCol), NewState) :- 
    % Update the board
    set_piece(Board, ToRow, ToCol, Player, TempBoard),
    set_piece(TempBoard, FromRow, FromCol, none, NewBoard),

    % Switch player
    switch_player(Player, NewPlayer),
    
    % Create the new state
    NewState = state(NewBoard, NewPlayer).

% Predicate to print all player positions
print_player_positions(Board, Player) :-
    find_all_player_positions(Board, Player, Positions),
    print_positions(Positions).
    
print_positions([]).
print_positions([(Row, Col, Player) | Rest]) :-
    format("Player ~w piece at row ~w, column ~w~n", [Player, Row, Col]),
    print_positions(Rest).
    

% Predicate to find all positions of a player's pieces
find_all_player_positions(Board, Player, Positions) :-
    find_all_player_positions(Board, Player, 1, 1, [], Positions).

% Base case: no more rows to check
find_all_player_positions([], _, _, _, Positions, Positions).

% Recursive case: check the current row for player's pieces
find_all_player_positions([Row|Rest], Player, RowIndex, ColIndex, Accumulator, Positions) :-
    find_player_positions_in_row(Row, Player, RowIndex, 1, [], RowPositions),
    NextRowIndex is RowIndex + 1,
    append(Accumulator, RowPositions, NewAccumulator),
    find_all_player_positions(Rest, Player, NextRowIndex, 1, NewAccumulator, Positions).

% Predicate to find player's pieces in a row
find_player_positions_in_row([], _, _, _, Positions, Positions).

% Recursive case: check the current cell for a player's piece
find_player_positions_in_row([Cell|Rest], Player, RowIndex, ColIndex, Accumulator, Positions) :-
    (Cell = Player ->
        append(Accumulator, [(RowIndex, ColIndex, Player)], NewAccumulator)
    ;
        NewAccumulator = Accumulator
    ),
    NextColIndex is ColIndex + 1,
    find_player_positions_in_row(Rest, Player, RowIndex, NextColIndex, NewAccumulator, Positions).

% Encontra todos os movimentos válidos a partir de todas as posições com peças do jogador atual
valid_moves_for_current_player(state(Board, Player), ValidMoves) :-
    find_all_player_positions(Board, Player, Positions),
    find_all_valid_moves_for_positions(state(Board, Player), Positions, [], ValidMoves).

% Encontra todos os movimentos válidos a partir de uma lista de posições
find_all_valid_moves_for_positions(_, [], ValidMoves, ValidMoves).
find_all_valid_moves_for_positions(State, [(FromRow, FromCol, Player) | Rest], Accumulator, ValidMoves) :-
    valid_moves_from_position(State, FromRow, FromCol, MovesFromPosition),
    append(Accumulator, MovesFromPosition, NewAccumulator),
    find_all_valid_moves_for_positions(State, Rest, NewAccumulator, ValidMoves).



% Predicate to print al list of moves
print_moves([]).
print_moves([Move | Rest]) :-
    write(Move), nl,
    print_moves(Rest).

% Predicate that checks if the game is over
game_over(GameState, white) :-
    % Check if a white piece has reached row 1
    piece_reached_row(GameState, white, 1).

game_over(GameState, black) :-
    % Check if a black piece has reached row 9
    piece_reached_row(GameState, black, 9).

% Current player is loser, Opponent is winner
%game_over(state(Board, white), black) :-
    % Check if there are no more valid moves for the current player
%    valid_moves(state(Board, white), Moves),
%    Moves == [],
%    write('No more valid moves for white :(').

% Current player is loser, Opponent is winner
%game_over(state(Board, black), white) :-
%    % Check if there are no more valid moves for the current player
%    valid_moves(state(Board, black), Moves),
%    Moves == [],
%    write('No more valid moves for black :(').


% Predicate to check if a piece of the given color has reached the specified row
piece_reached_row(state(Board, _), Player, Row) :-
    nth1(Row, Board, RowList),
    member(Player, RowList).

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