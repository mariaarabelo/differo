:- use_module(library(between)).
:- use_module(library(lists)).
:- use_module(library(random)).
:- use_module(library(system)).

% Seed the random number generator
seed_random :-
    datime(datime(_, _, _, Hour, Min, Sec)), % Get current time components
    Seed is (Hour * 3600 + Min * 60 + Sec),    % Calculate a seed value
    setrand(Seed).                            % Seed the random generator


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

% Display rules for differo
display_rules:-
    write('DIFFERO Game'), nl,
    write('A game for two players on a hexagonal board with 5 spaces on each side.'), nl,
    write('Each player has 13 pieces.'), nl,
    write('Victory: You win when one of your pieces reaches the goal :)'), nl,
    write('Defeat: You lose if you cannot move any of your pieces on your turn :('), nl, nl,
    write('Rules:'), nl,
    write('1. The piece moves in a diagonal line.'), nl,
    write('2. The piece can jump over any number of pieces at once, whether they are yours or your opponent\'s.'), nl,
    write('3. You can take (number of your pieces) - (number of opponent\'s pieces) steps on the diagonal to be moved.'), nl,
    write('4. If this value is less than or equal to 0, the piece cannot move on the diagonal.'), nl,
    write('5. You cannot move the piece off the board or into a place already occupied by another piece.'), nl,
    write('6. You cannot move the piece into the opponent\'s goal.'), nl,
    write('Good luck and enjoy playing DIFFERO!'), nl, nl.

% Predicate to switch the player.
switch_player(white, black).
switch_player(black, white).

% Initial game state
initial_state(Board, white) :- initial_board(Board).

play :-
    seed_random,
    nl,
    display_rules,
    show_options,
    read(Option),
    handle_option(Option).

show_options :-
    write('1. Human vs Human'), nl,
    write('2. Human vs Bot'), nl,
    write('3. Bot vs Bot'), nl,
    write('4. Exit'), nl.

handle_option(1) :- start_game(human, human).
handle_option(2) :- start_game(human, bot).
handle_option(3) :- start_game(bot, bot).
handle_option(4) :- write('Exiting the game.'), nl.
handle_option(_) :-
    write('Invalid option'), nl,
    play.

% Rest of the predicates like seed_random, display_rules, start_game, etc. remain the same.


% Predicate to play the game.
start_game(Player1Type, Player2Type) :-
    initial_state(Board, white),
    game_loop(state(Board, white), Player1Type, Player2Type).

% Inside your game loop
game_loop(State, Player1Type, Player2Type) :-
    State = state(Board, Player),
    print_board(Board), nl,
    write('Current Player: '), write(Player), nl,
    opponent(Player, Player1Type, Player2Type, Opponent),
    choose_move(State, Opponent, Player, Move),
    move(State, Move, NewState),
    check_game_status(NewState, Player1Type, Player2Type).

opponent(white, Player1Type, _, Player1Type).
opponent(black, _, Player2Type, Player2Type).

check_game_status(state(NewBoard, NewPlayer), Player1Type, Player2Type) :-
    (
        game_over(state(NewBoard, NewPlayer), Winner), % Check if game is over
        print_board(NewBoard), nl,
        write('Player '), write(Winner), write(' won!'), nl
    ;
        game_loop(state(NewBoard, NewPlayer), Player1Type, Player2Type) % Continue game if not over
    ).

% Adjust the choose_move predicate to handle difficulty levels
choose_move(state(Board, Player), bot, Player, Move) :-
    valid_moves(state(Board, Player), ValidMoves),
    random_member(Move, ValidMoves),
    nl, write('Bot '), write(Player), write(' move: '), write(Move), nl.

choose_move(state(Board, Player), human, Player, Move) :-
    valid_moves(state(Board, Player), ValidMoves),
    length(ValidMoves, NumberOfMoves),
    format("Player ~w has ~w possible moves~n", [Player, NumberOfMoves]),
    print_moves(ValidMoves),
    get_move(state(Board, Player), Move).

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
    diagonal_path(FromRow, FromCol, ToRow, ToCol),
    % !, write('Valid move!'), nl,

    % Count pieces on diagonal
    count_pieces_in_diagonal(Board, FromRow, FromCol, ToRow, ToCol, WhiteCount, BlackCount),
    
    % write('Number of white pieces: '), write(WhiteCount), nl,
    % write('Number of black pieces: '), write(BlackCount), nl,
    valid_steps(Player, FromRow, ToRow, WhiteCount, BlackCount),
    piece_reaches_opponent_goal(state(Board, Player), ToRow).
    % write('Valid move!'), nl.

piece_reaches_opponent_goal(state(_Board, white), ToRow):-
    ToRow \= 9.

piece_reaches_opponent_goal(state(_Board, black), ToRow):-
    ToRow \= 1.

valid_steps(white, FromRow, ToRow, WhiteCount, BlackCount) :-
    Steps is WhiteCount - BlackCount,
    Steps > 0,
    Steps =:= abs(FromRow-ToRow).

valid_steps(black, FromRow, ToRow, WhiteCount, BlackCount) :-
    Steps is BlackCount - WhiteCount,
    Steps > 0,
    Steps =:= abs(ToRow-FromRow).


% Count pieces on diagonal
count_pieces_in_diagonal(Board, FromRow, FromCol, ToRow, ToCol, WhiteCount, BlackCount) :-
    get_all_positions(Board, AllPositions), % Retrieve all positions from the board
    
    % Iterate over all positions and count the pieces on the diagonal path
    count_diagonal_pieces(AllPositions, FromRow, FromCol, ToRow, ToCol, 0, 0, WhiteCount, BlackCount),!.

% Predicate to get all positions with their contents on the board
get_all_positions(Board, AllPositions) :-
    findall(
        (Row, Col, Content),
        (
            nth1(Row, Board, BoardRow),        % Get the row with index
            nth1(Col, BoardRow, Content)       % Get the content with index
        ),
        AllPositions
    ).

count_diagonal_pieces([], _, _, _, _, WhiteCount, BlackCount, WhiteCount, BlackCount).
count_diagonal_pieces([(Row, Col, Piece)|Rest], FromRow, FromCol, ToRow, ToCol, CurrentWhiteCount, CurrentBlackCount, WhiteCount, BlackCount) :-
    ( 
        diagonal_path(FromRow, FromCol, Row, Col),
        diagonal_path(Row, Col, FromRow, FromCol),
        diagonal_path(ToRow, ToCol, Row, Col),
        diagonal_path(Row, Col, ToRow, ToCol)
    ),
    !, % Cut here ensures that we don't backtrack over the successful diagonal path.
    % write('Position validated as a diagonal: '), write(Row), write(','), write(Col), nl,
    update_counts(Piece, CurrentWhiteCount, CurrentBlackCount, NewWhiteCount, NewBlackCount),
    count_diagonal_pieces(Rest, FromRow, FromCol, ToRow, ToCol, NewWhiteCount, NewBlackCount, WhiteCount, BlackCount).

count_diagonal_pieces([(_Row, _Col, _)|Rest], FromRow, FromCol, ToRow, ToCol, CurrentWhiteCount, CurrentBlackCount, WhiteCount, BlackCount) :-
    % For positions that are not on the diagonal path, we simply continue recursion without updating counts.
    count_diagonal_pieces(Rest, FromRow, FromCol, ToRow, ToCol, CurrentWhiteCount, CurrentBlackCount, WhiteCount, BlackCount), !.

update_counts(white, CurrentWhiteCount, CurrentBlackCount, NewWhiteCount, CurrentBlackCount) :-
    NewWhiteCount is CurrentWhiteCount + 1.

update_counts(black, CurrentWhiteCount, CurrentBlackCount, CurrentWhiteCount, NewBlackCount) :-
    NewBlackCount is CurrentBlackCount + 1.

update_counts(_, CurrentWhiteCount, CurrentBlackCount, CurrentWhiteCount, CurrentBlackCount).

% top of board
diagonal_path(Row1, Col1, Row2, Col2) :-
    (Row1 =<5, Row2=<5),
    (Col2=:=Col1).

% top of board
diagonal_path(Row1, Col1, Row2, Col2) :-
    (Row1 =<5, Row2=<5),
    ((Col2-Col1)=:=(Row2-Row1)).

% bottom of board
diagonal_path(Row1, Col1, Row2, Col2) :-
    (Row1 >= 5, Row2 >= 5),
    ((Col2-Col1) =:= -(Row2-Row1)).

% bottom of board
diagonal_path(Row1, Col1, Row2, Col2) :-
    (Row1 >= 5, Row2 >= 5),
    (Col2=:=Col1).

% crossing the middle of the board
diagonal_path(Row1, Col1, Row2, Col2) :-
    Row1<5,
    Row2>5,
    Col2=:=Col1+(5-Row1).

diagonal_path(Row1, Col1, Row2, Col2) :-
    Row1>5,
    Row2<5,
    Col2=:=Col1-(5-Row2).

% crossing the middle of the board
diagonal_path(Row1, Col1, Row2, Col2) :-
    Row1<5,
    Row2>5,
    Col2=:=Col1+(5-Row2).

diagonal_path(Row1, Col1, Row2, Col2) :-
    Row1>5,
    Row2<5,
    Col2=:=Col1-(5-Row1).

% check if position is within bounds for the first 5 rows
within_bounds(Row, Col) :-
    between(1, 5, Row),
    MaxCols is 4 + Row,
    between(1, MaxCols, Col).
    
% check if position is within bounds for the rows after 5
within_bounds(Row, Col) :-
    between(5, 9, Row),
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
move(state(Board, Player), move(FromRow, FromCol, ToRow, ToCol), state(NewBoard, NewPlayer)) :- 
    % Update the board
    set_piece(Board, ToRow, ToCol, Player, TempBoard),
    set_piece(TempBoard, FromRow, FromCol, none, NewBoard),

    % Switch player
    switch_player(Player, NewPlayer).

% Find all valid moves from a position
valid_moves_from_position(state(Board, Player), FromRow, FromCol, ValidMoves) :-
    findall(
        move(FromRow, FromCol, ToRow, ToCol),
        valid_move(state(Board, Player), move(FromRow, FromCol, ToRow, ToCol)),
        MovesWithDuplicates
    ),
    remove_duplicates(MovesWithDuplicates, ValidMoves).

remove_duplicates([], []).
remove_duplicates([H | T], ListWithoutDuplicates) :-
    member(H, T), !,
    remove_duplicates(T, ListWithoutDuplicates).

remove_duplicates([H | T], [H | ListWithoutDuplicates]) :-
    remove_duplicates(T, ListWithoutDuplicates).


% Predicate to print all player positions
print_player_positions(Board, Player) :-
    find_all_player_positions(Board, Player, Positions),
    print_positions(Positions).
    
print_positions([]).
print_positions([(Row, Col, Player) | Rest]) :-
    format("Player ~w piece at row ~w, column ~w~n", [Player, Row, Col]),
    print_positions(Rest).

find_all_player_positions(Board, Player, PlayerPositions) :-
    get_all_positions(Board, AllPositions),
    filter_player_positions(AllPositions, Player, PlayerPositions).


filter_player_positions(AllPositions, Player, PlayerPositions):-
    filter_player_positions(AllPositions, Player, [], PlayerPositions).

filter_player_positions([], _, PlayerPositions, PlayerPositions).
filter_player_positions([(Row, Col, Content)|T], Player, Accumulator, PlayerPositions) :-
    (Content = Player ->
        append(Accumulator, [(Row, Col, Player)], NewAccumulator)
    ;
        NewAccumulator = Accumulator
    ),
    filter_player_positions(T, Player, NewAccumulator, PlayerPositions).

% Encontra todos os movimentos válidos a partir de todas as posições com peças do jogador atual
valid_moves(state(Board, Player), ValidMoves) :-
    find_all_player_positions(Board, Player, Positions),
    find_all_valid_moves_for_positions(state(Board, Player), Positions, [], ValidMoves).

% Encontra todos os movimentos válidos a partir de uma lista de posições
find_all_valid_moves_for_positions(_, [], ValidMoves, ValidMoves).
find_all_valid_moves_for_positions(State, [(FromRow, FromCol, _Player) | Rest], Accumulator, ValidMoves) :-
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
game_over(state(Board, white), black) :-
    % Check if there are no more valid moves for the current player
    valid_moves(state(Board, white), ValidMoves),
    ValidMoves == [],
    write('No more valid moves for white :(').

% Current player is loser, Opponent is winner
game_over(state(Board, black), white) :-
   % Check if there are no more valid moves for the current player
   valid_moves(state(Board, black), ValidMoves),
   ValidMoves == [],
   write('No more valid moves for black :(').


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