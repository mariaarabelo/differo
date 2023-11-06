# README for DIFFERO Game Implementation in Prolog

## Group Information
- **Group Designation:** Differo_1
- **Student Number (Contributions):**
  - Maria Eduarda Sousa Rabelo up202000130 (50%)
  - Vitor Alves Moreira up201900198 (50%)

## Installation and Execution
Other than SICStus Prolog 4.8 , no other software is required to execute the game. To run the program, you should follow these steps:

1. Open Sicstus.
2. Consult main.pl, located on the src folder.
3. Call the play predicate without any arguments.

## Description of the game
DIFFERO is a two-player board game played on a hexagonal board with 5 spaces on each side, and each player has 13 pieces. You win when one of your pieces reaches the goal or when the opponent doesn't have any more moves allowed.

Rules of DIFFERO:
1. The piece moves in a diagonal line.
2. The piece can jump over any number of pieces at once, whether they are yours or your opponent's.
3. You can take (number of your pieces) - (number of opponent's pieces) steps on the diagonal to be moved.
4. If this value is less than or equal to 0, the piece cannot move on the diagonal.
5. You cannot move the piece off the board or into a place already occupied by another piece.
6. You cannot move the piece into the opponent's goal.

Information was collected from https://boardgamegeek.com/boardgame/375056/differo.



## Game Logic
The DIFFERO game in Prolog has been implemented with the following key components and features:

### Internal Game State Representation
To keep track of the game's internal state, we make use of a GameState data structure that is composed by the following structure:
- Board: A list of lists, where each element of the main list represents a row of the board, and each element of a inner list represents a column of that row, containing atoms 'none,' 'white,' and 'black' for empty spaces, white pieces, and black pieces, respectively.
- Player: The current player (white or black).



The GameState data structure is initialized with the initial board and white player:

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

% Initial game state
initial_state(Board, white) :- initial_board(Board).

### Game Loop
The `game_loop/2` predicate handles the main gameplay loop, allowing players to make moves and checks for game over conditions.

game_loop(state(Board, Player), Player1Type, Player2Type) :-

    print_game_state(state(Board, Player)),
    
    (
        Player == white -> Opponent = Player1Type;
        Player == black -> Opponent = Player2Type
    ),
    choose_move(state(Board, Player), Opponent, Player, Move),
    move(state(Board, Player), Move, NewState),
    (
        game_over(NewState, Winner), % Check if game is over
        print_game_state(NewState), % Print final state
        write('Player '), write(Winner), write(' won!'), nl
    ;
        game_loop(NewState, Player1Type, Player2Type) % Continue game if not over
    ).

### Game State Visualization
The game has a hexagonal board that is printed before every player move. 

- print_game_state(GameState) predicate displays the game state.
- print_board/1 predicate displays the game board.
- display_rules/0 predicate displays the game rules.

User move is gotten with the get_move(GameState, Move) predicate.

get_move(state(Board, Player), Move) :-
    repeat,
    nl,
    write('Player '), write(Player), 
    write(' move: (e.g., move(1,1,2,2).)'), nl,
    read(Move),
    valid_move(state(Board, Player), Move),
    !.


### Move Validation and Execution

- The game enforces rules for valid moves, checking if a move respects the following:
    - Piece in the original position does belong to current player.
    - 
  - Within bounds of the board.
  - Moving along a diagonal path.
  - Following the steps allowed based on the count of pieces on the diagonal path.
  - Not moving onto an already occupied space.


valid_move(state(Board, Player), move(FromRow, FromCol, ToRow, ToCol)) :-
    get_piece(Board, FromRow, FromCol, Player),
    get_piece(Board, ToRow, ToCol, none),
    diagonal_path(FromRow, FromCol, ToRow, ToCol),
    count_pieces_in_diagonal(Board, FromRow, FromCol, ToRow, ToCol, WhiteCount, BlackCount),
    valid_steps(Player, FromRow, ToRow, WhiteCount, BlackCount).

- The move/3 predicate executes valid moves to produce a new game state.

move(state(Board, Player), move(FromRow, FromCol, ToRow, ToCol), state(NewBoard, NewPlayer)) :- 
    % Update the board
    set_piece(Board, ToRow, ToCol, Player, TempBoard),
    set_piece(TempBoard, FromRow, FromCol, none, NewBoard),

    % Switch player
    switch_player(Player, NewPlayer).


### List of Valid Moves
- valid_moves/3 computes all valid moves for the current player.

valid_moves(state(Board, Player), ValidMoves) :-
    find_all_player_positions(Board, Player, Positions),
    find_all_valid_moves_for_positions(state(Board, Player), Positions, [], ValidMoves).

find_all_valid_moves_for_positions(_, [], ValidMoves, ValidMoves).
find_all_valid_moves_for_positions(State, [(FromRow, FromCol, Player) | Rest], Accumulator, ValidMoves) :-
    valid_moves_from_position(State, FromRow, FromCol, MovesFromPosition),
    append(Accumulator, MovesFromPosition, NewAccumulator),
    find_all_valid_moves_for_positions(State, Rest, NewAccumulator, ValidMoves).

valid_moves_from_position(state(Board, Player), FromRow, FromCol, ValidMoves) :-
    findall(
        move(FromRow, FromCol, ToRow, ToCol),
        valid_move(state(Board, Player), move(FromRow, FromCol, ToRow, ToCol)),
        MovesWithDuplicates
    ),
    remove_duplicates(MovesWithDuplicates, ValidMoves).


### End of Game
- game_over/2 identifies the game's end and the winner.

In order to verify that, we check if any piece of the current player reached its goal.

game_over(GameState, white) :-
    piece_reached_row(GameState, white, 1).

game_over(GameState, black) :-
    piece_reached_row(GameState, black, 9).

We also check if the currect player's list of available move is empty.

game_over(state(Board, white), black) :-
    valid_moves(state(Board, white), ValidMoves),
    ValidMoves == [],
    write('No more valid moves for white :(').

game_over(state(Board, black), white) :-
   valid_moves(state(Board, black), ValidMoves),
   ValidMoves == [],
   write('No more valid moves for black :(').


### Game State Evaluation
- value/3 assesses the game state, potentially used for AI decision-making. 
????????????w

### Computer Plays
- choose_move/4 allows the computer to make moves based on a given level (1: random, 2: greedy)
??????????????ww

## Conclusions
This DIFFERO Prolog implementation offers a functional game with core features. Known issues include [describe any known issues]. Future improvements may involve [mention potential enhancements].

## Bibliography
- https://boardgamegeek.com/boardgame/375056/differo
- https://sicstus.sics.se

