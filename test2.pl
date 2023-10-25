:- use_module(library(between)).
:- use_module(library(lists)).

% Define a term to represent the game state.
state(Board, Player, WhiteCount, BlackCount).

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

% Initial game state
initial_state(State) :- 
    empty_board(Board),
    State = state(Board, white, 13, 13).


play :-
    nl,
    write('1. Play'), nl,
    write('2. Exit'), nl,
    read(Option),
    nl,
    handle_option(Option).

handle_option(1) :-
    start_game.

handle_option(2) :-
    write('Exiting the game.'), nl,
    halt.

% Predicate to play the game.
start_game :-
    initial_state(GameState),  % This is how I execute a function and get the return value in a variable
    print_game_state(GameState).


print_game_state(State) :-
    state(Board, Player, _, _) = State, % This is how I get the values from an argument
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

print_cell(empty) :- write(' ').
print_cell(white) :- write('W').
print_cell(black) :- write('B').


