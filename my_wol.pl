%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEST STRATEGY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test strategy predicate
test_strategy(Num_of_games, Player1_strat, Player2_strat) :-
  get_strategy_data(Num_of_games, 0, Player1_strat, Player2_strat, Blue_wins,
                    Red_wins, Longest_game, Shortest_game, Average_game_length,
                    Average_game_time),
  format('Blue wins:           ~w~n', [Blue_wins]),
  format('Red wins:            ~w~n', [Red_wins]),
  format('Longest game:        ~w~n', [Longest_game]),
  format('Shortest game:       ~w~n', [Shortest_game]),
  format('Average game length: ~w~n', [Average_game_length]),
  format('Average game time:   ~wms~n', [Average_game_time]).

% Helpers

%% base case
get_strategy_data(Num_of_games, Num_of_games, _, _,  0, 0, 0, 250, 0, 0).
%% recursion
get_strategy_data(Num_of_games, Num_simulations, Player1_strat, Player2_strat,
                  Blue_wins, Red_wins, Longest_game, Shortest_game,
                  Average_game_length, Average_game_time) :-
  statistics(runtime, [_,_]),
  play(quiet, Player1_strat, Player2_strat, Num_moves, Winner),
  statistics(runtime, [_,T]),
  New_num_simulations is Num_simulations + 1,
  get_strategy_data(Num_of_games, New_num_simulations, Player1_strat,
                    Player2_strat, Prev_blue_wins, Prev_red_wins,
                    Prev_longest_game, Prev_shortest_game,
                    Prev_average_game_length, Prev_average_game_time),
  update_wins(Winner, Prev_blue_wins, Blue_wins, Prev_red_wins, Red_wins),
  update_game_length_data(Winner, Prev_longest_game, Prev_shortest_game,
                          Longest_game, Shortest_game, Num_moves),
  update_averages(Prev_average_game_length, Num_moves, Prev_average_game_time,
                  T, Average_game_length, Average_game_time, Num_of_games).

update_averages(Prev_average_game_length, Num_moves, Prev_average_game_time, T,
                Average_game_length, Average_game_time, Num_of_games) :-
  Average_game_length is Num_moves / Num_of_games + Prev_average_game_length,
  Average_game_time is T / Num_of_games + Prev_average_game_time.

update_game_length_data('exhaust', Prev_longest_game, Prev_shortest_game,
                        Longest_game, Shortest_game, _) :-
  Longest_game is Prev_longest_game,
  Shortest_game is Prev_shortest_game,!.
update_game_length_data(_, Prev_longest_game, Prev_shortest_game,
                        Longest_game, Shortest_game, Num_moves) :-
  Longest_game is max(Prev_longest_game, Num_moves),
  Shortest_game is min(Prev_shortest_game, Num_moves).

update_wins(b, Curr_blue_wins, New_blue_wins, Curr_red_wins, New_red_wins) :-
  New_blue_wins is Curr_blue_wins + 1,
  New_red_wins is Curr_red_wins,!.
update_wins(r, Curr_blue_wins, New_blue_wins, Curr_red_wins, New_red_wins) :-
  New_blue_wins is Curr_blue_wins,
  New_red_wins is Curr_red_wins + 1,!.
update_wins(_, Curr_blue_wins, New_blue_wins, Curr_red_wins, New_red_wins) :-
  New_blue_wins is Curr_blue_wins,
  New_red_wins is Curr_red_wins.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STRATEGIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Bloodlust

bloodlust(Player, Curr_state, New_state, Move) :-
  find_all_poss_moves(Player, Curr_state, Poss_moves),
  best_move(bloodlust, Player, Curr_state, Poss_moves, _, Move, 64),
  new_board(Player, Move, Curr_state, New_state).

% Self Preservation

self_preservation(Player, Curr_state, New_state, Move) :-
  find_all_poss_moves(Player, Curr_state, Poss_moves),
  best_move(self_preservation, Player, Curr_state, Poss_moves, _, Move, -64),
  new_board(Player, Move, Curr_state, New_state).

% Land Grab

land_grab(Player, Curr_state, New_state, Move) :-
  find_all_poss_moves(Player, Curr_state, Poss_moves),
  best_move(land_grab, Player, Curr_state, Poss_moves, _, Move, -64),
  new_board(Player, Move, Curr_state, New_state).

% Helpers

find_all_poss_moves(b, [Curr_blue, Curr_red], Poss_moves) :-
  findall([A, B, MA, MB],
          (member([A,B], Curr_blue), neighbour_position(A, B, [MA, MB]),
           \+member([MA, MB], Curr_blue), \+member([MA, MB], Curr_red)),
          Poss_moves).
find_all_poss_moves(r, [Curr_blue, Curr_red], Poss_moves) :-
  findall([A, B, MA, MB],
          (member([A,B], Curr_red), neighbour_position(A, B, [MA, MB]),
           \+member([MA, MB], Curr_red), \+member([MA, MB], Curr_blue)),
          Poss_moves).

next_move_cranked(b, Move, [Curr_blue, Curr_red], Cranked_board) :-
  alter_board(Move, Curr_blue, New_blue),
  next_generation([New_blue, Curr_red], Cranked_board).
next_move_cranked(r, Move, [Curr_blue, Curr_red], Cranked_board) :-
  alter_board(Move, Curr_red, New_red),
  next_generation([Curr_blue, New_red], Cranked_board).

new_board(b, Move, [Curr_blue, Curr_red], [New_blue, New_red]) :-
  alter_board(Move, Curr_blue, New_blue),
  New_red = Curr_red.
new_board(r, Move, [Curr_blue, Curr_red], [New_blue, New_red]) :-
  New_blue = Curr_blue,
  alter_board(Move, Curr_red, New_red).

% BLOODLUST
%% base case
best_move(bloodlust, _, _, [], Best_move, Best_move, _).
best_move(bloodlust, _, _, _, Best_move, Best_move, 0).
%% recursion
best_move(bloodlust, Player, Curr_state, [Head|Rest_moves], Move, Best_move,
          Curr_smallest) :-
  next_move_cranked(Player, Head, Curr_state, [Cranked_blue, Cranked_red]),
  (Player = b
    -> length(Cranked_red, Num_opponent) ; length(Cranked_blue, Num_opponent)),
  (Curr_smallest > Num_opponent
   -> New_smallest is Num_opponent, New_best_move = Head
   ;  New_smallest is Curr_smallest, New_best_move = Move),
  best_move(bloodlust, Player, Curr_state, Rest_moves, New_best_move, Best_move,
            New_smallest).
% SELF PRESERVATION
%% base case
best_move(self_preservation, _, _, [], Best_move, Best_move, _).
best_move(self_preservation, _, _, _, Best_move, Best_move, 64).
%% recursion
best_move(self_preservation, Player, Curr_state, [Head|Rest_moves], Move,
          Best_move, Curr_biggest) :-
  next_move_cranked(Player, Head, Curr_state,[Cranked_blue, Cranked_red]),
  (Player = b
    -> length(Cranked_blue, Num_player) ; length(Cranked_red, Num_player)),
  (Num_player > Curr_biggest
    -> New_biggest is Num_player, New_best_move = Head
    ;  New_biggest is Curr_biggest, New_best_move = Move),
  best_move(self_preservation, Player, Curr_state, Rest_moves, New_best_move,
            Best_move, New_biggest).
% LAND GRAB
%% base case
best_move(land_grab, _, _, [], Best_move, Best_move, _).
%% recursion
best_move(land_grab, Player, Curr_state, [Head|Rest_moves], Move, Best_move,
          Biggest_diff) :-
  next_move_cranked(Player, Head, Curr_state, [Cranked_blue, Cranked_red]),
  length(Cranked_blue, Num_blue),
  length(Cranked_red, Num_red),
  (Player = b -> Diff is Num_blue - Num_red ; Diff is Num_red - Num_blue),
  (Diff > Biggest_diff
    -> New_biggest_diff is Diff, New_best_move = Head
    ;  New_biggest_diff is Biggest_diff, New_best_move = Move),
  best_move(land_grab, Player, Curr_state, Rest_moves, New_best_move, Best_move,
            New_biggest_diff).
