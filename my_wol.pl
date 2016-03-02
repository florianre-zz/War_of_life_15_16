%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TEST STRATEGY %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Test strategy predicate
test_strategy(N_of_games, Pl1_strat, Pl2_strat) :-
  get_strategy_data(N_of_games, 0, Pl1_strat, Pl2_strat, Blue_wins,
                    Red_wins, Longest_game, Shortest_game, Average_game_length,
                    Average_game_time),
  format('Blue wins:           ~w~n', [Blue_wins]),
  format('Red wins:            ~w~n', [Red_wins]),
  Draws is N_of_games - Blue_wins - Red_wins,
  format('Draws:               ~w~n', [Draws]),
  format('Longest game:        ~w~n', [Longest_game]),
  format('Shortest game:       ~w~n', [Shortest_game]),
  format('Average game length: ~w~n', [Average_game_length]),
  format('Average game time:   ~3ds~n', [Average_game_time]).

% Helpers

%% base case
get_strategy_data(N_of_games, N_of_games, _, _,  0, 0, 0, 250, 0, 0).
%% recursion
get_strategy_data(N_of_games, N_simulations, Pl1_strat, Pl2_strat, Blue_wins,
                  Red_wins, Longest_game, Shortest_game, Average_game_length,
                  Average_game_time) :-
  statistics(runtime, [_,_]),
  play(quiet, Pl1_strat, Pl2_strat, N_moves, Winner),
  statistics(runtime, [_,T]),
  New_N_simulations is N_simulations + 1,
  get_strategy_data(N_of_games, New_N_simulations, Pl1_strat, Pl2_strat,
                    Prev_blue_wins, Prev_red_wins, Prev_longest_game,
                    Prev_shortest_game, Prev_average_game_length,
                    Prev_average_game_time),
  update_wins(Winner, Prev_blue_wins, Blue_wins, Prev_red_wins, Red_wins),
  update_game_length_data(Winner, Prev_longest_game, Prev_shortest_game,
                          Longest_game, Shortest_game, N_moves),
  update_averages(Prev_average_game_length, N_moves, Prev_average_game_time, T,
                  Average_game_length, Average_game_time, N_of_games).

update_averages(Prev_average_game_length, N_moves, Prev_average_game_time, T,
                Average_game_length, Average_game_time, N_of_games) :-
  Average_game_length is N_moves / N_of_games + Prev_average_game_length,
  Average_game_time is T / N_of_games + Prev_average_game_time.

update_game_length_data('exhaust', Prev_longest_game, Prev_shortest_game,
                        Prev_longest_game, Prev_shortest_game, _) :- !.
update_game_length_data(_, Prev_longest_game, Prev_shortest_game,
                        Longest_game, Shortest_game, N_moves) :-
  Longest_game is max(Prev_longest_game, N_moves),
  Shortest_game is min(Prev_shortest_game, N_moves).

update_wins(b, Curr_blue_wins, New_blue_wins, Curr_red_wins, Curr_red_wins) :-
  New_blue_wins is Curr_blue_wins + 1,!.
update_wins(r, Curr_blue_wins, Curr_blue_wins, Curr_red_wins, New_red_wins) :-
  New_red_wins is Curr_red_wins + 1,!.
update_wins(_, Curr_blue_wins, Curr_blue_wins, Curr_red_wins, Curr_red_wins).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% STRATEGIES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Bloodlust

bloodlust(Pl, Curr_state, New_state, Move) :-
  find_all_poss_moves(Pl, Curr_state, Poss_moves),
  best_move(bloodlust, Pl, Curr_state, Poss_moves, _, Move, 64),
  new_board(Pl, Move, Curr_state, New_state).

% Self Preservation

self_preservation(Pl, Curr_state, New_state, Move) :-
  find_all_poss_moves(Pl, Curr_state, Poss_moves),
  best_move(self_preservation, Pl, Curr_state, Poss_moves, _, Move, -64),
  new_board(Pl, Move, Curr_state, New_state).

% Land Grab

land_grab(Pl, Curr_state, New_state, Move) :-
  find_all_poss_moves(Pl, Curr_state, Poss_moves),
  best_move(land_grab, Pl, Curr_state, Poss_moves, _, Move, -64),
  new_board(Pl, Move, Curr_state, New_state).

% Minimax

minimax(Pl, Curr_state, New_state, Move) :-
  find_all_poss_moves(Pl, Curr_state, Poss_moves),
  best_move(max, Pl, 0, 2, Curr_state, Poss_moves, _, Move, -64, _),
  new_board(Pl, Move, Curr_state, New_state).

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
  alter_board(Move, Curr_blue, New_blue), New_red = Curr_red.
new_board(r, Move, [Curr_blue, Curr_red], [New_blue, New_red]) :-
  New_blue = Curr_blue, alter_board(Move, Curr_red, New_red).

% BLOODLUST
%% base case
best_move(bloodlust, _, _, [], Best_move, Best_move, _).
best_move(bloodlust, _, _, _, Best_move, Best_move, 0).
%% recursion
best_move(bloodlust, Pl, Curr_state, [Head|Rest_moves], Move, Best_move,
          Curr_smallest) :-
  next_move_cranked(Pl, Head, Curr_state, [Cranked_blue, Cranked_red]),
  (Pl = b -> length(Cranked_red, N_Op) ; length(Cranked_blue, N_Op)),
  (Curr_smallest > N_Op
   -> New_smallest is N_Op, New_best_move = Head
   ;  New_smallest is Curr_smallest, New_best_move = Move),
  best_move(bloodlust, Pl, Curr_state, Rest_moves, New_best_move, Best_move,
            New_smallest).
% SELF PRESERVATION
%% base case
best_move(self_preservation, _, _, [], Best_move, Best_move, _).
best_move(self_preservation, _, _, _, Best_move, Best_move, 64).
%% recursion
best_move(self_preservation, Pl, Curr_state, [Head|Rest_moves], Move,
          Best_move, Curr_biggest) :-
  next_move_cranked(Pl, Head, Curr_state,[Cranked_blue, Cranked_red]),
  (Pl = b -> length(Cranked_blue, N_Pl) ; length(Cranked_red, N_Pl)),
  (N_Pl > Curr_biggest
    -> New_biggest is N_Pl, New_best_move = Head
    ;  New_biggest is Curr_biggest, New_best_move = Move),
  best_move(self_preservation, Pl, Curr_state, Rest_moves, New_best_move,
            Best_move, New_biggest).
% LAND GRAB
%% base case
best_move(land_grab, _, _, [], Best_move, Best_move, _).
%% recursion
best_move(land_grab, Pl, Curr_state, [Head|Rest_moves], Move, Best_move,
          Biggest_diff) :-
  next_move_cranked(Pl, Head, Curr_state, [Cranked_blue, Cranked_red]),
  length(Cranked_blue, N_blue), length(Cranked_red, N_red),
  (Pl = b -> Diff is N_blue - N_red ; Diff is N_red - N_blue),
  (Diff > Biggest_diff
    -> New_biggest_diff is Diff, New_best_move = Head
    ;  New_biggest_diff is Biggest_diff, New_best_move = Move),
  best_move(land_grab, Pl, Curr_state, Rest_moves, New_best_move, Best_move,
            New_biggest_diff).
% MINIMAX
%% base cases
best_move(_, _, _, _, _, [], Best_move, Best_move, Best_diff, Best_diff).
best_move(_, Pl, Max_depth, Max_depth, [Curr_blue, Curr_red], _, _, _, _,
          Best_diff) :-
  length(Curr_blue, N_blue), length(Curr_red, N_red),
  (Pl = b -> Best_diff is N_blue - N_red ; Best_diff is N_red - N_blue).
%% max
best_move(max, Pl, D, Max_depth, Curr_state, [Head|Rest], Move, Best_move,
          Curr_diff, Biggest_diff) :-
  next_move_cranked(Pl, Head, Curr_state, [Cranked_blue, Cranked_red]),
  (Pl = b -> Op = r ; Op = b),
  find_all_poss_moves(Op, [Cranked_blue, Cranked_red], Op_moves),
  (Op_moves = []
    -> length(Cranked_blue, N_blue), length(Cranked_red, N_red),
       (Op = r -> Worst_diff is N_red - N_blue ; Worst_diff is N_blue - N_red)
    ; Next_D is D + 1,
      best_move(min, Op, Next_D, Max_depth, [Cranked_blue, Cranked_red],
                Op_moves, _, _, 64, Worst_diff)),
  (Worst_diff > Curr_diff
   -> New_diff is Worst_diff, New_move = Head
   ;  New_diff is Curr_diff, New_move = Move),
  best_move(max, Pl, D, Max_depth, Curr_state, Rest, New_move, Best_move,
            New_diff, Biggest_diff).
%% min
best_move(min, Pl, D, Max_depth, Curr_state, [Head|Rest], Move, Best_move,
          Curr_diff, Smallest_diff) :-
  next_move_cranked(Pl, Head, Curr_state, [Cranked_blue, Cranked_red]),
  (Pl = b -> Op = r ; Op = b),
  find_all_poss_moves(Op, [Cranked_blue, Cranked_red], Op_moves),
  (Op_moves = []
    -> length(Cranked_blue, N_blue), length(Cranked_red, N_red),
       (Op = r -> Best_diff is N_red - N_blue ; Best_diff is N_blue - N_red)
    ; Next_D is D + 1,
      best_move(max, Op, Next_D, Max_depth, [Cranked_blue, Cranked_red],
                Op_moves, _, _, -64, Best_diff)),
  (Best_diff < Curr_diff
   -> New_diff is Best_diff, New_move = Head
   ; New_diff is Curr_diff, New_move = Move),
  best_move(min, Pl, D, Max_depth, Curr_state, Rest, New_move, Best_move,
            New_diff, Smallest_diff).
