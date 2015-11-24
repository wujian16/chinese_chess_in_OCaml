open Piece
open Board
open Move
open Score
  
(* record previous AI computations in this game*)
type transposition_table

(* record history AI computations*)
type history_table

(* a record type with fields new_board, new_trans, new_hist, recom_steps *)
type result_AI

(* sort the all possible moves*)
val sort: board->step list

(*generate the best possible moves for the futural several steps
* int below is how many steps we predict for the future*)
val best_move: int->board->transposition_table->history_table->step list

(*update the historical information*)
val update: 
  step->transposition_table*history_table->transposition_table*history_table

val easy_AI: 
  board->transposition_table->history_table->result_AI
  
val hard_AI:
  board->transposition_table->history_table->result_AI
