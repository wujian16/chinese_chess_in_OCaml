open Piece
open Board
open Move
open Score
  
(*the same type board as in Board_Info*)
type board

(* record previous AI computations in this game*)
type transposition_table

(* record history AI computations*)
type history_table

(* sort the all possible moves*)
val sort: board->step list

(*generate the best possible moves for the futural several steps
* int below is how many steps we predict for the future*)
val best_move: int->board->transposition_table->history_table->step list

(*update the historical information*)
val update: 
  step->transposition_table*history_table->transposition_table*history_table
