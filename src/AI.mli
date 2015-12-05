open Piece
open Board
open Move
open Score

(* Hashcode for a board's current state *)
type board_hashcode

(* A record for the search result of a given state *)
type search_result

(* record previous AI computations in this game*)
type transposition_table

(* record history AI computations*)
type history_table

val cnt: int ref

val quiescence: int->int->int->board->prev_step->bool->bool->int

(* generate the best move using non-iterative alpha beta w/o tables *)
val best_move_v0: int-> board-> prev_step -> int*step list

(*generate the best possible moves for the futural several steps
* int below is how many steps we predict for the future*)
val best_move: int-> board-> transposition_table-> history_table-> step list

(*update the historical information*)
val update_AI:
  board-> step-> transposition_table -> history_table-> transposition_table * history_table

val easy_AI:
  board-> prev_step -> step

val hard_AI:
  board-> prev_step -> step