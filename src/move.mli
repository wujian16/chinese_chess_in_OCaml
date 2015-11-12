open Piece
open Board

(* the following type tells one step during one game*)
type step={start:position; destination: position; piece_captured: piece option}

(*We will implement a module with type Module_Info as a functor of
 * a module of type Board_Info*)
module type Move_Info:
sig
  (* the same as type board in Board_Info*)  
  type board

  (* record the previous step*)
  type prev_step

  (*check whether a step is valid or not*)
  val check_valid: board->prev_step->step->bool

  (* check whether game is over after this step*)
  val check_win: board->prev_step->step->bool

  (* check whether either side is checked*)
  val checked: board->prev_step->step->bool

  (* update the game information*)
  val update: (board,prev_step)->(board,prev_step) option

  (*generate all possible move steps*)
  val generate_piece_move: board->prev_step->piece->step list option

  (* print for debugging purpose*)
  val print_step: step->unit
end 
