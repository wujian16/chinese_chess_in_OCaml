open Core.Std
open Piece

module type Board_Info:
sig
  type board

  (* the variable round tells the current round
   * is red side's turn or blue side's turn*)
  val round: bool

  (*get the piece given position*)
  val check_position: board->position->piece option

  (*get the position given a piece*)
  val get_position: board->piece->position option

  (*the following function can tell whether a piece still exists on the borad*)
  val check_alive: board->piece->bool

  (*giving all the pieces which are still alive*)
  val get_alive_pieces: board->piece list
end
