open Core.Std

(* the variable round tells the current round
 * is red side's turn or blue side's turn*)
val round: bool

(* piece_info tells what is each peice*)
type piece_info = General|Advisor|Elephant|Horse|Chariot|Cannon|Soldier

(*the second item of piece tells which side the piece belongs to*)
type piece = piece_info*bool

type position = int*int

(*The representation can get the piece given position*)
val board_rep: piece array array

(*another representation can get the position given a piece*)
val board_piece: (piece, position) Hashtbl.t = <abstr>

(*get the piece given position*)
val check_position: position->piece option

(*get the position given a piece*)
val get_position: piece->position option

val check_alive: piece->bool

val get_alive_pieces: unit->piece list
