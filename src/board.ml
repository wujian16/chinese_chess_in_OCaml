(*open Core.Std*)
open Piece

(*board={first=piece array array; second=(string,piece) Hashtbl}
 *)
type board={first:piece option array array; second:(string, piece) Hashtbl.t}

(* the variable round tells the current round
 *  * is red side's turn or blue side's turn*)
type round = bool

(*get the piece given position*)
let check_position (b:board) (p:position) =
  ((b.first).(fst p)).(snd p)

(*the following function can tell whether a piece still exists on the borad*)
let check_alive (b:board) (pie:string) = Hashtbl.mem (b.second) pie 

(*get the position given a piece*)
let get_position (b:board) (pie:string) =
  if check_alive b pie = true then
  Some (Hashtbl.find (b.second) pie).place
  else None

(*giving all the pieces which are still alive*)
let get_alive_pieces (b:board)=Hashtbl.fold (fun s p lst->p::lst) (b.second) []

(*print the ball*)
let print_board (b:board)=()

(*print the peice*)
let print_piece (pie:piece)=()
