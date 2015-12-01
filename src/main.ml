(* The main file for REPL
 * including printing the chess board
 * printing the information needed for player
 * starting some dialogue with player
 * starting the game, ending the game, etc*)
open Board
open Move
open Piece
open Score
open AI

(*types for commands*)
type cmd=undo|ai|twoplayer|quit|help|coordinate

let init_board  = Board.init()
let line_counter = ref 0

let print_piece (p : piece option) : unit =
  match  p with
  | None -> Printf.printf "%s" "  "
  (* red // green *)
  | Some pc -> let clr = if pc.team then "\027[31m" else "\027[32m" in
    Printf.printf "%s %s" clr pc.print_name


let print_board (b: board) : unit =
  Printf.printf "%s%s" "\027[37m" " 1  2  3  4  5  6  7  8  9\n";
  Array.iter (fun inner_arr ->
    incr line_counter; Printf.printf "%s   %d" "\027[37m" (!line_counter) ;
    Array.iter print_piece inner_arr; Printf.printf "%s\n" "\027[37m") (get_boardArray init_board)

let run_board = print_board (init_board)

(*parse*)
val parse: string->cmd
