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
open Array

(*types for commands*)
type inputs = Undo| AI | TwoPlayer| Help | Forfeit | Restart| Exception| StartCo |
            EndCo | Other_input

(*Make them forfeit before they can quit*)

(* 1P vs AI is true and 2P is false
Red is true and black is false*)
type game_state = {game_mode: bool; color: bool; started: bool}
let curr_GameState = {game_mode = true; color = true; started = false}

let init_board  = Board.init()
let line_counter = ref 0

let print_piece (p : piece option) : unit =
  match  p with
  | None -> Printf.printf "%s%s" "\027[37m" "  -"
  (* red // green *)
  | Some pc -> let clr = if pc.team then "\027[31m " else "\027[32m " in
    Printf.printf "%s %s" clr pc.print_name


let print_board (b: board) : unit =
  Printf.printf "%s%s" "\027[37m" "     1  2  3  4  5  6  7  8  9\n";
  Array.iter (fun inner_arr ->
  incr line_counter;
  if (!line_counter = 10) then Printf.printf "%s %d" "\027[37m" (!line_counter)
  else Printf.printf "%s %d" "\027[37m " (!line_counter) ;
  Array.iter print_piece inner_arr; Printf.printf "%s\n" "\027[37m")
  (get_boardArray init_board)

let run_board = print_board (init_board)

(* let comma = Str.regexp ","

let input_Parse (input) =
  let input = Str.bounded_split comma input 2 in
    if List.length input > 1
      then [List.hd input] @ List.tl input
    else input

let position_Convert (input: bytes list) =
  (int_of_string (List.hd input), int_of_string (List.hd (List.rev input)))

let check_ValidCoor (input) (board) =
  let isPiece = Board.check_position (board) (position_Convert(input)) in
  match isPiece with
  | None    ->
  | Some pc ->
 *)

(* (parse) *)
let to_tuple (x:int) (y:int) : (int*int) =
    (x,y)

(* val parse: string->cmd *)
(*in this form start_x start_y dest_x dest_y*)
  let start = to_tuple (List.nth (Array.to_list Sys.argv) 1)
    (List.nth (Array.to_list Sys.argv) 2)  in

  let dest = to_tuple (List.nth (Array.to_list Sys.argv) 3)
    (List.nth (Array.to_list Sys.argv) 4) in

let init_state (input: string) =
  if
