(*  The main file for REPL
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

let previous_Step = init_PrevStep()

(*Make them forfeit before they can quit*)

(* 1P vs AI is true and 2P is false
Red is true and black is false*)
let curr_Board  = Board.init()
type game_state = {mutable game_mode: bool; mutable color: bool; mutable started: bool; mutable board: board}
let curr_GameState () = {game_mode = true; color = true; started = false; board = curr_Board}


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
  (get_boardArray curr_Board)

let run_board = print_board (curr_Board)

let comma = Str.regexp ","

let input_Parse (input : bytes) =
  let input = Str.bounded_split comma input 2 in
    if List.length input > 1
      then [List.hd input] @ List.tl input
    else input
(*convert [x;y] to a position *)
let rec position_Convert (input: bytes list): int * int =
  if (List.length input = 2) then
    (int_of_string (List.hd input), int_of_string (List.hd (List.rev input)))
  else
    failwith "Incorrect coordinate size"

(*Gets the piece option at the given parsed location*)
let check_ValidCoor (p: position) (b: board) : piece option =
  check_position b p

  (* check whether there is a piece at this position *)
let is_piece (b:board ) (p:position): bool =
  let po = check_position b p in
  match po with
  | None -> false
  | Some _ -> true

(*validate first coordinate(piece want to move), includ*)
let rec check_first_coor (board: board): bool =
let input = read_line() in
(*if (Bytes.lowercase input) <> "quit" then*)
  try
  let parsed_Input = input_Parse input in
  let check_Converted_Position = check_ValidCoor parsed_Input board in
    match check_Converted_Position with
    | None -> print_endline "Nothing at this position";
              failwith "Invalid first coordinate"
    | Some x -> if (curr_GameState.color = x.team) then
                  let () = Printf.printf "%s\n" x.name in  true
                else
                  failwith "This piece is not of the current player's color!"
  with
    _ -> print_endline "Please input a valid starting coordinate."; repl (board)


(* second coordinate *)
let rec check_snd_coor (board:board) :bool=
  let input2 = read_line() in
  (* let second_coordinate =
  ( *)try
  let sndparsed_Input = input_Parse input2 in
  let check_Converted_Position = check_ValidCoor sndparsed_Input board in
    match check_Converted_Position with
    | None -> true
    | Some x -> (Printf.printf "%s\n" x.name); true
  with
    _ -> print_endline "Please input a valid starting coordinate."; repl2 (board)

let start_test = repl init_board in
 Printf.printf "%B" start_test
let start_test2 = repl2(init_board)

(* let position_Convert (input: bytes list) =
  (int_of_string (List.hd input), int_of_string (List.hd (List.rev input))) *)
(* let command_undo  =
  TODO *)



let assign_Inputs (input: bytes list) =
 function
  | [] ->           Exception
  | "ai" ->         AI
  | "undo" ->       Undo
  | "twoplayer" ->  TwoPlayer
  | "help" ->       Help
  | "forfeit" ->    Forfeit
  | "restart" ->    Restart
  | h :: t ->       Other_input


let rec init_stateGame : unit=
  (*if (Array.length Sys.argv - 1) = 1 then *)
  let input = read_line() in
  match lowercase(input) with
  | "1p" -> let () = curr_GameState.started <- true in
            let () = curr_GameState.game_mode <- true in
            (print_endline "You will now playing against the AI.")
  | "2p" -> let () = curr_GameState.started <- true in
            let () = curr_GameState.game_mode <- false in
            (print_endline "You will be playing against a second human player.")
  | _ ->    (print_endline "Please type a valid game mode."); init_stateGame()


let rec init_stateTeam () =
  let input = read_line() in
  match lowercase(input) with
  | "red"    -> let () = curr_GameState.color <- true in
                print_endline "You are now playing as Red."
  | "green"  -> let () = curr_GameState.color <- false in
                print_endline "You are now playing as Green."
  | _        -> print_endline ("Please choose a valid team color.")


  let initialize =
(print_endline ("Please choose the game mode that you would like to play.\n
If you would like to play against an AI, type '1P'.\n
If you would like to play two player, type '2P'. \n " );

let () = init_stateGame() in
print_endline ("Please select the color of the team you would like to play as:
  RED goes first GREEN goes second.\n
  Please type 'red' if you would like to play as red or 'green'
if you would like to play as green.");
init_stateTeam())

"Please input the starting coordinates of the piece you would like to move
 in the following format: \"x, y \" where x is the x coordinate and y is the
 y coordinate"

 "Please input the end coordinate of the selected piece in the
 same format  \"x, y \". If you would like to select a different piece to move,
 type 'back'"


"%s general has been killed! %s wins!"
(*

  if List.nth (Array.to_list Sys.argv) 1 = "1p"  then

  else if List.nth (Array.to_list Sys.argv) 1 = "2p" then

  else *)


(* let start_game input =


let rec run_game =
  choose_
  initialize_board;
 *)
