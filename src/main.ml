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

let previous_Step = move.init_PrevStep()

(*Make them forfeit before they can quit*)

(* 1P vs AI is true and 2P is false
Red is true and black is false*)
type game_state = {mutable game_mode: bool; mutable color: bool; mutable started: bool}
let curr_GameState () = {game_mode = true; color = true; started = false}

let curr_Board  = Board.init()
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
  (get_boardArray curr_board)

let run_board = print_board (init_board)

let comma = Str.regexp ","

let input_Parse (input) =
  let input = Str.bounded_split comma input 2 in
    if List.length input > 1
      then [List.hd input] @ List.tl input
    else input

let rec position_Convert (input: bytes list): int * int =
  if (List.length input = 2) then
    (int_of_string (List.hd input), int_of_string (List.hd (List.rev input)))
  else
    failwith "Incorrect coordinate size"

(*Gets the piece at the given parsed location*)
let check_ValidCoor (input) (board) =
  Board.check_position (board) (position_Convert(input))

let rec repl (board: board): bool =
let input = read_line() in
(*if (Bytes.lowercase input) <> "quit" then*)
  try
  let parsed_Input = input_Parse input in
  let check_Converted_Position = check_ValidCoor parsed_Input board in
    match check_Converted_Position with
    | None -> print_endline "Nothing at this position";
              failwith "Invalid first coordinate"
    | Some x ->  (Printf.printf "%s\n" x.name); true
  with
    _ -> print_endline "Please input a valid starting coordinate."; repl (board)

let rec repl2 (board:board) =
  let input2 = read_line() in
  (* let second_coordinate =
  ( *)try
  let sndparsed_Input = input_Parse input2 in
  let check_Converted_Position = check_ValidCoor sndparsed_Input board in
    match check_Converted_Position with
    | None -> print_endline "Nothing at this position"
    | Some x -> (Printf.printf "%s\n" x.name); true
  with
    _ -> print_endline "Please input a valid starting coordinate."; repl2 (board)

let start_test = repl init_board in
 Printf.printf "%B" start_test
let start_test2 = repl2(init_board)

let position_Convert (input: bytes list) =
  (int_of_string (List.hd input), int_of_string (List.hd (List.rev input)))

let assign_Inputs (input: bytes list) =
  | [] ->           Exception
  | "ai" ->         AI
  | "undo" ->       Undo
  | "twoplayer" ->  TwoPlayer
  | "help" ->       Help
  | "forfeit" ->    Forfeit
  | "restart" ->    Restart
  | h :: t ->       Other_input

let input_convert (board: board) (user_input: bytes) (input: inputs)=
  match input with
  | Other_input ->
  | Undo        ->
  | AI          -> if curr_GameState.started then
                      board
                    else
                      curr_GameState.started <- true;  game_mode <- true;
  | TwoPlayer   -> if curr_GameState.started then
                      board
                    else
                      board; curr_GameState.started <- true;  game_mode <- false;
  | Help        ->
  | Forfeit     ->
  | Restart     ->
  | Exception   -> board
  | StartCo     ->
  | EndCo       ->

let input_response (inputs: inputs)(thisBoard: board)
                    (newBoard: board) =
  match inputs with
  | Other_input ->  if thisBoard <> newBoard then
                      print_board(newBoard)
                    else (print_endline "Invalid input. Please try again.")
  | Undo        ->  if thisBoard <> newBoard then
                      print_board(newBoard)
                    else (print_endline "You cannot undo!")
  | AI          -> if curr_GameState.started then
                      print_endline "You have already started the game!"
                    else
                      print_endline "You are now playing against the AI!"
  | TwoPlayer   -> if curr_GameState.started then
                      print_endline "You have already started the game!"
                    else
                      print_endline "You are now playing against the other player!"
  | Help        ->
  | Forfeit     ->
  | Restart     ->
  | Exception   ->
  | StartCo     ->
  | EndCo       ->
  | Exception   -> (print_endline "Please input a valid input.")


let  _  =
print_endline ("Please choose the game mode that you would like to play.
If you would like to play against an AI, type '1P'.
If you would like to play two player, type '2P'. ");
init_state (List.nth (Array.to_list Sys.argv) 1) ;
print_endline ("Please select the color of the team you would like to play as: red goes first
black goes second. Please type 'red' if you would like to play as red or 'black'
if you would like to play as black.")
choose_team ( List.nth (Array.to_list Sys.argv) 1 ) ;
print_endline "ASK FOR 2 COORDINATES";
repl_move ()

"Please input the starting coordinates of the piece you would like to move
 in the following format: \"x, y \" where x is the x coordinate and y is the
 y coordinate"

 "Please input the end coordinate of the selected piece in the
 same format  \"x, y \". If you would like to select a different piece to move,
 type 'back'"


"%s general has been killed! %s wins!"


  if List.nth (Array.to_list Sys.argv) 1 = "1p"  then

  else if List.nth (Array.to_list Sys.argv) 1 = "2p" then

  else


let start_game input =

(*Pull the history, to be used for undo. For help, give them options based off of
*)

(*Pull the intialized board from board.ml and new mutated board from update
in move.ml*)

(*
let assign_PieceChar piece =
  match piece with
  | General   ->
  | Advisor   ->
  | Elephant  ->
  | Horse     ->
  | Rook      ->
  | Cannon    ->
  | Soldier   ->


let piece_convertPrint =
  None
  rookR1
  horseR1
  elepR1;
  advisorR1
  generalR
  advisorR2
  elepR2
  horseR2
  rookR2
  canR1
  canR2
  soldR1
  soldR2
  soldR3
  soldR4
  soldR5
  soldB1
  soldB2
  soldB3
  soldB4
  soldB5
  canB1
  canB2
  rookB1
  horseB1
  elepB1;
  advisorB1
  generalB
  advisorB2
  elepB2;
  horseB2
  rookB2 in
*)
