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
open Bytes

(*types for commands*)
type inputs = Undo| AI | TwoPlayer| Help | Forfeit | Restart| Exception| StartCo |
            EndCo | Other_input

let previous_Step = init_PrevStep()

(*Make them forfeit before they can quit*)

(* 1P vs AI is true and 2P is false
Red is true and black is falseteble *)
let curr_Board  = Board.init()
let init_Step = Move.init_step ()
type game_state =
{
(* AI  or 2 person  true=AI*)
game_mode: bool;
(* now who's playing *)
color: bool;
 (* start the new game *)
started: bool;
(* board carrying *)
board: board;
prev_step : prev_step;
curr_step : step;
undon: bool}

(* if red goes first *)
let init_GameState  =
  {game_mode = true; color = true; started = false; board = curr_Board;
  prev_step = init_PrevStep() ;
  curr_step = init_step () ;
  undon = false
  }

(* following funtion all have readline! *)
let input_Parse (input : bytes) : bytes list =
  let comma = Str.regexp "," in
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

let valid_first_coor (gs  : game_state) (ps : position) : bool =

    match check_position gs.board ps with
    | None -> let () = print_endline "Nothing at this position" in false
    | Some x -> if (gs.color = x.team) then
                  let () = Printf.printf "%s\n" x.name in  true
                else
                 let () = print_endline "This piece is not of the current player's color!" in  false

    (* | _ -> let () = print_endline "Please input a valid starting coordinate." in false *)


let valid_snd_coor (gs  : game_state) (ps : position) : bool =

    match check_position gs.board ps  with
    | None -> true
    | Some x -> let () = (Printf.printf "%s\n" x.name) in true

    (* | _ -> let () = print_endline "Please input a valid starting coordinate." in false *)

let rec init_game () : game_state =
  init_GameState |> choose_mode

and choose_mode (gs: game_state) : game_state =
  let () = print_endline "type 'AI' to play with AI, type 2p to play with another" in

  let input = read_line () in
  match  lowercase (input) with
  | "ai" -> choose_color {gs with game_mode = true}
  | "2p" -> choose_color {gs with game_mode = false}
  | _ -> choose_mode gs

and choose_color (gs:game_state) : game_state =
  let () = print_endline "Type 'Red' to play first, type 'green' to play later." in
  let input = read_line () in
  match lowercase (input) with
  | "red" -> first_coor {gs with color = true}
  | "green" -> first_coor {gs with color = false}
  | _ -> print_endline "please type either 'red' or 'green'"; choose_color gs

and run_round (gs:game_state) : game_state =
  if gs.game_mode then run_ai gs else run_human gs

and  first_coor (gs:game_state) : game_state =
   let () = print_endline "type the first piece you want to move, in the form
   'x, y' " in
   let input = read_line () in
   let pos = input |> input_Parse |> position_Convert  in

   if ( pos |> (valid_first_coor gs )) then
   let pc = check_position gs.board pos in
   let () =
   print_endline ("You are moving the piece "^(string_of_piece pc)) in
   let st = {gs.curr_step with start = pos} in
   second_coor {gs with curr_step = st} else first_coor gs

and  second_coor (gs: game_state) : game_state =
   let () = print_endline "type the destination you want to go to, in the form
   'x, y' " in
   let input = read_line () in
   let pos = input |> input_Parse |> position_Convert  in
   begin
   match pos |> valid_snd_coor gs with
   | true ->
      let pc = check_position gs.board pos in
      print_endline ("You are moving to "^(string_of_position pos));
      let st = {gs.curr_step with destination = pos;
    piece_captured = pc} in
    if check_valid gs.board gs.prev_step st then
      run_step {gs with curr_step = st }
   | false -> print_endline "invalid coordinate"; second_coor gs

and run_step (gs : game_state ) : game_state =

    match check_win gs.board gs.prev_step gs.curr_step with
      | true -> let () = print_endline "You win! " in init_game ()
      | false -> let () =  print_endline "continue---"  in
    let () = update_board gs.board st in
      run_round {gs with prev_step = gs.prev_step |> update_prev st ; color= not (gs.color)}


and run_human (gs: game_state ) : game_state =
  gs |> first_coor |>second_coor

and run_ai (gs: game_state) : game_state =
  let bst_step = hard_A1 gs.board gs.prev_step in
  let up_gs = {gs with curr_step = bst_step} in
  run_step up_gs











