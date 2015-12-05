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
Red is true and black is falseteble *)
let curr_Board  = Board.init()
type game_state =
{
(* AI  or 2 person  *)
game_mode: bool;
(* now who's playing *)
color: bool;
 (* start the new game *)
started: bool;
(* board carrying *)
board: board;
prev_step : prev_Step;
curr_step : step
undon: bool}

(* if red goes first *)
let init_GameState () =
  {game_mode = true; color = true; started = false; board = curr_Board;
  prev_step = init_PrevStep() ;
  curr_step =
  }

(* following funtion all have readline! *)
let input_Parse (input : bytes) : bytes list =
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



and rec choose_mode (gs: game_state) : game_state =
  let () = print_endline "type 'AI' to play with AI, type 2p to play with another" in

  let input = read_line () in
  match  lowercase (input) with
  | "ai" -> choose_color {gs with game_mode = true}
  | "2p" -> choose_color {gs with game_mode = false}
  | _ -> choose_mode gs

and choose_color (gs:game_state) : game_state =
  let () = print_endline "Type 'Red' to play first, type 'green' to play later."
  let input = read_line () in
  match lowercase (input) with
  | "red" -> first_coor {gs with color = true}
  | "green" -> first_coor {gs with color = false}
  | _ -> print_endline "please type either 'red' or 'green'"; choose_color gs

and rec first_coor (gs:game_state) : game_state =
   let () = print_endline "type the first piece you want to move, in the form
   'x, y' " in
   let input = read_line () in
   let pos = input |> input_parse |> position_Convert  in
   if ( pos |> valid_first_coor gs) then
   print_endline "You are moving the piece "^(string_of_piece (pos));
   let st = {gs.step with start = pos} in second_coor {gs with step = st}


and rec second_coor (gs: game_state) : game_state =
   let () = print_endline "type the destination you want to go to, in the form
   'x, y' " in
   let input = read_line () in
   let pos = input |> input_parse |> position_Convert  in

   begin

   match pos |> valid_snd_coor with
   | true ->
      print_endline "You are moving to "^(string_of_position pos);
      let st = {gs.step with destination = pos;
    piece_captured = (check_position gs.board pos)} in
    begin match check_valid gs.board st with
    |true -> begin
      match check_win gs.board st with
      | true -> let () = print_endline "You win! " in init_game init_GameState
      | false -> print_endline "continue---"  in
      update_board gs.board st; first_coor {gs with prev_step = gs.prev_step |> update_prev_step }
    end
    |false -> print_endline "This somehow violated the rule"
    end
   | false -> print_endline "invalid coordinate"; second_coor gs

   end
   (*

    st |> check_valid
  in let () =
    print_endline "you captured "^(string_of_piece) in
    let () = update_board gs.board st in
    let new_prev = update_prev_step gs.prev_Step st in
   run_first {gs with color = (not gs.color); prev_step = new_prev}

 else print_endline "invalid coordinate"; second_coor gs
 *)
let check_first_coor (ps : position) (gs  : game_state): bool =

    match check_position gs.board ps with
    | None -> let () = print_endline "Nothing at this position" in false
    | Some x -> if (curr_GameState.color = x.team) then
                  let () = Printf.printf "%s\n" x.name in  true
                else
                  failwith "This piece is not of the current player's color!"
  with
    | _ -> let () = print_endline "Please input a valid starting coordinate." in false



let check_snd_coor (ps : position) (gs  : game_state): bool =

    match check_position gs.board ps  with
    | None -> true
    | Some x -> (Printf.printf "%s\n" x.name); true
  with
    _ -> print_endline "Please input a valid starting coordinate."; repl2 (board)

let start_test = repl init_board in
 Printf.printf "%B" start_test
let start_test2 = repl2(init_board)









