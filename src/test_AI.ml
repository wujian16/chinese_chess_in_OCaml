open AI
open Piece
open Board
open Move
open Score

let b0 = init () in
print_board b0;
let prev0 = init_PrevStep () in
(*
let moves = generate_piece_move b0 prev0 elepR1 in
List.iter print_step moves
*)
let (score, pred) = best_move_v0 1 b0 prev0 in
print_int score;
List.iter print_step pred
