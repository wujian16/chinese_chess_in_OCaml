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

let init ()=
  let r1=Array.of_list [Some rookR1;Some horseR1; Some elepR1;
  Some advisorR1; Some generalR1; Some advisorR2;Some elepR2;
  Some horseR2; Some rookR2] in
  let r2=Array.of_list [None;None;None;None;None;None;None;None;None] in
  let r3=Array.of_list [None; Some canR1; None; None; None; None; None; Some canR2; None]
  in
  let r4=Array.of_list [Some soldR1; None; Some soldR2; None; 
  Some soldR3; None; Some soldR4; None; 
  Some soldR5] in
  let r5=Array.of_list [None;None;None;None;None;None;None;None;None] in
  let r6=Array.of_list [None;None;None;None;None;None;None;None;None] in
  let r7=Array.of_list [Some soldB1; None; Some soldB2; None; 
  Some soldB3; None; Some soldB4; None; 
  Some soldB5] in
  let r8=Array.of_list [None; Some canB1; None; None; None; None; None; Some canB2; None]
  in
  let r9=Array.of_list [None;None;None;None;None;None;None;None;None] in
  let r10=Array.of_list [Some rookB1;Some horseB1; Some elepB1;
  Some advisorB1; Some generalB1; Some advisorB2;Some elepB2;
  Some horseB2; Some rookB2] in
  let f1= Array.of_list [r1;r2;r3;r4;r5;r6;r7;r8;r9;r10] in
  let f2= (string, piece) Hashtbl.create 32 in
  let ()=
  for i =0 to 9 do
    for j=0 to 8 do
      match f1.(i).(j) with
      |None->()
      |Some p->f2.add p.name p
    done
  done
  in
  {first=f1;second=f2}

(*print the ball*)
let print_board (b:board)=()

(*print the peice*)
let print_piece (pie:piece)=()
