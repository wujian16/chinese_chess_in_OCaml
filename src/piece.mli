(* piece_info tells what is each peice*)
type piece_info = General|Advisor|Elephant|Horse|Chariot|Cannon|Soldier

(*the type tells the position of the piece on the board*)
type position = int*int

(*the follwing record type defines all the information of a piece*)
type piece={
name:string;
print_name : string;
type_of: piece_info;
team: bool
}
val genaralR: piece
val genaralB: piece

val advisor1R : piece
val advisor2R : piece
val advisorB1 : piece
val advisorB2 : piece

val elepR1 : piece
val elepR2 : piece
val elepB1 : piece
val elepB2 : piece

val horseR1 : piece
val horseR2 : piece
val horseB1 : piece
val horseB2 : piece

val rookR1 : piece
val rookR2 : piece
val rookB1 : piece
val rookB2 : piece

val canR1 : piece
val canR2 : piece
val canB1 : piece
val canB2 : piece


val soldR1 : piece
val soldR2 : piece
val soldR3 : piece
val soldR4 : piece
val soldR5 : piece

val soldB1 : piece
val soldB2 : piece
val soldB3 : piece
val soldB4 : piece
val soldB5 : piece


