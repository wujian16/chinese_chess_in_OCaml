(* piece_info tells what is each peice*)
type piece_info = General|Advisor|Elephant|Horse|Chariot|Cannon|Soldier

(*the type tells the position of the piece on the board*)
type position = int*int

(*the follwing record type defines all the information of a piece*)
type piece={
name:string;
print_name : string;
type_of: piece_info;
team: bool;
init_place : position;
place: position
}
