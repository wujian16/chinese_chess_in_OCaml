(* piece_info tells what is each peice*)
type piece_info = General|Advisor|Elephant|Horse|Chariot|Cannon|Soldier

type position = int*int

type piece={name:string; type_of: piece_info; team: bool; place: position}

