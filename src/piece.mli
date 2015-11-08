(* piece_info tells what is each peice*)
type piece_info = General|Advisor|Elephant|Horse|Chariot|Cannon|Soldier

(*the second item of piece tells which side the piece belongs to*)
type piece = piece_info*bool

type position = int*int
