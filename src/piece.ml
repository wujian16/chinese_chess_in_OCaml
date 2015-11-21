type piece_type = General|Advisor|Elephant|Horse|Rook|Cannon|Soldier
type positon = int*int

type piece={
name:string;
print_name : string;
print_color: (*color*);
type_of: piece_type;
team: bool;
init_place : position;
place: position
 }

(*general*)
let generalR= {name= "GR"; print_name="G";print_color= (*TODO*) ;
type_of=General;team = true; init_place= (1,5)
place= init_place }
let generalB= {name= "GB"; print_name="G";print_color= (*TODO*) ;
 type_of=General;team = false ; init_place=(10,5)
place= init_place }

let advisorR1= {name = "A1R"; print_name= "A"; print_color= (*TODO; 2*2*);
type_of = Advisor; team = true; init_place = (1,4); init_place = (*TODO*);
place = "TODO" }

let advisorR2= {name = "A2R"; print_name= "A"; print_color= (*TODO; 2*2*);
type_of = Advisor; team = true; init_place = (1,8); init_place = (*TODO*);
place = "TODO" }

let advisorB1= {name = "A1B"; print_name= "A"; print_color= (*TODO; 2*2*);
type_of = Advisor; team =false; init_place = (10, 4); init_place = (*TODO*);
place = "TODO" }

let advisorB2= {name = "A2B"; print_name= "A"; print_color= (*TODO; 2*2*);
type_of = Advisor; team =false; init_place = (10,8); init_place = (*TODO*);
place = "TODO" }

let ElepR1= {name = "ER1"; print_name = "E"; print_color = (*TODO*); type_of=
Elephant; team = true ;init_place=
(1,3); place=init_place  }
let ElepR1= {name = "ER2"; print_name = "E"; print_color = (*TODO*); type_of=
Elephant; team = true; init_place=
(1,7); place=init_place  }
let ElepB1= {name = "EB1"; print_name = "E"; print_color = (*TODO*); type_of=
Elephant; team =false; init_place=
(10,3); place=init_place  }
let ElepB1= {name = "EB2"; print_name = "E"; print_color = (*TODO*); type_of=
Elephant; team =false; init_place=
(10,7); place=init_place }

let horseR1 = {name= "HR1"; print_name = "H"; print_color= (*TODO*); type_of =
  Horse ; team = true; init_place = (1,2); place = (*TODO*)}
let horseR2 = {name= "HR2"; print_name = "H"; print_color= (*TODO*); type_of =
  Horse ; team = true; init_place = (1,8); place = (*TODO*)}
let horseB1 = {name= "HB1"; print_name = "H"; print_color= (*TODO*); type_of =
  Horse ; team = false; init_place = (10,2); place = (*TODO*)}
let horseB2 = {name= "HB2"; print_name = "H"; print_color= (*TODO*); type_of =
  Horse ; team = false; init_place = (10,8); place = (*TODO*)}


let rookR1 = {name = "RR1"; print_name= "TODO R"; print_color = (*Todo*) ;
type_of = Rook; team =true); init_place = (1,1) ; place = init_place}
let rookR2 = {name = "RR2"; print_name= "TODO R"; print_color = (*Todo*) ;
type_of = Rook; team = true; init_place = (1,9) ; place = init_place}
let rookB1 = {name = "RB1"; print_name= "TODO R"; print_color = (*Todo*) ;
type_of = Rook; team = false; init_place = (10,1) ; place = init_place}
let rookB2 = {name = "RB2"; print_name= "TODO R"; print_color = (*Todo*) ;
type_of = Rook; team = false; init_place = (10,9) ; place = init_place}

let canR1 = {name= "CR1"; print_name = "C"; print_color = (*TODO*);
type_of = Rook; team = true; init_place = (3, 2); place= (*TODO*)}
let canR2 = {name= "CR2"; print_name = "C"; print_color = (*TODO*);
type_of = Rook; team = true; init_place = (3,8); place= (*TODO*)}
let canB1 = {name= "CB1"; print_name = "C"; print_color = (*TODO*);
type_of = Rook; team = false; init_place = (8, 2); place= (*TODO*)}
let canB2 = {name= "CB2"; print_name = "C"; print_color = (*TODO*);
type_of = Rook; team = false; init_place = (8, 8); place= (*TODO*)}

let soldR1 = {name = "SR1"; print_name = "S"; print_color = (*TODO*);
type_of = Soldier; team = true; init_place = (4,1); place= init_place}
let soldR2 = {name = "SR2"; print_name = "S"; print_color = (*TODO*);
type_of = Soldier; team = true; init_place = (4,3); place= init_place}
let soldR3 = {name = "SR3"; print_name = "S"; print_color = (*TODO*);
type_of = Soldier; team = true; init_place = (4,5); place= init_place}
let soldR4 = {name = "SR4"; print_name = "S"; print_color = (*TODO*);
type_of = Soldier; team = true; init_place = (4,7); place= init_place}
let soldR5 = {name = "SR5"; print_name = "S"; print_color = (*TODO*);
type_of = Soldier; team = true; init_place = (4,9); place= init_place}
let soldR1 = {name = "SB1"; print_name = "S"; print_color = (*TODO*);
type_of = Soldier; team = false; init_place = (7,1); place= init_place}
let soldR2 = {name = "SB2"; print_name = "S"; print_color = (*TODO*);
type_of = Soldier; team = false; init_place = (7,3); place= init_place}
let soldR3 = {name = "SB3"; print_name = "S"; print_color = (*TODO*);
type_of = Soldier; team = false; init_place = (7,5); place= init_place}
let soldR4 = {name = "SB4"; print_name = "S"; print_color = (*TODO*);
type_of = Soldier; team = false; init_place = (7,7); place= init_place}
let soldR5 = {name = "SB5"; print_name = "S"; print_color = (*TODO*);
type_of = Soldier; team = false; init_place = (7,9); place= init_place}

let print_position ps = match  p with
  | x, y -> print_int x; print_int y
  | _ -> failwith "invalid"

let print_piece pc = print_byte (pc.name^"at "); print_position pc.position
