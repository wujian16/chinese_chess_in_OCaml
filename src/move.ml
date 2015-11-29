(* x is the column, y is the row*)
open Board
open Piece
exception TODO
exception InvalidMove
(* the following type tells one step during one game*)
type step={start:position; destination: position; piece_captured: piece option}

type  previous_step = step list


let check_position_eat (b: board) (p:position) : (piece option) =
let
match   with
| patt -> expr
| _ -> expr2

let in_bound ((x,y):position) : bool=
x>=1 && x<=9 && y>=1 && y <=10

let in_square (pc:piece) ((x,y): position) : bool =
match pc.team with
  | true  -> x>=4 &&x<=6 && y>=1 && y<=3
  | false -> x>=4 && x <= 6 && y>=8 && y<=10


let self_side (pc:piece) ((x,y): position):bool=
if pc.team=true && x>=1 && x<=9 && y>=1 && y <=5 then true
else if pc.team=false && x>=1 && x<=9 && y>=6 && y <=10 then true
else false

(*generate all the steps of a Rook on board with position p *)
let move_rook (b:board) (pc:piece) ((x,y): position) :step list =

begin
  let result = ref [] in

  let rec loop_forward curr_position =
  begin
  match  (check_position b curr_position), (in_bound curr_position) with
  | None, true -> result := {start = (x,y); destination = curr_position ;
      piece_captured = None}::(!result); loop_forward (x, y+1)
  | Some sth ,true -> if sth.team <> pc.team then
      result := {start = (x,y); destination = curr_position;
      piece_captured = sth}::(!result) else ()
  | _ , _ -> ()
end
  in

  let rec loop_back curr_position =
  begin
  match (check_position b curr_position), (in_bound curr_position )with
  | None, true-> result := {start = (x, y); destination = curr_position;
    piece_captured = None}::(!result); loop_back (x, y-1)
  | Some sth ,true -> if sth.team <> pc.team then
      result := {start = (x, y); destination = curr_position;
    piece_captured = sth}::(!result)  else ()
  | _ , _ -> ()
end
  in

  let rec loop_left  curr_position =
  begin
    match (check_position b curr_position), (in_bound curr_position )with
    | None, true -> result:= {start = (x, y); destination = curr_position;
      piece_captured = None}::(!result); loop_left (x-1, y)
    | Some sth ,true -> if sth.team <> pc.team then
         result := {start = (x, y); destination = curr_position;
      piece_captured = sth}::(!result) else ()
    | _ , _ -> ()
end

    in
  let rec loop_right curr_position =
  begin
    match (check_position b curr_position), (in_bound curr_position )with
    | None, true -> result:= {start = (x, y); destination = curr_position;
      piece_captured = None}::(!result); loop_right (x+1, y)
    | Some sth ,true -> if sth.team <> pc.team then
         result := {start = (x, y); destination = curr_position;
      piece_captured = sth}::(!result) else ()
   | _ , _ -> ()
end

    in
  loop_forward (x, y+1);
  loop_back (x, y-1);
  loop_right (x+1 , y);
  loop_left (x-1, y);
  !result
end



let move_soldier (b:board) (pc:piece) ((x,y): position) : step list =
(*red piece in its own part: row 0-5*)
match pc.team, y<=5 with
 (*red piece, in river side*)
 | true,  true -> [{start= (x,y); destination = (x, y+1); piece_captured =
  (check_position b (x, y+1))}]
 (*red piece, other side of river*)
 | true, false -> let raw_pos = [(x+1, y; (x-1, y); (x, y+1) ] in
 List.flatten (List.map (fun p -> if (in_bound p) && (
    match piece_captured b p with
    | Some sth-> sth.team <> pc.team
    | None -> true
 ) then [{start= (x,y); destination = p;
    piece_captured = (check_position b p)}] else []) raw_pos )
 (*black piece, own side*)
 | false, true -> [{start= (x,y); destination = (x, y-1); piece_captured =
  (check_position b (x, y+1))}]
 (*black piece, other side*)
 | false, false-> let raw_pos = [(x+1, y); (x-1, y); (x, y-1) ] in
 List.flatten (List.map (fun p -> if (in_bound p) && (
    match piece_captured b p with
    | Some sth-> sth.team <> pc.team
    | None -> true
 ) then [{start= (x,y); destination = p;
    piece_captured = (check_position b p)}] else []) raw_pos )




let move_cannon (b:board) (pc:piece) ((x,y): position) :step list =
  begin
    let result = ref [] in
    let flag=ref false in
    let rec loop_forward curr_position=
    if !flag=false then
      match  (check_position b curr_position), (in_bound curr_position) with
       | None, true -> result := {start = (x,y); destination = curr_position ;
        piece_captured = None}::(!result); loop_forward (x, y+1)
       | sth ,true -> flag:=true; loop_forward (x, y+1)
       | _ , _ -> ()
    else
      match  (check_position b curr_position), (in_bound curr_position) with
       | Some sth ,true -> if sth.team=pc.team then ()
                           else
                  result := {start = (x,y); destination = curr_position;
                  piece_captured = Some sth}::(!result);
       | None, true -> loop_forward (x, y+1)
       | _ , _ -> ()
    in
    let rec loop_back curr_position =
    if !flag=false then
      match  (check_position b curr_position), (in_bound curr_position) with
       | None, true -> result := {start = (x,y); destination = curr_position ;
        piece_captured = None}::(!result); loop_back (x, y-1)
       | sth ,true -> flag:=true; loop_back (x, y-1)
       | _ , _ -> ()
    else
      match  (check_position b curr_position), (in_bound curr_position) with
       | Some sth ,true -> if sth.team=pc.team then ()
                           else
                  result := {start = (x,y); destination = curr_position;
                  piece_captured = Some sth}::(!result);
       | None, true -> loop_back (x, y-1)
       | _ , _ -> ()
    in
    let rec loop_left curr_position =
    if !flag=false then
      match  (check_position b curr_position), (in_bound curr_position) with
       | None, true -> result := {start = (x,y); destination = curr_position ;
        piece_captured = None}::(!result); loop_left (x-1, y)
       | sth ,true -> flag:=true; loop_left (x-1, y)
       | _ , _ -> ()
    else
      match  (check_position b curr_position), (in_bound curr_position) with
       | Some sth ,true -> if sth.team=pc.team then ()
                           else
                  result := {start = (x,y); destination = curr_position;
                  piece_captured = Some sth}::(!result);
       | None, true -> loop_left (x-1, y)
       | _ , _ -> ()
    in
    let rec loop_right curr_position =
    if !flag=false then
      match  (check_position b curr_position), (in_bound curr_position) with
       | None, true -> result := {start = (x,y); destination = curr_position ;
        piece_captured = None}::(!result); loop_right (x+1, y)
       | sth ,true -> flag:=true; loop_right (x+1, y)
       | _ , _ -> ()
    else
      match  (check_position b curr_position), (in_bound curr_position) with
       | Some sth ,true -> if sth.team=pc.team then ()
                           else
                  result := {start = (x,y); destination = curr_position;
                  piece_captured = Some sth}::(!result);
       | None, true -> loop_right (x+1, y)
       | _ , _ -> ()
      in
    let ()=flag:=false in
    let ()=loop_forward (x, y+1) in
    let ()=flag:=false in
    let ()=loop_back (x, y-1) in
    let ()=flag:=false in
    let ()=loop_right (x+1 , y) in
    let ()=flag:=false in
    let ()=loop_left (x-1, y) in
    !result
  end

let move_horse (b:board) (pc:piece) ((x,y): position) :step list =
(*red piece in its own part: row 0-5*)
 let raw_hori_pos = [(x+2, y+1); (x+2, y-1); (x-2,y+1); (x-2,y-1)] in
 let hori_pos = List.flatten (List.map (fun p -> if (in_bound p) && (
    match piece_captured b p with
    | Some sth-> sth.team <> pc.team
    | None -> true
 ) && ( check_position ((p.x + x)/2 , y) = None) then [{start= (x,y); destination = p;
    piece_captured = (check_position b p)}] else []) raw_hori pos
in let vert_raw_pos = [(x+1, y+2) ; (x+1,y-2); (x-1, y+2); (x-1, y-2)] in
let vert_pos = List.flatten (List.map (fun p -> if (in_bound p) && (
    match piece_captured b p with
    | Some sth-> sth.team <> pc.team
    | None -> true
 ) && ( check_position ((p.y+ y)/2 , x) = None) then [{start= (x,y); destination = p;
    piece_captured = (check_position b p)}] else []) raw_hori pos

let move_elephant (b:board) (pc:piece) ((x,y): position) :step list =
  let raw_pos=[(2,2); (-2,-2); (2,-2); (-2, 2)] in
  match (self_side pc.team pc.place) with
 (*self side*)
 | true -> List.flatten (List.map (fun p ->
     if self_side pc.team (x+(fst p), y+(snd p)) &&
        check_position b (x+(fst p)/2, y+(snd p)/2)=None
     then [{start= (x,y); destination = p;
     piece_captured = (check_position b p)}] else []) raw_pos)
 (*other side of river*)
 | false -> []


let move_advisor (b:board) (pc:piece) ((x,y): position) : step list =
let raw_pos = [(x+1, y+1); (x-1, y-1); (x-1, y+1); (x+1, y-1) ] in
 List.flatten (List.map (fun p -> if (in_bound p) && (
    match piece_captured b p with
    | Some sth-> sth.team <> pc.team
    | None -> true
 ) && (in_square pc p)then [{start= (x,y); destination = p;
    piece_captured = (check_position b p)}] else []) raw_pos )


let move_general (b:board) (pc:piece) ((x,y): position) : step list =
let raw_pos = [(x+1, y); (x-1, y); (x, y+1); (x, y-1) ] in
 List.flatten (List.map (fun p -> if (in_bound p) && (
    match piece_captured b p with
    | Some sth-> sth.team <> pc.team
    | None -> true
 ) && (in_square pc p)then [{start= (x,y); destination = p;
    piece_captured = (check_position b p)}] else []) raw_pos )

(* check whether the step satisfies the additional rules*)
let additional_rules_1 (b:board) (pv:prev_step) (s:step)=
  match check_position b s.start with
  | None->false
  | Some p-> match p.type_of with
             | General-> if p.name="GB" then 
                  match (get_position b "GR") with 
                  |None-> false
                  |Some opp_pos->if s.destination.x=opp_pos.x then false
                                 else true
                  else match (get_position b "GB") with 
                  |None-> false
                  |Some opp_pos->if s.destination.x=opp_pos.x then false
                                 else true
             | _ -> true

let additional_rules_2 (b:board) (pv:prev_step) (s:step)=
  if List.length pc >=2 then
    let h1=List.nth pv 0 in
    let h2=List.nth pv 1 in
    if (fst h1.start)=(fst s.destination) && (snd h1.start)=(snd s.destination)
     && (fst h1.destination)=(fst s.start) && (snd h1.destination)=(snd s.start)
     (fst h2.start)=(fst s.start) && (snd h2.destination)=(snd s.destination)
     && (fst h2.start)=(fst s.start) && (snd h2.destination)=(snd s.destination)
    then false
    else true
  else true

let additional_rules (b:board) (pv:prev_step) (s:step)=
  if additional_rules_1 b pv s=true && additional_rules_2 b pv s=true
  then true
  else false


                                          
let generate_piece_move (b:board) (pv:prev_step) (p:piece)= 
  match p.type_of with
  let pos_option=get_position b p.name in
  match pos_option with
  |None->None
  |Some pos->let mvs=
     | Genaral -> move_general b pv pos
     | Advisor -> move_advisor b pv pos
     | Elephant -> move_elephant b pv pos
     | Horse    -> move_horse b pv pos
     | Rook     -> move_rook b pv pos
     | Cannon   -> move_cannon b pv pos
     | Soldier  -> move_soldier b pv pos
  in let candidate=List.filter (fun m->additional_rules b pv m) mvs in
  if List.length candidate>0 then Some candidate
  else None


  (*check*)


let check_valid (b: board) (pv:prev_step) (st:step) :bool =
  match st.start with
  | patt -> expr
  | _ -> expr2


let checked =  TODO (*may not implement*)

let update = TODO(*What the heck it it?*)



let string_of_step stp = begin match stp with
  | {start = st; destination = ds; piece_captured=pcap} ->
"start: "^(string_of_position st)^"end: "^(string_of_position ds)^
(begin match pcap with
  | None -> "Captured nothing"
  | Some pcs -> "Captured "^(string_of_piece_with_position pcs)
  | _ -> failwith "not valid"  end)
end

let print_step stp = print_endline (string_of_step stp)



(*
1.generals cannot face each other
2. interactions
*)
