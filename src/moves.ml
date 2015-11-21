open Board
open Piece
exception TODO
exception InvalidMove
(* the following type tells one step during one game*)
type step={start:position; destination: position; mutable piece_captured: piece option}

module  Move_Info = struct
  type  board = board
  type  previous_step = step


  (*generate all the steps of a Rook on board with position p *)
  let move_rook (b:board) (pc:piece) (p: position) :step list =
    let result = [] in
    begin
    match  p.place, pc.team  with
    | x , y ,b ->
      (*forward*)
      for y'=y+1 to 10 do
        begin
          match check_position b (x, y') with
          | None -> {start= p; end = (x, y'); piece_captured = None}::result
          | Some oth -> if oth.team = b then result;raise Exit
                        else {start= p; end = (x, y'); piece_captured = oth}::result
        end
        (*TODOhow to link them!?!!!!!!!!!!!!!!*)
        for y' = y-1 downto 1 do
        begin
          match check_position b (x, y') with
          | None -> {start= p; end = (x, y'); piece_captured = None}::result
          | Some oth -> if oth.team =b then result;raise Exit
                        else {start= p; end = (x, y'); piece_captured = oth}::result
        end
 (*dfajlsj*)
  in  result
  end
(*move *)
  let move_sodier (b:board) (pc:piece) (p: position) :step list =
    let result = [] begin
    (*two cases row+1 or col+1*)
      match pc.place, pc.team with
        | x, y, true  -> begin
  (*how to let the computer know i'M doing position *)
        let psb: (position list)  = [(x+1, y); (x-1,y), (x, y+1)]
        let tem_rslt = List.map (fun dest -> match dest with
        | x, y -> if (x>=1 && x <= 10) && (y>=1 and y<=9) then begin
          match check_position  b dest with
          | None -> [{start= p; destination=dest; piece_captured = None }]
          | Some oth -> if oth.team = pc.team then [] else
            [{start=p ; destination = dest; piece_captured = oth}]
        end
        | _ -> failwith"not possible"
      in List.flatten tem_rslt end

      | x, y ,false -> begin
        let psb : (position list ) = [(x+1, y); (x-1,y); (x, y-1)]
        let tem_rslt = List.map (fun dest -> match dest with
        | x, y -> if (x>=1 && x <= 10) && (y>=1 and y<=9) then begin
          match check_position  b dest with
          | None -> [{start= p; destination=dest; piece_captured = None }]
          | Some oth -> if oth.team = pc.team then [] else
            [{start=p ; destination = dest; piece_captured = oth}]
        end
        | _ ->failwith"not possible"
      in List.flatten tem_rslt end

  end
      end


  let move_horse =


  let move_cannon = TODOHARD

  let move_elephant = TODO
  let move_advisor = TODO
  let move_general = TODO


  let generate_piece_move b pv p =
    let result_list = []
    match p.type_of with
    |Rook -> begin
     match p.team, p.place with
      | true, x, y  -> for i=x to 9
      (*increment 1, check if available (empty or eat enemy: capture= add));
    then do the same thing with row; when encouter a piece, just break: raise Exit *)
      | _ -> expr2
      (*reverse*)
    end
  end


    | _ -> expr2



  let check_valid (b: board) (pv:prev_step) (st:step) :bool = TODO

  let check_win = match pie with
  | patt -> expr
  | _ -> expr2

  let checkek =  TODO (*may not implement*)

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


end


(*
1.generals cannot face each other
2. interactions
*)