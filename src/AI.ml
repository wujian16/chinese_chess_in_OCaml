open Piece
open Board
open Move
open Score
exception TODO

type board_hashcode = string

type search_result = {depth:int; best:step list; conclu:int}

type transposition_table = (string, search_result) Hashtbl.t

type history_table = (string, search_result) Hashtbl.t

let depth_limit=2

let nHistoryTable=Hashtbl.create (90*90)

let ()=
	for i = 1 to 9 do
	  for j=1 to 10 do
			for m=1 to 9 do
				for n=1 to 10 do
					Hashtbl.add nHistoryTable ((i,j),(m,n)) 0
				done
			done
		done
	done

(* The value of each piece type
*)
let mVV (p:piece) =
  match p.type_of with
  |General->5
  |Advisor->1
  |Elephant->1
  |Horse->3
  |Rook->4
  |Cannon->3
  |Soldier->2

let value_MVV (b:board) (s:step)=
  let pos=
  match s.piece_captured with
  |None->0
  |Some dst->(mVV dst)
  in
  begin
  match (check_position b s.start) with
   | None -> failwith "impossible"
   | Some src->pos-(mVV src)
  end

let compared_MVV (b:board) (s1:step) (s2:step)=
  (value_MVV b s2)-(value_MVV b s1)

(* compare the vals of two steps in history table*)
let compared_hist s1 s2=
	(Hashtbl.find nHistoryTable (s2.start, s2.destination))
	-(Hashtbl.find nHistoryTable (s1.start, s1.destination))


let check_end_game (b:board) (p:prev_step) : bool =
	(generate_all_moves b p true = []) || (generate_all_moves b p false = []) ||
	(not ((check_alive b "GR") && (check_alive b "GB")))


(* quiescence search
*)
let rec quiescence (alpha:int) (beta:int) (depth:int)
(b:board) (p:prev_step) (ai_col:bool) (curr_rd:bool):int=
  if depth=0 then (eval_board b ai_col)
  else
  (
    if ai_col=curr_rd then
    (
    let v = ref alpha in
    let result = ref min_int in
    let sort_moves =
    if checked b (init_PrevStep ()) (not curr_rd) then
      let ()=print_endline "hehehehehe" in
      let all_moves = generate_all_moves b p curr_rd in
      List.sort (fun s1 s2->compared_MVV b s1 s2) all_moves
    else
    (
      print_endline "hahahah";
      let score=eval_board b ai_col in
      (if (score > !v) then
            v:= score
       else if (score > beta) then
            (*Printf.printf "Score is greater than beta, impossible, break immediately\n";*)
            result := beta
      else ()
      );
      (
      if !result=beta then []
      else let all_moves = generate_all_moves b p curr_rd in
           let cap_moves = ref [] in
           let ()=List.iter (fun s-> begin
           match s.piece_captured with
           |None -> ()
           |Some p-> cap_moves:=(s::(!cap_moves))
           end
           ) all_moves in
           List.sort (fun s1 s2->compared_MVV b s1 s2) !cap_moves
      )
    ) in
    List.iter (fun s-> print_step s) sort_moves;
    print_endline "Not hello";
    (
    if !result=beta then !result
    else
       (
        let i=ref 0 in
        let result = ref min_int in
        let sort_moves_array=Array.of_list sort_moves in
        while ((!i < (Array.length sort_moves_array)) && (beta <> !result))
        do
          let (updated_b,updated_prev) = update_unmutable sort_moves_array.(!i) b p in
          print_int !i;
          print_string " ";
          print_int (List.length (get_alive_pieces updated_b));
          print_endline "";
          let next=(quiescence !v beta (depth-1) updated_b updated_prev
            ai_col (not curr_rd)) in
          (if (next > !v) then
            v:= next
          else if (next > beta) then
            (*Printf.printf "Score is greater than beta, impossible, break immediately\n";*)
            result := beta
          else ()
          );
        i:=!i+1;
        done;
        if !result=beta then !result
        else !v
      )
    )
  )


  else
    (
    let v = ref beta in
    let result = ref max_int in
    let sort_moves =
    if checked b (init_PrevStep ()) (not curr_rd) then
      let ()=print_endline "hehehehehe" in
      let all_moves = generate_all_moves b p curr_rd in
      List.sort (fun s1 s2->compared_MVV b s1 s2) all_moves
    else
    (
      print_endline "hahahah";
      let score=eval_board b ai_col in
      (if (score < !v) then
            v:= score
       else if (score < alpha) then
            (*Printf.printf "Score is greater than beta, impossible, break immediately\n";*)
            result := alpha
      else ()
      );
      (
      if !result=alpha then []
      else let all_moves = generate_all_moves b p curr_rd in
           let cap_moves = ref [] in
           let ()=List.iter (fun s-> begin
           match s.piece_captured with
           |None -> ()
           |Some p-> cap_moves:=(s::(!cap_moves))
           end
           ) all_moves in
           List.sort (fun s1 s2->compared_MVV b s1 s2) !cap_moves
      )
    ) in
    List.iter (fun s-> print_step s) sort_moves;
    print_endline "Not hello";
    (
    if !result=alpha then !result
    else
       (
        let i=ref 0 in
        let result = ref max_int in
        let sort_moves_array=Array.of_list sort_moves in
        while ((!i < (Array.length sort_moves_array)) && (alpha <> !result))
        do
          let (updated_b,updated_prev) = update_unmutable sort_moves_array.(!i) b p in
          print_int !i;
          print_string " ";
          print_int (List.length (get_alive_pieces updated_b));
          print_endline "";
          let next=(quiescence alpha !v (depth-1) updated_b updated_prev
            ai_col (not curr_rd)) in
          (if (next < !v) then
            v:= next
          else if (next < alpha) then
            (*Printf.printf "Score is greater than beta, impossible, break immediately\n";*)
            result := alpha
          else ()
          );
        i:=!i+1;
        done;
        if !result=alpha then !result
        else !v
      )
    )
  )
)

let cnt = ref 0

let rec alphaBeta (alpha:int) (beta:int) (depth_left:int)
	(b:board) (p:prev_step) (ai_col:bool) (curr_rd:bool): int*step list =

	if depth_left = 0 then (cnt := !cnt + 1; (*print_int !cnt; Printf.printf "\n"; *)((eval_board b ai_col),[]))
     (*((quiescence alpha beta depth_limit b p ai_col curr_rd), []))*)
	else if check_end_game b p then (cnt := !cnt + 1; (*print_int !cnt; Printf.printf "\n";*)((eval_board b ai_col),[]))
	else

		(if curr_rd = ai_col then
			(let result = ref min_int in
			let v = ref alpha in
			let i = ref 0 in
			let best_steps = ref [] in
			begin match (generate_all_moves b p ai_col) with
			(*let h_e = (generate_piece_move b p canR1) in
			begin match h_e with*)
			| [] -> (depth_left - max_int,[])
			| l ->
				List.iter (fun a -> if (in_bound a.start)&&(in_bound a.destination) then ()
					else print_step a) l;
				(*List.iter print_step l;*)
				let sorted_l = List.sort compared_hist l in
				let all_moves = Array.of_list sorted_l in

				while ((!i < (Array.length all_moves)) && (beta <> !result))
				do

					let (updated_b,updated_prev) = update_unmutable all_moves.(!i) b p in
					let (score,new_best_steps) = alphaBeta !v beta (depth_left - 1)
								updated_b updated_prev ai_col (not curr_rd) in
					(*
					Printf.printf "\nIts %d's child has score %d. Its best moves are: \n" !i score;
					List.iter print_step new_best_steps;
					Printf.printf "\nThe current alpha is %d; beta is %d\n" !v beta;*)
          			let flag=ref false in
					(if (score > !v) then
						(
						v:= score;
						(*Printf.printf "Score is greater than current alpha. Alpha is updated: %d\n" !v;*)
 						best_steps:= all_moves.(!i)::new_best_steps;
						flag:=true
						(*Printf.printf "Now the best steps are: \n";
						List.iter print_step !best_steps*)
						)
					else if (score > beta) then
						((*Printf.printf "Score is greater than beta, impossible, break immediately\n";*)
						result := beta;
					  flag:=true
					  )
					else ()(*(Printf.printf "Nothing happens\n")*)
					);

					(if (!flag=true) then
						let new_val=(Hashtbl.find nHistoryTable
						(all_moves.(!i).start,all_moves.(!i).destination))+(depth_left*depth_left)
						in
						(Hashtbl.replace nHistoryTable
						(all_moves.(!i).start,all_moves.(!i).destination)
					  new_val)
          			else ());
					cnt := !cnt + 1;
					i:= !i+1
				done;
				(*print_int !cnt;Printf.printf "\n";*)
				if !result = beta then (*(Printf.printf "Break with beta %d; Best steps are: \n" beta;
										List.iter print_step !best_steps;*) (beta,!best_steps)
				else (*(Printf.printf "Node's value is %d; Best steps are: \n" !v;
					List.iter print_step !best_steps;*) (!v,!best_steps)
			end)

		else
			(
			let result = ref max_int in
			let v = ref beta in
			let i = ref 0 in
			let best_steps = ref [] in
			begin match generate_all_moves b p (not ai_col) with
			(*begin match (generate_piece_move b p canB2) with*)
			| [] -> ((-depth_left) - min_int,[])
			| l ->
				List.iter (fun a -> if (in_bound a.start)&&(in_bound a.destination) then ()
					else print_step a) l;
				let sorted_l = List.sort compared_hist l in
				let all_moves = Array.of_list sorted_l in

				while ((!i < (Array.length all_moves)) && (alpha <> !result))
				do
					(*Printf.printf "This is move: \n";
					print_step all_moves.(!i);*)

					let (updated_b,updated_prev) = update_unmutable all_moves.(!i) b p in
					let (score,new_best_steps) = alphaBeta alpha !v (depth_left - 1)
								updated_b updated_prev ai_col (not curr_rd) in
          (*
					Printf.printf "\nIts %d's child has score %d. Its best moves are: \n" !i score;
					List.iter print_step new_best_steps;
					Printf.printf "\nThe current alpha is %d; beta is %d\n" alpha !v;*)
          			let flag=ref false in
					(
					if (score < !v) then
						(v:= score;
						(*Printf.printf "Score is smaller than current beta. Beta is updated: %d\n" !v;*)
						best_steps:= all_moves.(!i)::new_best_steps;
						flag:=true
						(*Printf.printf "Now the best steps are: \n";
						List.iter print_step !best_steps*)
						)
					else if (score < alpha) then
						(*(Printf.printf "Score is less than alpha, impossible, break immediately\n";*)
						(result := alpha;
					  flag:=true)
					else ()(*(Printf.printf "Nothing happens\n"))*));
					(if (!flag=true) then
						let new_val=(Hashtbl.find nHistoryTable
						(all_moves.(!i).start,all_moves.(!i).destination))+(depth_left*depth_left)
						in
						(Hashtbl.replace nHistoryTable
						(all_moves.(!i).start,all_moves.(!i).destination)
					  new_val)

					else ());
					cnt := !cnt + 1;
					i:= !i+1
				done;
				(*print_int !cnt;Printf.printf "\n";*)
				if !result = alpha then (*
					(Printf.printf "Break with beta %d; Best steps are: \n" alpha;
										List.iter print_step !best_steps;*) (alpha,!best_steps)
				else (*(Printf.printf "Node's value is %d; Best steps are: \n" !v;
					List.iter print_step !best_steps; *)(!v,!best_steps)
			end)
		)


(*
let alphaBetaMax (alpha:int ref) (beta:int ref) (depth_left:int)
	(b:board) (p:prev_step): int =

	let result = ref (int_of_float neg_infinity) in
	if depth_left = 0 then (evaluate b)
	else
		let all_moves = Array.of_list (generate_all_moves b p col) in
		let i = ref 0 in
		while ((!i < (Array.length all_moves)) && (!beta <> !result))
		do (* need to update board for each move...... *)
			let (updated_b,updated_prev) = update all_moves.i (b,p) in
			let score = alphaBetaMin alpha beta (depth_left - 1)
						updated_b updated_prev in
			if (score >= !beta) then result := !beta; i:=!i+1
			else if (score > !alpha) then alpha := score; i:=!i+1
			else i:=!i+1
		done;
		if !result = !beta then !beta else !alpha

and alphaBetaMin (alpha:int ref) (beta:int ref) (depth_left:int)
	(b:board) (p:prev_step): int =

	let result = ref (int_of_float infinity) in
	if depth_left = 0 then -(evaluate b)
	else
		let all_moves = Array.of_list (generate_all_moves b p !col) in
		let i = ref 0 in
		while ((!i < (Array.length all_moves)) && (!alpha <> !result))
		do
			let (updated_b,updated_prev) = update all_moves.i (b,p) in
			let score = alphaBetaMax alpha beta (depth_left - 1)
						updated_b updated_prev in
			if (score <= !alpha) then result := !alpha; i:=!i+1
			else if (score < !beta) then beta := score; i:=!i+1
			else i:=!i+1
		done;
		if !result = !alpha then !alpha else !beta
*)

let best_move_v0 (n:int) (b:board) (p:prev_step) (ai_col:bool): int*step list =
	(* need to modify the alpha beta to keep track of the optimum steps *)
	  let t=Unix.gettimeofday () in
		let result=ref (0, []) in
		let i=ref 1 in
		while ((Unix.gettimeofday () -. t)<=300.0 && !i<=n) do
			result:=(alphaBeta min_int max_int (!i) b p ai_col ai_col);
			i:=!i+1
		done;

		!result

(*generate the best possible moves for the futural several steps
* int below is how many steps we predict for the future*)
let best_move (n:int) (b:board) (tran:transposition_table) (hist:history_table) : step list =
raise TODO

(*update the historical information*)
let update_AI (b:board) (s:step) (tran:transposition_table) (hist:history_table)
				: transposition_table * history_table =
  raise TODO

let easy_AI (b:board) (p:prev_step) (ai_col:bool): step =

  let (score,pred) = best_move_v0 2 b p ai_col in
  List.hd pred

let hard_AI (b:board) (p:prev_step) (ai_col:bool): step =
let res =
  let (score,pred) = best_move_v0 2 b p ai_col in
  List.hd pred
in let () = print_step res in res