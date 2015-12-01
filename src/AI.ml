open Piece
open Board
open Move
open Score
exception TODO
  
type board_hashcode = string

type search_result = {depth:int; best:step list; conclu:int}

type transposition_table = (string, search_result) Hashtbl.t

type history_table = (string, search_result) Hashtbl.t

let sort = raise TODO

let generate_all_moves (b:board) (p:prev_step) (side:round): step list= 
	let all_pieces = get_alive_side b side in
	let each_steps = List.map (fun a -> generate_piece_move b p a) all_pieces in
	List.fold_left 
		(fun l s -> match s with
		 | None -> l
		 | Some t -> l@t)
		[] each_steps


let alphaBeta (alpha:int) (beta:int) (depth_left:int) 
	(b:board) (p:prev_step) (ai_col:bool) (curr_rd:bool): (int,step list) =

	if depth_left = 0 then ((evaluate b),[])
	else
		if curr_rd = ai_col then
		
			let result = ref (int_of_float neg_infinity) in
			v = ref alpha;
			i = ref 0;
			best_steps = ref [];
			begin match generate_all_moves b p col with
			| [] -> ((-depth_left) - (int_of_float neg_infinity),[])
			| l ->
				let all_moves = Array.of_list l in

				while ((!i < (Array.length all_moves)) && (beta <> !result))
				do 
					let (updated_b,updated_prev) = update all_moves.i (b,p) in
					let (score,new_best_steps) = alphaBeta !v beta (depth_left - 1) 
								updated_b updated_prev ai_col !curr_rd in
					if (score > !v) then 
						v:= score; 
						best_steps:= all_moves.i::new_best_steps;
						i:= i+1
					else if (score > beta) then result := beta; i:= i+1
					else i:= i+1
				done;
				if !result = beta then (beta,!best_steps) else (!v,!best_steps)
			end

		else

			let result = ref (int_of_float infinity) in
			v = ref beta;
			i = ref 0;
			best_steps = ref [];
			begin match generate_all_moves b p col with
			| [] -> (depth_left - (int_of_float infinity),[])
			| l ->
				let all_moves = Array.of_list l in
				while ((!i < (Array.length all_moves)) && (alpha <> !result))
				do 
					let (updated_b,updated_prev) = update all_moves.i (b,p) in
					let (score,new_best_steps) = alphaBeta alpha !v (depth_left - 1) 
								updated_b updated_prev ai_col !curr_rd in
					if (score < !v) then 
						v:= score; 
						best_steps:= all_moves.i::new_best_steps;
						i:= i+1
					else if (score < alpha) then result := alpha; i:= i+1
					else i:= i+1
				done;
				if !result = alpha then (alpha,!best_steps) else (!v,!best_steps)
			end

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
let best_move_v0 (n:int) (b:board) (p:step list): step list = 
	(* need to modify the alpha beta to keep track of the optimum steps *)
	let (score, next_steps) = 
		alphaBeta (int_of_float neg_infinity) (int_of_float infinity) 
			n b p col round in
	next_steps

