open Piece
open Board
open Move
open Score
exception TODO

type board_hashcode = string

type search_result = {depth:int; best:step list; conclu:int}

type transposition_table = (string, search_result) Hashtbl.t

type history_table = (string, search_result) Hashtbl.t

let sort b = raise TODO

<<<<<<< HEAD
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
=======
let generate_all_moves (b:board) (p:prev_step) (side:bool): step list=
	let all_pieces = get_alive_side b side in
	let each_steps = List.map 
		(fun a -> let l = generate_piece_move b p a in List.iter print_step l; l) all_pieces in
	List.flatten each_steps


let rec alphaBeta (alpha:int) (beta:int) (depth_left:int)
	(b:board) (p:prev_step) (ai_col:bool) (curr_rd:bool): int*step list =
>>>>>>> 80ad638a4581ad392a89d511814cd7dcec6676c8

	if depth_left = 0 then ((eval_board b),[])
	else
<<<<<<< HEAD
		if curr_rd = ai_col then

			let result = ref (int_of_float neg_infinity) in
			v = ref alpha;
			i = ref 0;
			best_steps = ref [];
			begin match generate_all_moves b p col with
			| [] -> ((-depth_left) - (int_of_float neg_infinity),[])
			| l ->
=======
		(if curr_rd = ai_col then 
			(let result = ref min_int in
			let v = ref alpha in
			let i = ref 0 in
			let best_steps = ref [] in
			(*begin match (generate_all_moves b p ai_col) with*)
			let h_e = (generate_piece_move b p canR1) in
			begin match h_e with
			| [] -> (depth_left - max_int,[])
			| l -> 
				List.iter print_step l;
>>>>>>> 80ad638a4581ad392a89d511814cd7dcec6676c8
				let all_moves = Array.of_list l in

				while ((!i < (Array.length all_moves)) && (beta <> !result))
				do
<<<<<<< HEAD
					let (updated_b,updated_prev) = update all_moves.i (b,p) in
					let (score,new_best_steps) = alphaBeta !v beta (depth_left - 1)
								updated_b updated_prev ai_col !curr_rd in
					if (score > !v) then
						v:= score;
						best_steps:= all_moves.i::new_best_steps;
						i:= i+1
					else if (score > beta) then result := beta; i:= i+1
					else i:= i+1
=======
		
					let (updated_b,updated_prev) = update_unmutable all_moves.(!i) b p in
					let (score,new_best_steps) = alphaBeta !v beta (depth_left - 1)
								updated_b updated_prev ai_col (not curr_rd) in
					Printf.printf "This is move: \n";
					print_step all_moves.(!i);
					Printf.printf "\nIts %d's child has score %d. Its best moves are: \n" !i score;
					List.iter print_step new_best_steps;
					Printf.printf "\nThe current alpha is %d; beta is %d\n" !v beta;
					(if (score > !v) then
						(
						v:= score;
						Printf.printf "Score is greater than current alpha. Alpha is updated: %d\n" !v;
						best_steps:= all_moves.(!i)::new_best_steps;
						Printf.printf "Now the best steps are: \n";
						List.iter print_step !best_steps
						)
					else if (score > beta) then 
						(Printf.printf "Score is greater than beta, impossible, break immediately\n";
						result := beta)
					else (Printf.printf "Nothing happens\n")
					);
					i:= !i+1
>>>>>>> 80ad638a4581ad392a89d511814cd7dcec6676c8
				done;
				if !result = beta then (Printf.printf "Break with beta %d; Best steps are: \n" beta; 
										List.iter print_step !best_steps; (beta,!best_steps) )
				else (Printf.printf "Node's value is %d; Best steps are: \n" !v; 
					List.iter print_step !best_steps; (!v,!best_steps))
			end)

		else
<<<<<<< HEAD

			let result = ref (int_of_float infinity) in
			v = ref beta;
			i = ref 0;c
			best_steps = ref [];
			begin match generate_all_moves b p col with
			| [] -> (depth_left - (int_of_float infinity),[])
=======
			(print_endline "2";
			let result = ref max_int in
			let v = ref beta in
			let i = ref 0 in
			let best_steps = ref [] in
			(*begin match generate_all_moves b p ai_col with*)
			begin match (generate_piece_move b p rookR1) with
			| [] -> ((-depth_left) - min_int,[])
>>>>>>> 80ad638a4581ad392a89d511814cd7dcec6676c8
			| l ->
				let all_moves = Array.of_list l in
				while ((!i < (Array.length all_moves)) && (alpha <> !result))
				do
<<<<<<< HEAD
					let (updated_b,updated_prev) = update all_moves.i (b,p) in
					let (score,new_best_steps) = alphaBeta alpha !v (depth_left - 1)
								updated_b updated_prev ai_col !curr_rd in
					if (score < !v) then
						v:= score;
						best_steps:= all_moves.i::new_best_steps;
						i:= i+1
					else if (score < alpha) then result := alpha; i:= i+1
					else i:= i+1
=======
					let (updated_b,updated_prev) = update_unmutable all_moves.(!i) b p in
					let (score,new_best_steps) = alphaBeta alpha !v (depth_left - 1)
								updated_b updated_prev ai_col (not curr_rd) in
					if (score < !v) then
						(v:= score;
						best_steps:= all_moves.(!i)::new_best_steps;
						i:= !i+1)
					else if (score < alpha) then (result := alpha; i:= !i+1)
					else i:= !i+1
>>>>>>> 80ad638a4581ad392a89d511814cd7dcec6676c8
				done;
				if !result = alpha then (alpha,!best_steps) else (!v,!best_steps)
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
<<<<<<< HEAD
let best_move_v0 (n:int) (b:board) (p:step list): step list =
	(* need to modify the alpha beta to keep track of the optimum steps *)
	let (score, next_steps) =
		alphaBeta (int_of_float neg_infinity) (int_of_float infinity)
			n b p col round in
	next_steps
=======
let best_move_v0 (n:int) (b:board) (p:prev_step) : int*step list =
	(* need to modify the alpha beta to keep track of the optimum steps *)
		alphaBeta min_int max_int
			n b p col round
>>>>>>> 80ad638a4581ad392a89d511814cd7dcec6676c8

(*generate the best possible moves for the futural several steps
* int below is how many steps we predict for the future*)
let best_move (n:int) (b:board) (tran:transposition_table) (hist:history_table) : step list = 
raise TODO

(*update the historical information*)
let update_AI (b:board) (s:step) (tran:transposition_table) (hist:history_table) 
				: transposition_table * history_table = 
  raise TODO

let easy_AI (b:board) (tran:transposition_table) (hist:history_table) 
  				: step list * transposition_table * history_table =
  raise TODO
  
let hard_AI (b:board) (tran:transposition_table) (hist:history_table) 
  				: step list * transposition_table * history_table =
  raise TODO
