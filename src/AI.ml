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

let generate_all_moves (b:board) (p:prev_step) (side:bool): step list=
	let all_pieces = get_alive_side b side in
	let each_steps = List.map 
		(fun a -> let l = generate_piece_move b p a in List.iter print_step l; l) all_pieces in
	List.flatten each_steps


let rec alphaBeta (alpha:int) (beta:int) (depth_left:int)
	(b:board) (p:prev_step) (ai_col:bool) (curr_rd:bool): int*step list =

	if depth_left = 0 then ((eval_board b),[])
	else
		(if curr_rd = ai_col then 
			(print_endline "1";
			let result = ref min_int in
			print_int !result;
			print_string "here\n";
			let v = ref alpha in
			let i = ref 0 in
			let best_steps = ref [] in
			(*begin match (generate_all_moves b p col) with*)
			let h_e = (generate_piece_move b p soldR1)@(generate_piece_move b p soldR2)
				@(generate_piece_move b p soldR3)@(generate_piece_move b p soldR4)@
				(generate_piece_move b p soldR5)@(generate_piece_move b p horseR1) in
			begin match h_e with
			| [] -> print_endline "1.1";(depth_left - max_int,[])
			| l -> print_endline "1.2";
				let all_moves = Array.of_list l in
				print_endline "1.3";
				print_int (Array.length all_moves);
				print_string "hi\n";
				print_int beta;
				print_string "hello\n";
				print_int !result;

				while ((!i < (Array.length all_moves)) && (beta <> !result))
				do
					print_endline "1.4";
					let (updated_b,updated_prev) = update_unmutable all_moves.(!i) b p in
					let (score,new_best_steps) = alphaBeta !v beta (depth_left - 1)
								updated_b updated_prev ai_col (not curr_rd) in
					if (score > !v) then
						(v:= score;
						best_steps:= all_moves.(!i)::new_best_steps;
						i:= !i+1)
					else if (score > beta) then (result := beta; i:= !i+1)
					else i:= !i+1
				done;
				if !result = beta then (beta,!best_steps) 
				else (!v,!best_steps)
			end)

		else
			(print_endline "2";
			let result = ref max_int in
			let v = ref beta in
			let i = ref 0 in
			let best_steps = ref [] in
			(*begin match generate_all_moves b p col with*)
			begin match (generate_piece_move b p horseR1) with
			| [] -> ((-depth_left) - min_int,[])
			| l ->
				let all_moves = Array.of_list l in
				while ((!i < (Array.length all_moves)) && (alpha <> !result))
				do
					let (updated_b,updated_prev) = update_unmutable all_moves.(!i) b p in
					let (score,new_best_steps) = alphaBeta alpha !v (depth_left - 1)
								updated_b updated_prev ai_col (not curr_rd) in
					if (score < !v) then
						(v:= score;
						best_steps:= all_moves.(!i)::new_best_steps;
						i:= !i+1)
					else if (score < alpha) then (result := alpha; i:= !i+1)
					else i:= !i+1
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
let best_move_v0 (n:int) (b:board) (p:prev_step) : int*step list =
	(* need to modify the alpha beta to keep track of the optimum steps *)
		alphaBeta min_int max_int
			n b p col round

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