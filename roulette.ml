(*Roulette Function*)

let roulette _ =
	(*let result = Int32.to_int (Random.int32 (Int32.of_int(37))) in*)
	let result = Random.int 37 in
	let col = color result in
	String.concat " " [string_of_int (result); col];;

(*Helper Function -- Color*)

let color x =
	if x = 0 then "green"
	else if (x >= 1 && x <=10) || (x >= 19 && x <= 28) then
		if x mod 2 = 0 then "black"
		else "red"
	else
		if x mod 2 = 0 then "red"
		else "black"
	;;
