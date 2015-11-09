open Board

(* the following type tells one step during one game*)
type step={start:position; destination: position; piece_captured: piece option}

module type Move_Info:
sig  
  type board

  type prev_step

  val check_valid: board->prev_step->step->bool

  val check_win: board->prev_step->step->bool

  val checked: board->prev_step->step->bool

  val update: (board,prev_step)->(board,prev_step) option

  val generate_piece_move: board->prev_step->piece->step list option
end 
