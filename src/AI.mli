open Board
open Move

module type AI=
sig
  type board

  val sort: board->move list

  val best_move: int->board->move list
end
