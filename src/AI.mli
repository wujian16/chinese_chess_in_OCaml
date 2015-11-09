open Board
open Move

module type AI=
sig
  type board

  type transposition_table

  type history_table

  val sort: board->step list

  val best_move: int->board->transposition_table->history_table->step list

  val update: transposition_table*history_table->transposition_table*history_table
end
