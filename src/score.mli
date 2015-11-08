open Board

module type Score=
sig
  type board

  val compute_score: board->int
end
