open Board

module type Score=
sig
  type board

  (*compute the score based on tje current board information*)
  val compute_score: board->int
end
