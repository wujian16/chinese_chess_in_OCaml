(* The main file for REPL
 * including printing the chess board
 * printing the information needed for player
 * starting some dialogue with player
 * starting the game, ending the game, etc*)
open Board
open Move
open Piece
open Score
open AI

(*types for commands*)
type cmd=undo|ai|twoplayer|quit|help|coordinate

(*parse*)
val parse: string->cmd
