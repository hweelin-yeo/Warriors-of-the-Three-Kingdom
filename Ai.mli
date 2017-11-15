(* Ai.mli

 * As this game will be able to be played between multiple human players,
 * as well as an AI, an Ai.mli file is needed to perform a move.
 * This is the AI that players will be able to play against
*)

open State
open PlayerState

(* [ai_generate_next_move s] perform a move, returning a new game state. *)

val ai_generate_next_move: state -> cardID
