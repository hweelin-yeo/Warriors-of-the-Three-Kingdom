(* Ai.mli

 * As this game will be able to be played between multiple human players,
 * as well as an AI, an Ai.mli file is needed to perform a move.
 * This is the AI that players will be able to play against
*)

open State
open PlayerState

(**
 * returns: [easy_ai_next_move s] is the card that the easy difficulty AI
 * will play in a turn given a state [s].
 * requires: [s] is a state.
*)
val easy_ai_next_move: state -> cardID option

(**
 * returns: [medium_ai_next_move s] is the card that the medium difficulty AI
 * will play in a turn given a state [s].
 * requires: [s] is a state.
*)
val medium_ai_next_move: state -> cardID option
