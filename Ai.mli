(* Ai.mli

 * As this game will be able to be played between multiple human players,
 * as well as an AI, an Ai.mli file is needed to perform a move.
 * This is the AI that players will be able to play against
*)

open State
open PlayerState

(**
 * returns: [greedy_ai_next_move s] is the cardID that the greedy AI will
 * play in a turn given a state [s].
 * requires: [s] is a state.
*)
val greedy_ai_next_move: state -> cardID
