(* State.ml *)

(* A state contains the following:
   - description: description to be printed in REPL
   - sec_description: secondary description to be printed in REPL
   - current_player: the player who is currently having his/her turn
   - recruit_pool: deck of centralised cards
   - available_picks: 3 cards from recruit pool that current player can pick
   - player_states: list of player_states, which will be elaborated later
*)
open Yojson
open PlayerState
open RecruitPile

type state
type playerID = string
type cardID = int



(* [change_next_player st ] returns a new state for the next player *)
val change_next_player: state -> state

(* [change_current_player st p] returns a new state with
   the next player p
 * Use case: skip function
 * requires: p <= state.total_players *)
val change_to_player: state -> int -> state


(*--------------- KIV -------------------------*)
(* [change_description st s] returns a new state with
   description s *)
(* val change_description: state -> string -> state *)
(* [change_sec_description st s] returns a new state with
   secondary description s *)
(* val change_sec_description: state -> string list-> state *)
(*--------------- KIV -------------------------*)



(* [pick_card st c] returns a new state with
   card drawn as some c *)
val draw_card: state -> cardID -> state

(* [add_card s c] adds card c to the recruit_pool *)
val add_card_recruit_pool: state -> cardID -> state

(* [remove_card s c] removes card c from the recruit_pool *)
val remove_card_recruit_pool: state -> cardID -> state

(* [init_state i j] initialises the state. it takes in int i,
   the number of players, and a json file for the recruit pool*)
val init_state: int -> int -> state
