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

(* [change_available_picks s] returns a new state where
   state's available_picks randomised   to be chosen by
   player*)
val change_available_picks: state -> state

(* [change_description st s] returns a new state with
   description s *)
val change_description: state -> string -> state

(* [change_sec_description st s] returns a new state with
   secondary description s *)
val change_sec_description: state -> string -> state

(* [change_current_player st p] returns a new state with
   the next player p *)
val change_current_player: state -> playerID -> state

(* [add_card s c] adds card c to the recruit_pool *)
val add_card_recruit_pool: state -> cardID -> player_state

(* [remove_card s c] removes card c from the recruit_pool *)
val remove_card_recruit_pool: state -> cardID -> player_state

(* [init_state i j] removes card c from the recruit_pool *)
val init_state: int -> Yojson.Basic.json -> state
