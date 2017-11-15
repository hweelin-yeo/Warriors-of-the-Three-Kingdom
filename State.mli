(* State.ml *)

(* A state contains the following:
   - description: description to be printed in REPL
   - sec_description: secondary description to be printed in REPL
   - current_player: the player who is currently having his/her turn
   - recruit_pool: deck of centralised cards
   - available_picks: 3 cards from recruit pool that current player can pick
   - player_states: list of player_states, which will be elaborated later
*)
open PlayerState

type state = {description: string;
              sec_description: string list;
              current_player: playerID;
              recruit_pool: cardID list;
              available_picks: cardID list;
              player_states: player_state list;
}

(* [change_description st s] returns a new state with description s *)
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

(* [change_player_score s i] changes the score of a player by i *)
val change_player_states: state -> state
