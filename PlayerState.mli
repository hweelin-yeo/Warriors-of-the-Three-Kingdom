(* PlayerState.ml

 * [player_state] is a record type containing the following data:
	- player_id
	- player_score
	- player_deck
*)

type playerID
type cardID

type player_state

(* [generate_player_id_int i] generates the player_id_int, i, of a player *)
(* val generate_player_id_int: int -> int *)

(* [generate_player_id i] generates the player_id of a player *)
(* val generate_player_id: int -> playerID *)

(* [change_player_score s i] changes the score of a player by i *)
val change_player_score: player_state -> int -> player_state

(* [add_card s c] adds card c to the player_deck *)
val add_card: player_state -> cardID -> player_state

(* [remove_card s c] removes card c from the player_deck *)
val remove_card: player_state -> cardID -> player_state

(* [init_player_state i] initialises a player_state, with player_id_int i *)
val init_player_state: int -> player_state

(* [skip_turn s] is the player_state with the player's resource incremented
   by 1. Nothing else in the state changes.*)
val skip_turn: player_state -> player_state
