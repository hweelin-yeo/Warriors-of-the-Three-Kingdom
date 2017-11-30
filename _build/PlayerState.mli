(* PlayerState.ml

 * [player_state] is a record type containing the following data:
	- player_id
	- player_score
	- player_deck
*)

type playerID
type cardID

type player_state = {player_id: playerID;
                     player_id_int: int;
                     player_score: int;
                     player_deck: cardID list;
                     player_resource: int}

(* [generate_player_id_int i] generates the player_id_int, i, of a player *)
(* val generate_player_id_int: int -> int *)

(* [generate_player_id i] generates the player_id of a player *)
(* val generate_player_id: int -> playerID *)

(* [change_player_score i s] changes the score of a player in player_state [s]
   by [i]. The score will be set to 0 if it goes below 0. *)
val change_player_score: int -> player_state -> player_state


(* [add_card c s] adds card [c] to the player_deck in [s]. If the [c] is ""
   then no card will be added to the deck. *)
val add_card: cardID -> player_state -> player_state

(* [remove_card c s] removes card [c] from the player_deck in [s]. The player's
   card deck will remain unchanged if the player does not have [c]. *)
val remove_card: cardID -> player_state -> player_state


(* [init_player_state i] initialises a player_state, with player_id_int [i]. *)
val init_player_state: int -> player_state

(* [skip_turn s] is the player_state with the player's resource incremented
   by 1. Nothing else in the state changes. *)
val skip_turn: player_state -> player_state

(* [change_player_resource i s] is [s] with the player's resource
   incremented by [i]. *)
val change_player_resource: int -> player_state -> player_state
