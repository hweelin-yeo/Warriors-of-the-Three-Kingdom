(* PlayerState.ml

 * [player_state] is a record type containing the following data:
	- player_id
	- player_score
	- player_deck
*)

type playerID
type cardID

type player_state =
{
  player_id: playerID; (* Does not change *)
  player_id_int: int; (* Does not change *)
  player_score: int;
  player_deck: cardID list;
  player_resource: int;
  player_is_human: bool;
}

(**
 * returns: [change_player_score i s] changes the score of a player in
 * player_state [s] by [i]. The score will be set to 0 if it goes below 0.
 * requires:
 *  - [i] is an integer
 *  - [s] is a player_state
*)
val change_player_score: int -> player_state -> player_state

(**
 * returns: [add_card c s] adds card [c] to the player_deck in [s].
 * If [c] is "" then no card will be added to the deck.
 * requires:
 *  - [c] is a card
 *  - [s] is a player_state
*)
val add_card: cardID -> player_state -> player_state

(**
 * returns: [remove_card c s] removes card [c] from the player_deck in [s].
 * The player's card deck will remain unchanged if the player does not have [c].
 * requires:
 *  - [c] is a card
 *  - [s] is a player_state
*)
val remove_card: cardID -> player_state -> player_state

(**
 * returns: [init_player_state i] initialises a player_state,
 * with player_id_int [i]. The created player is played by a human.
 * requires: [i] is an int.
*)
val init_player_state: int -> player_state

(**
 * returns: [skip_turn s] is the player_state with the player's resource
 * incremented by 1. Nothing else in the state changes.
 * requires: [s] is a player_state.
*)
val skip_turn: player_state -> player_state

(**
 * returns: [change_player_resource i s] is [s] with the player's resource
 * incremented by [i].
 * requires:
 *  - [i] is an int
 *  - [s] is a player_state
*)
val change_player_resource: int -> player_state -> player_state

(* returns: [change_player_to_ai s] is [s] with the player played by an ai.
 * requires: [s] is a player_state *)
val change_player_to_ai: player_state -> player_state

(* returns: [change_player_to_human s] is [s] with the player played by a human.
 * requires: [s] is a player_state *)
val change_player_to_human: player_state -> player_state
