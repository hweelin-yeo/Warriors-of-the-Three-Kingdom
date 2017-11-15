(* PlayerState.ml

 * [player_state] is a record type containing the following data:
	- player_id
	- player_score
	- player_deck
*)

type playerID = string
type cardID = string

type player_state = {player_id: string;
                     player_score: string;
                     player_deck: string list
                    }

(* [generate_player_id i] generates the player_id of a player *)
val generate_player_id: int -> playerID

(* [change_player_score s i] changes the score of a player by i *)
val change_player_score: player_state -> int -> player_state

(* [add_card s c] adds card c to the player_deck *)
val add_card: player_state -> cardID -> player_state

(* [remove_card s c] removes card c from the player_deck *)
val remove_card: player_state -> cardID -> player_state
