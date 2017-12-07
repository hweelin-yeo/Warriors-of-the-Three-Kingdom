(* Ai.mli

 * As this game will be able to be played between multiple human players,
 * as well as an AI, an Ai.mli file is needed to perform a move.
 * This is the AI that players will be able to play against
*)

open State
open PlayerState

type player_state =
  {
    player_id: playerID; (* Does not change *)
    player_score: int;
    player_deck: int list;
    player_resource: int;
    player_is_human: bool;
  }

type state = {
  (* description: string;
     sec_description: string list; *)
  total_players: int;
  card_drawn: cardID option;
  current_player: int;
  (* current_player_id: playerID; *)
  recruit_pool: cardID list;
  available_picks: cardID list;
  player_states: (int * player_state) list;
}

(**
 * returns: [easy_ai_next_move s] is the card that the easy difficulty AI
 * will play in a turn given a state [s]. Given a state, this AI will choose
 * a random card out of all the available picks. It will skip if it cannot
 * choose any card.
 * requires: [s] is a state.
*)
val easy_ai_next_move: state -> cardID option

(**
 * returns: [medium_ai_next_move s] is the card that the medium difficulty AI
 * will play in a turn given a state [s]. Given a state, this AI will choose
 * the card with the highest power out of all the available picks.
 * It will skip if it cannot choose any card.
 * requires: [s] is a state.
*)
val medium_ai_next_move: state -> cardID option

(**
 * returns: [hard_ai_next_move s] is the card that the hard difficulty AI
 * will play in a turn given a state [s]. Given a state, this AI will choose
 * the card that will put the player at the highest rank possible. If two or
 * more cards result in the same rank, it will choose the card that will have
 * the biggest difference in the power against the player with the rank one
 * below. It will skip if it cannot choose any card.
 * requires: [s] is a state.
*)
val hard_ai_next_move: state -> cardID option
