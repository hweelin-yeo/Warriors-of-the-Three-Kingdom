(* changed the function specifications for add_card a little bit.
   Now it does not allow "" as cardID *)



type playerID = string
type cardID = string

type player_state = {player_id: playerID; (* Does not change *)
                     player_id_int: int; (* Does not change *)
                     player_score: int;
                     player_deck: cardID list;
                     player_resource: int}

(* let generate_player_id x = "Player " ^ (string_of_int x) *)

let change_player_score i s =
  {s with player_score = max 0 (s.player_score + i)}

let add_card c s =
  {s with player_deck = List.filter (fun x -> x <> "") (c :: s.player_deck)}

let remove_card c s =
  {s with player_deck = List.filter (fun x -> x <> c) s.player_deck}

let init_player_state i =
  {
    player_id = "Player " ^ (string_of_int i);
    player_id_int = i;
    player_score = 0;
    player_deck = [];
    player_resource = 1;
  }

let skip_turn s =
  {s with player_resource = s.player_resource + 1}

let change_player_resource i s =
  {s with player_resource = s.player_resource + i}
