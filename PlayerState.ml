type playerID = string
type cardID = string

type player_state = {player_id: playerID;
                     player_id_int: int;
                     player_score: int;
                     player_deck: cardID list;
                     player_resource: int}

(* let generate_player_id x = "Player " ^ (string_of_int x) *)

let change_player_score s i =
  {s with player_score = s.player_score + i}

let add_card s c =
  {s with player_deck = c :: s.player_deck}

let remove_card s c =
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
