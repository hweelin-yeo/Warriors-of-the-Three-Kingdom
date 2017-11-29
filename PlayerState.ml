type playerID = string
type cardID = string

type player_state = {player_id: playerID;
                     player_resource: int;
                     player_score: int;
                     player_deck: cardID list}

let generate_player_id x = "Player " ^ (string_of_int x)

let change_player_score s i =
  {s with player_score = s.player_score + i}

let add_card s c =
  {s with player_deck = c :: s.player_deck}

let remove_card s c =
  {s with player_deck = List.filter (fun x -> x <> c) s.player_deck}
