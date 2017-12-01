open PlayerState
open State

let add x y = x + y

type card = {
  card_name : string;
  card_id : int;
  cost : int;
  faction : string;
  power : int;
  flavor : string;
  abilities : state -> player_state -> int -> state;
  card_type : string;
}

let rec zhuge_liang_deck_helper (pd : player_deck) acc =
  match pd with
  | [] -> acc []
  | h :: t -> if (h.faction = "Shu") then
      zhuge_liang_deck_helper t (h :: h :: acc)
    else
      zhuge_liang_deck_helper t (h :: acc)

let rec compute_deck_score (pd : player_deck) acc =
  match pd with
  | [] -> acc
  | h :: t -> compute_deck_score t (h.power + acc)

let rec new_players_list (psl : player_state list) (id : player_id_int) (npl : player_state) acc =
  match psl with
  | [] -> acc
  | h :: t -> if h.player_id_int = id then new_player_list t id npl (npl :: acc)
    else new_player_list t id npl (h :: acc)

let zhuge_liang_funct (s : state) (p : player_state) (c_id : int) =
  let new_deck = zhuge_liang_deck_helper p.player_deck in
  let new_player_state = {
    playerID = p.playerID;
    player_id_int = p.player_id_int;
    player_score = compute_player_score new_deck;
    player_deck = new_deck;
    player_resource = p.player_resource;
    player_is_human = p.player_is_human;
  }  in
  let new_players_list s.player_states p.player_id_int new_player_state [] in
  {
    total_players = s.total_players;
    card_drawn = s.card_drawn;
    current_player = s.current_player;
    recruit_pool = s.recruit_pool;
    available_picks = s.available_picks;
    player_states = new_players_list;
  }



let cardList = [
  {
    card_name = "Shu Footsoldier";
    card_id = "1";
    cost = 1;
    faction = "Shu";
    power = 1;
    flavor = "Liu Bei's soldiers were recruited from the farmhands of \n China";
    abilities = fun (s : state) (p : player_state) (c_id : int) -> s;
    card_type = "Soldier"
  };
  {
    card_name = "Zhuge Liang, Sleeping Dragon";
    card_id = "1";
    cost = 4;
    faction = "Shu";
    power = 1;
    flavor = "The greatest strategist of the Three Kingdoms was the Sleeping \n
Dragon, Zhuge Liang";
    abilities = fun (s : state) (p : player_state) (c_id : int) ->
      zhuge_liang_funct s p c_id;
  }
]
