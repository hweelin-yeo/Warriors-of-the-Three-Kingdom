open PlayerState
open State

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
  }
]
