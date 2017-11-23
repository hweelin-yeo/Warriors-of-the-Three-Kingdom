open Yojson.Basic.Util

(*
current design is based on json that looks like this
{ soldiers: [{card_name, card_id, faction....}, {card_name, card_id...}]
, spells: [{card_name, card_id, faction....}, {card_name, card_id...}]
}
*)

type json = [
  | `Assoc of (string * json) list
  | `Bool of bool
  | `Float of float
  | `Int of int
  | `List of json list
  | `Null
  | `String of string
]

type soldier = {
  card_name: string;
  card_id: int;
  faction: string;
  rarity: int;
  power: int;
  flavor: string;
  abilities: string list;
  card_type: string;
}

type spell = {
  card_name: string;
  card_id: int;
  cost: int;
  faction: string;
  rarity: int;
  flavor: string;
  abilities: string list;
  card_type: string;
}

type card = Soldier of soldier | Spell of spell

type recruitPile = card list

let soldier_of_json j = Soldier {
  card_name = j |> member "card_name" |> to_string;
  card_id = j |> member "card_id" |> to_int;
  faction =  j |> member "faction" |> to_string;
  rarity =  j |> member "rarity" |> to_int;
  power =  j |> member "power" |> to_int;
  flavor =  j |> member "flavor" |> to_string;
  abilities = j |> member "abilities" |> to_list |> List.map to_string;
  card_type = j |> member "type" |> to_string;
}

let spell_of_json j = Spell {
  card_name = j |> member "card_name" |> to_string;
  card_id = j |> member "card_id" |> to_int;
  cost = j |> member "card_id" |> to_int;
  faction =  j |> member "faction" |> to_string;
  rarity =  j |> member "rarity" |> to_int;

  flavor =  j |> member "flavor" |> to_string;
  abilities = j |> member "abilities" |> to_list |> List.map to_string;
  card_type = j |> member "type" |> to_string
}

let pile_of_json j =
  let soldiers = j |> member "soldiers" |> to_list |> List.map soldier_of_json in
  let spells = j |> member "spells" |> to_list |> List.map spell_of_json in
  soldiers @ spells

let init_pile j = pile_of_json j

let rec take_card s p =
  match p with
  | [] -> p
  | h :: t -> if h.card_name = s then t
    else h :: take_card s t

let add_card c p = c :: p

let get_size p = List.length p
