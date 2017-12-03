(* state.ml *)

open PlayerState

type playerID = string
type cardID = int

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

type card = {
  card_name : string;
  card_id : int;
  cost : int;
  faction : string;
  power : int;
  flavor : string;
  abilities : state -> int -> state;
  card_type : string;
}


(*
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
  }*)


(*Function for vanilla cards, simulates a blank abilities box thus
the returned state is the same as the called state*)
let vanilla (s : state) (cid : cardID) = s

(*id_to_card takes a card id int and returns the card object option associated with it.
  The inputs are the card id and the card list that represents the card set*)
let rec id_to_card (id : cardID) (cl : card list) =
  match cl with
  | [] -> None (*Should not happen*)
  | h :: t -> if (h.card_id = id) then Some h
    else
      id_to_card id t

(*map_id_list maps a list of card ids to their respective cards*)
let rec map_id_list (idl : cardID list) (cl : card list) acc =
  match idl with
  | [] -> List.rev acc
  | h :: t -> map_id_list t cl ((id_to_card h cl) :: acc)

(*find_player, given the list of int player_state tuples, and a int player id,
  return the corresponding player_state*)
let rec find_player (psl : (int * player_state) list) (id : int) =
  match psl with
  | [] -> failwith "Player not found"
  | h :: t -> if (fst h = id) then h else find_player t id


(*compute_deck_score takes in a card_list that represents the deck and returns
  the total score of the deck*)
let rec compute_deck_score (cl : card list) acc =
  match cl with
  | [] -> acc
  | h :: t -> compute_deck_score t (h.power + acc)

(*zhuge_liang_funct*)
(*let zhuge_liang_funct (s : state) (cid : int) =
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player currentPlayerInt s.player_states in *)



    (*List of card records that simulates every card available*)
let cardList = [
  {
    card_name = "Shu Footsoldier";
    card_id = 1;
    cost = 1;
    faction = "Shu";
    power = 1;
    flavor = "Liu Bei's soldiers were recruited from the farmhands of \n China";
    abilities = vanilla;
      card_type = "Soldier"
  };

  {
    card_name = "Zhuge Liang, Sleeping Dragon";
    card_id = 1;
    cost = 4;
    faction = "Shu";
    power = 1;
    flavor = "The greatest strategist of the Three Kingdoms was the Sleeping \n
Dragon, Zhuge Liang";
    (*Dummy testing value here*)
    abilities = vanilla;
(*fun (s : state) (p : player_state) (c_id : int) ->
                  zhuge_liang_funct s p c_id;*)
      card_type = "Soldier"
  }
]

    (****************************************************************************************************)

let rec contains e lst =
  match lst with
  | [] -> false
  | h :: t -> if h = e then true else contains e t

let rec generate_nums num bound accum =
  if (List.length accum < num) then
    let new_num = Random.int bound in
    if (contains new_num accum) then generate_nums num bound accum
    else generate_nums num bound (new_num :: accum)
  else accum

(* [cpicks_from_index num_lst card_lst] returns a list of elements in
    card_lst whose positions correspond to the numbers in the num_lst *)

let rec picks_from_index num_lst card_lst =
  match num_lst with
  | [] -> []
  | h :: t -> (List.nth card_lst h) :: picks_from_index t card_lst

(* [refresh_available_picks s] returns a card list that corresponds
   to the next set of randomised available picks *)

let refresh_available_picks st =
  let n = List.length (st.recruit_pool) in
  picks_from_index (generate_nums 3 n []) (st.recruit_pool)


let return_next_player st prev =
  if prev = st.total_players then 1
  else prev+1

let change_next_player (st: state) =
  { st with card_drawn = None;
            current_player = return_next_player st (st.current_player);
            available_picks = refresh_available_picks st
  }

let change_to_player st i =
  { st with card_drawn = None;
            current_player = i;
            available_picks = refresh_available_picks st}

(* SPECIFICATION  *)

let card_f_state st f = f st

(* [change s p_id c_id f] returns a new player state after f, a card
   function of card with id [c_id] is applied to the state. *)

(* let change s p_id c_id f = failwith "unimplemented" *)
let add_card_recruit_pool st card =
  { st with recruit_pool = card :: st.recruit_pool}

let rec remove_card pool card =
  match pool with
  | [] -> []
  | h :: t -> if h = card then t else
      h :: remove_card t card

let remove_card_recruit_pool st card =
  let recpool_new = remove_card (st.recruit_pool) card in
  { st with recruit_pool = recpool_new }

  (* [return_pstate ps i] is a helper function for [return_player_state s id].
    It returns a playerstate with player_id_int [i]. *)

let rec return_pstate ps i =
  match ps with
  | [] -> failwith "No player with that id found"
  | (i, h) :: t -> if h.player_id_int = i then h else return_pstate t i

(* [return_player_state s id] returns a playerstate with id [id]. *)

let rec return_player_state s i =
  return_pstate (s.player_states) i

(* [change_player_state s i] returns a state with playerstate,
   whose player_id_int is 1, changed. *)

let change_player_state s ps  =
  let i = s.current_player in
  let pstates = List.remove_assoc (i) (s.player_states) in
  let new_player_states = (i, ps) :: pstates in
  { s with player_states = new_player_states}

let draw_card st c =
  let remove_rec_pool = remove_card_recruit_pool st c in
  let ps = return_player_state st (st.current_player) in
  let new_ps = (add_card c ps) in
  let changed_ps = change_player_state remove_rec_pool new_ps in
  { changed_ps with card_drawn = Some c }

(* [init_player_states n h accum] returns a list of playerstates,
   where n is the number of playerstates, h is the number of human
   players *)

let rec init_player_states n h accum =
  match n with
  | 0 -> accum
  | _ -> begin
    match h with
      | 0 -> let new_ps = (n, (init_player_state n false)) in
        init_player_states (n-1) 0 (new_ps :: accum)
      | _ -> let new_ps = (n, init_player_state n true) in
        init_player_states (n-1) (h-1) (new_ps :: accum)
    end

(* [generate_0_to_n_lst n accum] returns an int list [0; ...; n]
 * requires: n is an int *)
let rec generate_0_to_n_lst n accum =
  match n with
  | 0 -> accum
  | _ -> generate_0_to_n_lst (n-1) (n :: accum)

let init_state i h =
  {
     (* description = "";
        sec_description = "" :: []; *)
    total_players = i;
    card_drawn = None;
    current_player = 1;
    (* current_player_id = "Player 1"; *)
    recruit_pool = generate_0_to_n_lst 23 [];
    available_picks = generate_nums 3 23 [];
    player_states = init_player_states i h [];
  }



(* let change_description st str =
  { st with description = str }

let change_sec_description st str =
  { st with sec_description = str } *)
