(* state.ml *)

open PlayerState
open RecruitPile


type playerID = string
type cardID = string

type state = {
  description: string;
  sec_description: string list;
  current_player: int;
  current_player_id: playerID;
  recruit_pool: card list;
  available_picks: card list;
  player_states: (int * player_state) list;
}

let change_description st str =
  { st with description = str}

let change_sec_description st str =
  { st with sec_description = str}

let change_current_player st pl =
  { st with current_player = pl}

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

let refresh_available_picks st =
  let n = List.length (st.recruit_pool) in
  let new_picks = picks_from_index (generate_nums 3 n []) (st.recruit_pool) in
  { st with available_picks = new_picks}

let rec init_player_states n accum =
  match n with
  | 0 -> accum
  | _ -> let new_player_st = (n, init_player_state n) in
    init_player_states (n-1) (new_player_st :: accum)

let init_state i j =
  { description = "";
    sec_description = "" :: [];
    current_player = 1;
    current_player_id = "Player 1";
    recruit_pool = init_pile j;
    available_picks = picks_from_index (generate_nums 3 24 []) (init_pile j);
    player_states = init_player_states i [];
  }

(* [return_pstate ps i] is a helper function for [return_player_state s id].
    It returns a playerstate with player_id_int [i]. *)

let rec return_pstate ps i =
  match ps with
  | [] -> failwith "No player with that id found"
  | (i, h) :: t -> if h.player_id_int = id then h else return_pstate t id

(* [return_player_state s id] returns a playerstate with id [id]. *)

let rec return_player_state s i =
  return_pstate (s.player_states) i

let change_player_state s i f =
  let ps = return_player_state s i in
  let changed_ps = f ps in
  let pstates = List.remove_assoc i (s.player_states) in
  let new_player_states = (i, changed_ps) :: pstates in
  { s with player_states = new_player_states}
