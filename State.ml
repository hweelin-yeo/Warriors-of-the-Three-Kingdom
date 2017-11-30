(* state.ml *)

open PlayerState
open RecruitPile

type state = {
  description: string;
  sec_description: string list;
  current_player: playerID;
  recruit_pool: card list;
  available_picks: card list;
  player_states: player_state list;
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
  let new_picks = picks_from_index (generate_nums 3 24 []) (st.recruit_pool) in
  { st with available_picks = new_picks}

let rec init_player_states n accum =
  match n with
  | 0 -> accum
  | _ -> let new_player = init_player_state n in
    init_player_states (n-1) (new_player :: accum)

let init_state i j =
  {
    description = "";
    sec_description = "" :: [];
    current_player = "Player 1";
    recruit_pool = init_pile j;
    available_picks = picks_from_index (generate_nums 3 24 []) (init_pile j);
    player_states = init_player_states i [];
  }

(* [return_player_state s i] returns the playerstate who has id_int i*)

let return_player_state s
