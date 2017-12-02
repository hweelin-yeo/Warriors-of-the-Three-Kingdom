open State
open PlayerState
open RecruitPile


let rec find_card card_lst highest_card f =
  match card_lst with
  | [] -> highest_card
  | h :: t -> (
      if f h.power highest_card.power
      then find_card t h f
      else find_card t highest_card f
    )

let make_sorted_assoc_lst s =
  let current_player_state = List.assoc s.current_player s.player_states in
  let enough_resources_lst = List.filter (fun x -> x.cost <= current_player_state.player_resource) s.available_picks in
  let card_assoc_lst = List.map (fun x -> (x.power, x)) enough_resources_lst in
  List.sort compare card_assoc_lst

let easy_ai_next_move s =
  try
    Some (make_sorted_assoc_lst s |> List.rev|> List.hd |> snd)
  with Failure _ -> None
(* snd (List.hd (make_sorted_assoc_lst s)) *)

let medium_ai_next_move s =
  Some (make_sorted_assoc_lst s |> List.rev |> List.hd |> snd)

let rec find_index elem lst lst_size cur_idx =
  match lst with
  | [] -> lst_size - 1
  | h :: t ->
    if (fst h = fst elem) then cur_idx
    else find_index elem t lst_size (cur_idx + 1)

(* AKIRA: [find_rank st id] returns the rank of the current player of
   player_id [id]. [id] is an int.
*)
let find_rank st id =
  let player_lst = List.map (fun (x, y) -> (y.player_score, x)) st.player_states in
  let ranked_lst = List.rev (List.sort compare player_lst) in
  let ranked_lst_rev = List.map (fun (x, y) -> (y, x)) ranked_lst in (* (player_id * score)*)
  find_index (id, 0) ranked_lst_rev (List.length ranked_lst_rev) 1

(* AKIRA: [find_diff st id] returns the difference between the score of the
   current player of player_id [id] and the player ranked below him *)
let find_diff st id =
  let player_lst = List.map (fun (x, y) -> (y.player_score, x)) st.player_states in (* (score * player_id)*)
  let ranked_lst = List.rev (List.sort compare player_lst) in
  let ranked_lst_rev = List.map (fun (x, y) -> (y, x)) ranked_lst in (* (player_id * score)*)
  let id_idx = find_index (id, 0) ranked_lst_rev (List.length ranked_lst_rev) 1 in
  try (List.assoc ranked_lst_rev id) - fst (List.nth player_lst (id_idx + 1)) with Failure _ -> 0

(* [run_game_sim c st] returns a new state with card drawn as c *)
let run_indiv_sim c st = c |> draw_card st

(* [run_sims lst st rank_accum diff_accum] returns a association list of
   tuples: (rank of current player, diff between his score and the player
   ranked below him)

 * requires: lst to be of type card list
             st to be of type state
             rank_accum to be a tuple list,
             where typle is described as
             above.*)

let rec run_sims lst st rank_diff_accum =
  match lst with
  | [] -> rank_diff_accum
  | h :: t -> let new_st = run_indiv_sim h st in
    let rank = find_rank new_st st.current_player in
    let diff = find_diff new_st st.current_player in
    run_sims t st ((rank, diff) :: rank_diff_accum)
