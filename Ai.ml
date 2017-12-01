open State
open PlayerState
open RecruitPile


let rec find_card card_lst highest_card f =
  match card_lst with
  | [] -> highest_card
  | h :: t -> (
      if f h.power highest_card.power
      then find_highest_card t h f
      else find_highest_card t highest_card f
    )

let medium_ai_next_move s =
  let current_player_state = List.assoc s.current_player s.player_states in
  let enough_resources_list = List.filter (fun x -> x.cost <= current_player_state.player_resource) s.available_picks in
  find_highest_card enough_resources_list vanilla_card

(* AKIRA: [find_rank st id] returns the rank of the current player of
   player_id [id] *)
let find_rank st id = failwith "unimplemented"

(* AKIRA: [find_diff st id] returns the difference between the score of the
   current player of player_id [id] and the player ranked below him *)
let find_diff st id = failwith "unimplemented"

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
