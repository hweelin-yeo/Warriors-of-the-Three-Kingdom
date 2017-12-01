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
