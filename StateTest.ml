open OUnit2
open State
open RecruitPile
open PlayerState


let init_state_i = init_state 4 1 (* file *)
(* Tests for value of each field for file recruitpile.json*)
let init_state_test =
  [
    "total_players" >:: (fun _ -> assert_equal 4 (init_state_1.total_players));
    "card_drawn" >:: (fun _ -> assert_equal None (init_state_1.card_drawn));
    "current_player" >:: (fun _ -> assert_equal 1 (init_state_1.current_player));
    (* "recruit_pool" >:: (fun _ -> assert_equal 24 (init_state_1.recruit_pool |> List.length)); *)
    "available_picks" >:: (fun _ -> assert_equal 3 (init_state_1.available_picks |> List.length));
    "player_states" >:: (fun _ -> assert_equal 4 (init_state_1.player_state |> List.length));
  ]

let c = List.nth init_state_1.available_picks 1
let pick_card_state_1 = init_state_i |> draw_card c

(* [card_drawn_bool st] returns true if card_drawn is Some c. *)

let card_drawn_bool st =
  match st.card_drawn with
  | None -> false
  | Some c -> true

(* [pldeck_has_card st c] returns true if card [c] is in current player's
   player deck. *)

let pldeck_has_card st c =
  let player = st.current_player in
  let playerst = return_player_state st player in
  let playerdeck = playerst.player_deck in
  contains c playerdeck

let pick_card_state =
  [
    "total_players" >:: (fun _ -> assert_equal 4 (pick_card_state_1.total_players));
    "card_drawn" >:: (fun _ -> assert_equal true (pick_card_state_1.card_drawn |> card_drawn_bool));
    "current_player" >:: (fun _ -> assert_equal 1 (pick_card_state_1.current_player));
    "recruit_pool" >:: (fun _ -> assert_equal 23 (pick_card_state_1.recruit_pool |> List.length));
    "available_picks" >:: (fun _ -> assert_equal 3 (pick_card_state_1.available_picks |> List.length));
    "player_state_change" >:: (fun _ -> assert_equal true (pldeck_has_card pick_card_state_1 c));
  ]

let state_next_player = pick_card_state_1 |> change_next_player

let change_next_player_test =
  [
    "total_players" >:: (fun _ -> assert_equal 4 (state_next_player.total_players));
    "card_drawn" >:: (fun _ -> assert_equal false (state_next_player.card_drawn |> card_drawn_bool));
    "current_player" >:: (fun _ -> assert_equal 2 (state_next_player.current_player));
    "recruit_pool" >:: (fun _ -> assert_equal 23 (state_next_player.recruit_pool |> List.length));
    "available_picks" >:: (fun _ -> assert_equal 3 (state_next_player.available_picks |> List.length));
    ]

let c = List.nth state_next_player.available_picks 1
let pick_card_state_2 = init_state_i |> draw_card c

let next_player_draws_test =
  [
    "total_players" >:: (fun _ -> assert_equal 4 (pick_card_state_2.total_players));
    "card_drawn" >:: (fun _ -> assert_equal true (pick_card_state_2.card_drawn |> card_drawn_bool));
    "current_player" >:: (fun _ -> assert_equal 2 (pick_card_state_2.current_player));
    "recruit_pool" >:: (fun _ -> assert_equal 22 (pick_card_state_2.recruit_pool |> List.length));
    "available_picks" >:: (fun _ -> assert_equal 3 (pick_card_state_2.available_picks |> List.length));
    "player_state_change" >:: (fun _ -> assert_equal true (pldeck_has_card pick_card_state_2 c));
  ]

let state_next_player = 4 |> change_to_player pick_card_state_1

let change_to_player_test =
  [
    "total_players" >:: (fun _ -> assert_equal 4 (pick_card_state_2.total_players));
    "card_drawn" >:: (fun _ -> assert_equal false (pick_card_state_2.card_drawn |> card_drawn_bool));
    "current_player" >:: (fun _ -> assert_equal 4 (pick_card_state_2.current_player));
    "recruit_pool" >:: (fun _ -> assert_equal 22 (pick_card_state_2.recruit_pool |> List.length));
    "available_picks" >:: (fun _ -> assert_equal 3 (pick_card_state_2.available_picks |> List.length));
    "player_states" >:: (fun _ -> assert_equal 4 (pick_card_state_2.player_state |> List.length));
  ]

let suite =
  "Adventure test suite"
  >::: List.flatten [
    init_state_test
  ]

let _ = run_test_tt_main suite
