open OUnit2
open State


let init_state_1 = init_state 4 1 (* file *)
(* Tests for value of each field for file recruitpile.json*)
let init_state_test =
  [
    "total_players" >:: (fun _ -> assert_equal 4 (init_state_1.total_players));
    "card_drawn" >:: (fun _ -> assert_equal None (init_state_1.card_drawn));
    "current_player" >:: (fun _ -> assert_equal 1 (init_state_1.current_player));
    (* "recruit_pool" >:: (fun _ -> assert_equal 1 (init_state_1.r)); *)
    "available_picks" >:: (fun _ -> assert_equal 3 (init_state_1.available_picks |> List.length));
    "player_states" >:: (fun _ -> assert_equal 3 (init_state_1.player_state |> List.length));
]

let pick_card_state_1 = init_state 4 1 |> draw_card
let pick_card_state =
  [
    "total_players" >:: (fun _ -> assert_equal 4 (init_state_1.total_players));
    "card_drawn" >:: (fun _ -> assert_equal None (init_state_1.card_drawn));
    "current_player" >:: (fun _ -> assert_equal 1 (init_state_1.current_player));
    (* "recruit_pool" >:: (fun _ -> assert_equal 1 (init_state_1.r)); *)
    "available_picks" >:: (fun _ -> assert_equal 3 (init_state_1.available_picks |> List.length));
    "player_states" >:: (fun _ -> assert_equal 3 (init_state_1.player_state |> List.length));
  ]
(* tests for do' function for all possible commands *)
let do_test =
  [
  ]

let suite =
  "Adventure test suite"
  >::: List.flatten [
    init_state_test
  ]

let _ = run_test_tt_main suite
