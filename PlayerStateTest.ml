(* testing for playerstate *)
open OUnit2
open PlayerState

let tests =
  [
    (* Some test cases for threerooms.json *)
    "three_max" >:: (fun _ -> assert_equal 1 (1 |> init_player_state).player_id_int);
    "three_score" >:: (fun _ -> assert_equal 5 ((change_player_score (1 |> init_player_state) 5).player_score));
    "three_turns" >:: (fun _ -> assert_equal ["Card1"] (add_card (1 |> init_player_state) "Card1").player_deck);
  ]

let suite =
  "PlayerState test suite"
  >::: tests

let _ = run_test_tt_main suite
