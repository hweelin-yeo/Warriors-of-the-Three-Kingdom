(* testing for playerstate *)
open OUnit2
open PlayerState

let tests =
  [
"init_player_state_1" >:: (fun _ -> assert_equal 1 ((init_player_state 1 false).player_id));
"init_player_state_3" >:: (fun _ -> assert_equal 0 ((init_player_state 1 false).player_score));
"init_player_state_4" >:: (fun _ -> assert_equal [] ((init_player_state 1 false).player_deck));
"init_player_state_5" >:: (fun _ -> assert_equal 1 ((init_player_state 1 false).player_resource));

"change_player_score_1" >:: (fun _ -> assert_equal 5 ((init_player_state 1 false) |> change_player_score 5).player_score);
"change_player_score_2" >:: (fun _ -> assert_equal 0 ((init_player_state 1 false) |> change_player_score (-5)).player_score);

"remove_card_1" >:: (fun _ -> assert_equal [] ((init_player_state 1 false) |> add_card 1 |> remove_card 1).player_deck);

"skip_turn_1" >:: (fun _ -> assert_equal 2 ((init_player_state 1 false) |> skip_turn).player_resource);
"skip_turn_2" >:: (fun _ -> assert_equal 3 ((init_player_state 1 false) |> skip_turn |> skip_turn).player_resource);
"skip_turn_3" >:: (fun _ -> assert_equal 1 ((init_player_state 1 false) |> skip_turn).player_id);
"skip_turn_5" >:: (fun _ -> assert_equal 0 ((init_player_state 1 false) |> skip_turn).player_score);
"skip_turn_6" >:: (fun _ -> assert_equal [] ((init_player_state 1 false) |> skip_turn).player_deck);

"change_player_resource_1" >:: (fun _ -> assert_equal 2 ((init_player_state 1 false) |> change_player_resource 1).player_resource);
"change_player_resource_2" >:: (fun _ -> assert_equal 1 ((init_player_state 1 false) |> change_player_resource 0).player_resource);
"change_player_to_ai_1" >:: (fun _ -> assert_equal false ((init_player_state 1 false) |> change_player_to_ai).player_is_human);
"change_player_to_human_1" >:: (fun _ -> assert_equal true ((init_player_state 1 false) |> change_player_to_human).player_is_human);
  ]

let suite =
  "PlayerState test suite"
  >::: tests

let _ = run_test_tt_main suite
