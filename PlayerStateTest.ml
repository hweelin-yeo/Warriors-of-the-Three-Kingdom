(* testing for playerstate *)
open OUnit2
open PlayerState

let tests =
  [
"init_player_state_1" >:: (fun _ -> assert_equal "Player 1" (1 |> init_player_state).player_id);
"init_player_state_2" >:: (fun _ -> assert_equal 1 (1 |> init_player_state).player_id_int);
"init_player_state_3" >:: (fun _ -> assert_equal 0 (1 |> init_player_state).player_score);
"init_player_state_4" >:: (fun _ -> assert_equal [] (1 |> init_player_state).player_deck);
"init_player_state_5" >:: (fun _ -> assert_equal 1 (1 |> init_player_state).player_resource);

"change_player_score_1" >:: (fun _ -> assert_equal 5 (1 |> init_player_state |> change_player_score 5).player_score));
"change_player_score_2" >:: (fun _ -> assert_equal 0 (1 |> init_player_state |> change_player_score -5).player_score));

"add_card_1" >:: (fun _ -> assert_equal ["Card 1"] (1 |> init_player_state |> add_card "Card 1").player_deck);
"add_card_2" >:: (fun _ -> assert_equal [] (1 |> init_player_state |> add_card "").player_deck);

"remove_card_1" >:: (fun _ -> assert_equal [] (1 |> init_player_state |> add_card "Card 1" |> remove_card "Card 1").player_deck);
"remove_card_2" >:: (fun _ -> assert_equal ["Card 1"] (1 |> init_player_state |> add_card "Card 1" |> remove_card "Card 2").player_deck);
"remove_card_3" >:: (fun _ -> assert_equal [] (1 |> init_player_state |> remove_card "Card 1").player_deck);

"skip_turn_1" >:: (fun _ -> assert_equal 2 (1 |> init_player_state |> skip_turn).player_resource);
"skip_turn_2" >:: (fun _ -> assert_equal 3 (1 |> init_player_state |> skip_turn |> skip_turn).player_resource);
"skip_turn_3" >:: (fun _ -> assert_equal "Player 1" (1 |> init_player_state |> skip_turn).player_id);
"skip_turn_4" >:: (fun _ -> assert_equal 1 (1 |> init_player_state |> skip_turn).player_id_int);
"skip_turn_5" >:: (fun _ -> assert_equal 0 (1 |> init_player_state |> skip_turn).player_id_score);
"skip_turn_6" >:: (fun _ -> assert_equal [] (1 |> init_player_state |> skip_turn).player_deck);

"change_player_resource_1" >:: (fun _ -> assert_equal 2 (1 |> init_player_state |> change_player_resource 1).player_resource);
"change_player_resource_2" >:: (fun _ -> assert_equal 1 (1 |> init_player_state |> change_player_resource 0).player_resource);
  ]

let suite =
  "PlayerState test suite"
  >::: tests

let _ = run_test_tt_main suite
