(* testing for playerstate *)
open OUnit2
open Ai
open State
open PlayerState

type playerID = string
type cardID = int

type player_state =
  {
    player_id: playerID; (* Does not change *)
    player_id_int: int; (* Does not change *)
    player_score: int;
    player_deck: int list;
    player_resource: int;
    player_is_human: bool;
  }

type state = {
  (* description: string;
     sec_description: string list; *)
  total_players: int;
  card_drawn: cardID option;
  current_player: int;
  (* current_player_id: playerID; *)
  recruit_pool: cardID list;
  available_picks: cardID list;
  player_states: (int * player_state) list;
}

let player_1_state = {
  player_id = "Player 1";
  player_id_int = 1;
  player_score = 0;
  player_deck = [];
  player_resource = 100;
  player_is_human = false;
}

let player_2_state = {
  player_1_state with
  player_id = "Player 2";
  player_id_int = 2;
  player_is_human = true;
}

let player_3_state = {
  player_2_state with
  player_id = "Player 3";
  player_id_int = 3;
}

let player_4_state = {
  player_2_state with
  player_id = "Player 4";
  player_id_int = 4;
}

let sample_state_1 = {
  total_players = 4;
  card_drawn = None;
  current_player = 1;
  (* current_player_id: playerID; *)
  recruit_pool = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16;
                 17; 18; 19; 20; 21; 22; 23];
  available_picks = [0; 1; 2];
  player_states = [(1, player_1_state); (2, player_2_state);
                   (3, player_3_state); (4, player_4_state)];
}

let sample_state_2 = {
  sample_state_1 with
  available_picks = [3; 4; 5]
}

let sample_state_3 = {
  sample_state_1 with
  available_picks = [6; 7; 8]
}

let sample_state_4 = {
  sample_state_1 with
  available_picks = [9; 13; 19]
}

let sample_state_none = {
  sample_state_1 with
  player_states = [(1, {player_1_state with player_resource = 0});
                   (2, player_2_state); (3, player_3_state); (4, player_4_state)]
}

let tests =
  [
    "find_index_1" >:: (fun _ -> assert_equal 0 (find_index (1, 2) [(1, 2); (3, 4); (5, 6)] 3 0));
    "find_index_2" >:: (fun _ -> assert_equal 1 (find_index (3, 4) [(1, 2); (3, 4); (5, 6)] 3 0));
    "find_index_3" >:: (fun _ -> assert_equal 2 (find_index (5, 6) [(1, 2); (3, 4); (5, 6)] 3 0));

    "easy_1" >:: (fun _ -> assert_equal (Some 1) (easy_ai_next_move sample_state_1));
    (* "easy_2" >:: (fun _ -> assert_equal (Some 3) (easy_ai_next_move sample_state_2)); *)
    "easy_3" >:: (fun _ -> assert_equal (Some 6) (easy_ai_next_move sample_state_3));
    "easy_4" >:: (fun _ -> assert_equal (Some 19) (easy_ai_next_move sample_state_4));
    "easy_5" >:: (fun _ -> assert_equal (None) (easy_ai_next_move sample_state_none));

    "medium_1" >:: (fun _ -> assert_equal (Some 1) (medium_ai_next_move sample_state_1));
    (* "medium_2" >:: (fun _ -> assert_equal (Some 3) (medium_ai_next_move sample_state_2)); *)
    "medium_3" >:: (fun _ -> assert_equal (Some 6) (medium_ai_next_move sample_state_3));
    "medium_4" >:: (fun _ -> assert_equal (Some 19) (medium_ai_next_move sample_state_4));
    "medium_5" >:: (fun _ -> assert_equal (None) (medium_ai_next_move sample_state_none));
  ]

let suite =
  "AI test suite"
  >::: tests

let _ = run_test_tt_main suite
