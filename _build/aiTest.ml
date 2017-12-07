(* testing for playerstate *)
open OUnit2
open Ai
open State
open PlayerState

type playerID = int
type cardID = int

let player_1_state = {
  player_id = 1;
  player_score = 0;
  player_deck = [];
  player_resource = 100;
  player_is_human = false;
  player_functions = [fun st -> st.player_id]
}
let player_2_state = {
  player_1_state with
  player_id = 2;
  player_is_human = true;
}

let player_3_state = {
  player_2_state with
  player_id = 3;
}

let player_4_state = {
  player_2_state with
  player_id = 4;
}

let player_1'_state = {
  player_id = 1;
  player_score = 30;
  player_deck = [0;1;2;3;4;5];
  player_resource = 100;
  player_is_human = false;
  player_functions = [fun st -> st.player_id]
}

let player_2'_state = {
  player_id = 1;
  player_score = 20;
  player_deck = [9;10];
  player_resource = 100;
  player_is_human = false;
  player_functions = [fun st -> st.player_id]
}

let sample_state_1' = {
  total_players = 4;
  card_drawn = None;
  current_player = 1;
  recruit_pool = [6; 7; 8; 11; 12; 13; 14; 15; 16;
                 17; 18; 19; 20; 21; 22; 23];
  available_picks = [6; 7; 8];
  player_states = [(1, player_1'_state); (2, player_2'_state)];
}

let sample_state_lt3 = {
  total_players = 4;
  card_drawn = None;
  current_player = 1;
  recruit_pool = [0; 1];
  available_picks = [0; 1];
  player_states = [(1, player_1_state); (2, player_2_state);
                   (3, player_3_state); (4, player_4_state)];
}

let sample_state_1 = {
  total_players = 4;
  card_drawn = None;
  current_player = 1;
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
  available_picks = [9; 10; 11]
}

let sample_state_5 = {
  sample_state_1 with
  available_picks = [12; 13; 14]
}

let sample_state_6 = {
  sample_state_1 with
  available_picks = [15; 16; 17]
}

let sample_state_7 = {
  sample_state_1 with
  available_picks = [18; 19; 20]
}

let sample_state_8 = {
  sample_state_1 with
  available_picks = [21; 22; 23]
}

let sample_state_9 = {
  sample_state_1 with
  available_picks = [0; 3; 4]
}

let sample_state_none = {
  sample_state_1 with
  player_states = [(1, {player_1_state with player_resource = 0});
                   (2, player_2_state); (3, player_3_state); (4, player_4_state)]
}

let tests =
  [
    "find_index_1" >:: (fun _ -> assert_equal ~printer: string_of_int 0 (find_index (1, 2) [(1, 2); (3, 4); (5, 6)] 3 0));
    "find_index_2" >:: (fun _ -> assert_equal ~printer: string_of_int 1 (find_index (3, 4) [(1, 2); (3, 4); (5, 6)] 3 0));
    "find_index_3" >:: (fun _ -> assert_equal ~printer: string_of_int 2 (find_index (5, 6) [(1, 2); (3, 4); (5, 6)] 3 0));

    "medium_1" >:: (fun _ -> assert_equal (Some 1) (medium_ai_next_move sample_state_1));
    "medium_2" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
    (Some 4) (medium_ai_next_move sample_state_2));
    "medium_3" >:: (fun _ -> assert_equal (Some 6) (medium_ai_next_move sample_state_3));
    "medium_4" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
    (Some 10) (medium_ai_next_move sample_state_4));
    "medium_5" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
    (Some 14) (medium_ai_next_move sample_state_5));
    "medium_6" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
    (Some 16) (medium_ai_next_move sample_state_6));
    "medium_7" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
    (Some 18) (medium_ai_next_move sample_state_7));
    "medium_8" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
    (Some 22) (medium_ai_next_move sample_state_8));
    "medium_9" >:: (fun _ -> assert_equal (None) (medium_ai_next_move sample_state_none));

    "hard_1" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
        (Some 1) (hard_ai_next_move sample_state_1));
    "hard_2" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
        (Some 3) (hard_ai_next_move sample_state_2));
    "hard_3" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
        (Some 7) (hard_ai_next_move sample_state_3));
    "hard_4" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
        (Some 10) (hard_ai_next_move sample_state_4));
    "hard_5" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
        (None) (hard_ai_next_move sample_state_none));
    "hard_6" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
        (Some 3) (hard_ai_next_move sample_state_9));
    "hard_7_xhd" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
        (Some 8) (hard_ai_next_move sample_state_1'));
    "hard_8_lt3" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
        (Some 1) (hard_ai_next_move sample_state_lt3));
    "hard_9_lt3" >:: (fun _ -> assert_equal ~printer: (fun x -> match x with |Some x -> string_of_int x |None -> "None")
        (Some 1) (hard_ai_next_move sample_state_lt3));
  ]

let suite =
  "AI test suite"
  >::: tests

let _ = run_test_tt_main suite
