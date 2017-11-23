open OUnit2
open State
open RecruitPile

let j = Yojson.Basic.from_file "recruitpile.json"


(* Tests for value of each field for file recruitpile.json*)
let init_state_test_j =
[
  (* "max" >:: (fun _ -> assert_equal 11111 (j |> init_state |> win_score));
  "score" >:: (fun _ -> assert_equal 10100 (j |> init_state |> score));
  "turns" >:: (fun _ -> assert_equal 0 (j |> init_state |> turns));
  "current room id" >:: (fun _ -> assert_equal "room1" (j |> init_state |> current_room_id));
  "inventory" >:: (fun _ -> assert_equal [] (j |> init_state |> inv));
  "visited" >:: (fun _ -> assert_equal [] (j |> init_state |> visited));
  "locations" >:: (fun _ -> assert_equal [("white hat", "room1"); ("red hat", "room1")]
                      (j |> init_state |> locations)); *)
]

(* tests for do' function for all possible commands *)
let do_test =
  (* [
    (* Sweep case 1: tests do' function for quit *)
    "quit" >:: (fun _ -> assert_equal false (j |> init_state |> do' Quit |> should_cont));
    (* Sweep case 2: tests do' function for look *)
    "look" >:: (fun _ -> assert_equal (j |> init_state) (j |> init_state |> do' Look));
    (* Sweep case 3: tests do' function for turns *)
    "turns" >:: (fun _ -> assert_equal (j |> init_state) (j |> init_state |> do' Turns));
    (* Sweep case 4: tests do' function for inventory *)
    "inventory" >:: (fun _ -> assert_equal (j |> init_state) (j |> init_state |> do' Inventory));
    (* Sweep case 5: tests do' function for inventory *)
    "visited" >:: (fun _ -> assert_equal (j |> init_state) (j |> init_state |> do' Visited));
    (* Sweep case 6: tests do' function for display_all *)
    "display_all" >:: (fun _ -> assert_equal (j |> init_state) (j |> init_state |> do' Display_All));
    (* Sweep case 7: tests do' function for treasure *)
    "treasure" >:: (fun _ -> assert_equal (j |> init_state) (j |> init_state |> do' Treasure));
    (* Sweep case 8: tests do' function for dropping item in treasure room, in inventory *)
    "drop_points_inv" >:: (fun _ -> assert_equal [] (j |> init_state |> do' (Movement "drop white hat") |> inv));
    "drop_points_score" >:: (fun _ -> assert_equal 55200 (j |> init_state |> do' (Movement "drop white hat") |> score));

    (* Sweep case 9: tests do' function for dropping item not in treasure room, in inventory*)
    "drop_no_points_inv" >:: (fun _ -> assert_equal [] (j |> init_state |> do' (Movement "drop white hat") |> inv));
    "drop_no_points_score" >:: (fun _ -> assert_equal 54201 (j |> init_state |> do' (Movement "north") |>
                                    do' (Movement "drop white hat") |> score));

    (* Sweep case 10: tests do' function for dropping item not in inventory*)
    "drop_invalid" >:: (fun _ -> assert_equal 54201 (j |> init_state |> do' (Movement "north") |>
                                    do' (Movement "drop black hat") |> score));

    (* Sweep case 11: tests do' function for taking item in treasure room*)
    "take_treas_inv" >:: (fun _ -> assert_equal 2 (j |> init_state |> do' (Movement "take red hat")
                                               |> inv |> length));
    "take_treas_score" >:: (fun _ -> assert_equal 44200 (j |> init_state |> do' (Movement "take red hat")
                                                         |> score));
    (* Sweep case 12: tests do' function for dropping item not in treasure room*)
    "take_not_treas_inv" >:: (fun _ -> assert_equal 2 (j |> init_state |> do' (Movement "take black hat")
                                               |> inv |> length));
    "take_not_treas_score" >:: (fun _ -> assert_equal 54200 (j |> init_state |> do' (Movement "take black hat")
                                                         |> score));

    (* Sweep case 13: tests do' function for going an invalid direction*)
    "go_invalid" >:: (fun _ -> assert_equal (j |> init_state) (j |> init_state |> do' (Movement "go west")));

    (* Sweep case 14a: tests do' function for going a valid direction - funny letter case*)
    "go_valid_a" >:: (fun _ -> assert_equal 2 (j |> init_state |> do' (Movement "go NoRth")
                                               |> turns));

    (* Sweep case 14b: tests do' function for going a valid direction - Normal letter case *)
    "go_valid_b" >:: (fun _ -> assert_equal 2 (j |> init_state |> do' (Movement "go north")
                                               |> turns));

    (* Sweep case 15: tests do' function for valid Others command: direction
       without 'go', 'start' etc *)
    "others_valid" >:: (fun _ -> assert_equal 2 (j |> init_state |> do' (Movement "south")
                                                 |> turns));

    (* Sweep case 15: tests do' function for invalid Others command *)
    "others_invalid" >:: (fun _ -> assert_equal (j |> init_state) (j |> init_state
                                                                   |> do' (Others "dsjfnds"))); *)
  ]

let suite =
  "Adventure test suite"
  >::: List.flatten [
    init_state_test_j;
    init_state_test_i;
    do_test;
  ]

let _ = run_test_tt_main suite
