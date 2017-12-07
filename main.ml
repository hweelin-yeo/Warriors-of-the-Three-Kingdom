(* main.ml
 *
 * CS 3110 Fall 2017 Final Project
 *
*)


open State
open PlayerState
open Ai

(*********************************************************)
(*     Real Functions                                    *)
(*********************************************************)


let rec conv_int str =
  let l_str = String.lowercase_ascii str in
  if l_str = "1" || l_str = "one" then 1
  else if  l_str = "2" || l_str = "two" then 2
  else if  l_str = "3" || l_str = "three" then 3
  else if  l_str = "4" || l_str = "four" then 4
  else (print_endline "Not a recognizable number! Make sure you are choosing
                      a number between 1 through 4\n> ";
        match read_line () with
        | line ->conv_int line
       )
let rec print_id_list idl =
  match idl with
  | [] -> ();
  | h :: t -> print_string (string_of_int h);
    print_string " ";
    print_id_list t

let rec init_game () =
  print_endline "How many players? (Max 4 Players)"; print_string "\n> ";
  let players =  match read_line () with line -> conv_int line in
  if players < 2 then (print_endline "Nah, you can't do that try again";
                       init_game ()) else
    print_endline "How many humans players"; print_string "\n> ";
  let humans = match read_line () with line -> conv_int line in
  if humans > players then (print_endline "nah"; init_game () ) else
    game (init_state players humans)

and

  game_commands str st =
  if str = "help" then (print_endline "this is the help menu. You can use play \
                                       with these commands. you can also type \
                                       'quit' to quit to the game, 'menu' to \
                                       quit the game and return to the menu. \
                                       'score' to check the score, and 'deck' \
                                       to view your deck.\n" ; game st )

  else if str = "menu" then (print_endline "are you sure you want to go to the \
                                            menu? You will lose all your progre\
                                            ss [Y/N]";
                             match read_line () with line -> if
                               (String.lowercase_ascii line) = "y" ||
                               (String.lowercase_ascii line) = "yes"
                               then menu () else if
                                 (String.lowercase_ascii line) = "n" ||
                                 (String.lowercase_ascii line) = "no"
                               then game st
                               else (print_endline "please answer y \
                                                    or n"; game_commands "quit" st))


  else if str = "quit" then (print_endline "are you sure you want to quit? You \
                                            will lose all your progress [Y/N]";
                             match read_line () with line -> if
                               (String.lowercase_ascii line) = "y" ||
                               (String.lowercase_ascii line) = "yes"
                               then () else if (String.lowercase_ascii
                                                  line) = "n" ||
                                               (String.lowercase_ascii line) = "no"
                               then game st
                               else (print_endline "please answer y \
                                                    or n"; game_commands "quit" st))

  else if str = "describe 1" then (
    let current_cards = id_to_card_lst st st.available_picks in
    match current_cards with
    | h1 :: h2 :: h3 :: t -> print_endline h1.flavor;
      print_endline h1.card_text;
      print_endline "";
      game st;
    | h1 :: h2 :: t -> print_endline h1.flavor;
      print_endline h1.card_text;
      print_endline "";
      game st;
    | h :: t -> print_endline h.flavor;
      print_endline h.card_text;
      print_endline "";
      game st;
    | _ -> game st)

  else if str = "describe 2" then (
    let current_cards = id_to_card_lst st st.available_picks in
    match current_cards with
    | h1 :: h2 :: h3 :: t -> print_endline h2.flavor;
      print_endline h2.card_text;
      print_endline "";
      game st;
    | h1 :: h2 :: t -> print_endline h2.flavor;
      print_endline h2.card_text;
      print_endline "";
      game st;
    | h :: t ->
      game st;
    | _ -> game st)

  else if str = "describe 3" then (
    let current_cards = id_to_card_lst st st.available_picks in
    match current_cards with
    | h1 :: h2 :: h3 :: t -> print_endline h3.flavor;
      print_endline h3.card_text;
      print_endline "";
      game st;
    | h1 :: h2 :: t ->
      game st;
    | h :: t ->
      game st;
    | _ -> game st)

  else if str = "take 1" then (print_endline "You picked option 1";
                               match st.available_picks with
                               | h1 :: t ->
                                 let inc_state = increase_resource st in
                                 let card_name = (lookup_card h1).card_name in
                                 print_string "You picked ";
                                 print_endline card_name;
                                 let orig_player_state = return_player_state inc_state inc_state.current_player in
                                 let orig_player_resource = orig_player_state.player_resource in
                                 let selected_card = lookup_card h1 in
                                 (*if (selected_card.cost > orig_player_resource) then
                                   (print_endline "You do not have the resources necessary to recruit this unit";
                                    game st;)
                                   else*)
                                   let substate_1 = draw_card h1 inc_state in
                                   let current_player_state = return_player_state substate_1 substate_1.current_player in
                                   print_string "New player score = ";
                                   print_endline (string_of_int current_player_state.player_score);
                                   (*Testing line to check the resource that player can get*)
                                   print_endline (string_of_int current_player_state.player_resource);
                                   print_endline "";
                                   print_endline (string_of_int (List.length substate_1.recruit_pool));
                                   (*Implement transition turn function*)
                                   let next_state = change_next_player substate_1 in
                                   game next_state (*Game substate the selected card added to the player's state*)
                               | _ -> print_endline "Option not available";
                                 game st (*Other methods not implemented yet*))

  else if str = "take 2" then (print_endline "You picked option 2";
                               match st.available_picks with
                               | h1 :: h2 :: t ->
                                 let inc_state = increase_resource st in
                                 let card_name = (lookup_card h2).card_name in
                                 print_string "You picked ";
                                 print_endline card_name;
                                 let orig_player_state = return_player_state inc_state inc_state.current_player in
                                 let orig_player_resource = orig_player_state.player_resource in
                                 let selected_card = lookup_card h2 in
                                 if (selected_card.cost > orig_player_resource) then
                                   (print_endline "You do not have the resources necessary to recruit this unit";
                                    game st;)
                                 else
                                   let substate_1 = draw_card h2 inc_state in
                                   let current_player_state = return_player_state substate_1 substate_1.current_player in
                                   print_string "New player score = ";
                                   print_endline (string_of_int current_player_state.player_score);
                                   (*Testing line to check the resource that player can get*)
                                   print_endline (string_of_int current_player_state.player_resource);
                                   print_endline "";
                                   print_endline (string_of_int (List.length substate_1.recruit_pool));
                                   (*Implement transition turn function*)
                                   let next_state = change_next_player substate_1 in
                                   game next_state (*Game substate the selected card added to the player's state*)
                               | _ -> print_endline "Option not available";
                                 game st (*Other methods not implemented yet*))

  else if str = "take 3" then (print_endline "You picked option 2";
                               match st.available_picks with
                               | h1 :: h2 :: h3 :: t ->
                                let inc_state = increase_resource st in
                                 let card_name = (lookup_card h3).card_name in
                                 print_string "You picked ";
                                 print_endline card_name;
                                 let orig_player_state = return_player_state inc_state inc_state.current_player in
                                 let orig_player_resource = orig_player_state.player_resource in
                                 let selected_card = lookup_card h3 in
                                 if (selected_card.cost > orig_player_resource) then
                                   (print_endline "You do not have the resources necessary to recruit this unit";
                                    game st;)
                                 else
                                   let substate_1 = draw_card h3 inc_state in
                                   let current_player_state = return_player_state substate_1 substate_1.current_player in
                                   print_string "New player score = ";
                                   print_endline (string_of_int current_player_state.player_score);
                                   (*Testing line to check the resource that player can get*)
                                   print_endline (string_of_int current_player_state.player_resource);
                                   print_endline "";
                                   print_endline (string_of_int (List.length substate_1.recruit_pool));
                                   (*Implement transition turn function*)
                                   let next_state = change_next_player substate_1 in
                                   game next_state (*Game substate the selected card added to the player's state*)
                               | _ -> print_endline "Option not available";
                                 game st)

  else if str = "skip" then (print_endline "You skipped your turn, gain one extra resource";

                             let new_player_state =  increase_resource((increase_resource st)) in
                             let next_state = change_next_player new_player_state in
                            game next_state)

  else game st


and

  game st =
  (*Print the ids of the current picks*)
  (*let testValue = List.map (fun x -> print_string (string_of_int x ^ " ")) st.available_picks in*)
  if ((List.assoc st.current_player st.player_states).player_is_human = true) then (
  print_endline ("Player " ^ string_of_int st.current_player ^"'s Turn:");
  match (id_to_card_lst st st.available_picks) with
  | h1 :: h2 :: h3::t -> print_string h1.card_name;
    print_string "\n";
    print_string h2.card_name;
    print_string "\n";
    print_string h3.card_name;
    print_string "\n> ";
    let input_string = read_line () in
    game_commands input_string st;
  | h1 :: h2 :: t -> print_string h1.card_name;
    print_string "\n";
    print_string h2.card_name;
  | h :: t -> print_string h.card_name;
  | [] -> print_string "No Picks";
    match read_line () with
    | line -> game_commands (String.trim((String.lowercase_ascii line))) st
) else (
    print_endline ("Player " ^ string_of_int st.current_player ^ " is a computer set at medium difficulty.");
  match (id_to_card_lst st st.available_picks) with
  | h1 :: h2 :: h3::t -> print_string h1.card_name;
    print_string "\n";
    print_string h2.card_name;
    print_string "\n";
    print_string h3.card_name;
    print_string "\n> ";
    (match medium_ai_next_move st with
     | None -> let input_string = "skip" in
       print_endline "The computer decided to skip";
       print_endline "Press any button to continue";
       begin match read_line () with
         | line -> game_commands input_string st;
       end
     | Some x -> let input_string = "take " ^ string_of_int x in
       print_endline ("The computer's choice was " ^ input_string);
       print_endline "Press any button to continue";
       begin match read_line () with
         | line -> game_commands input_string st;
       end
    )
  | h1 :: h2 :: t -> print_string h1.card_name;
    print_string "\n";
    print_string h2.card_name;
  | h :: t -> print_string h.card_name;
  | [] -> print_string "No Picks";
    (* change this easy, medium, and hard depending on the setting of the AI *)
    match medium_ai_next_move st with
    | Some x -> print_string ("The computer chose " ^ string_of_int x);
      game_commands ("take" ^ string_of_int x) st
    | None -> print_string "The computer decided to skip."
)

and

  menu () =
  print_endline "Menu: Type in your choice fam";
  print_string "\n> ";
  let str = (match read_line () with line -> String.lowercase_ascii line) in

  if str = "play" then init_game ()
  else if str = "tutorial" then init_tutorial ()
  else if str = "credits" then init_credits ()
  else if str = "quit" then ()
  else (
    print_endline "I don't understand that command, try typing 'play',
                    'tutorial', 'credits'";
    menu ()
  )

and

  init_tutorial () =
  print_endline "This is the tutorial";
  menu ()

and

  init_credits () =
  print_endline "This is the credits";
  print_endline "AI Design: Akira Shindo";
  print_endline "Card Design: Kevin Gao";
  print_endline "Repl Design: Yang Lu";
  print_endline "Special Thanks to: Hweelin Yeo";
  menu ()


let main () =
  print_string "\nWelcome to Explosion of Explosions!.\n";
  print_endline "Created by CS3110 team"; menu ()

let () = main ()
