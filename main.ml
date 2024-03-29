(* main.ml
 *
 * CS 3110 Fall 2017 Final Project
 *
*)


open State
open PlayerState
open Ai

let rec conv_int str =
  let l_str = String.lowercase_ascii str in
  if l_str = "1" || l_str = "one" then 1
  else if  l_str = "2" || l_str = "two" then 2
  else if  l_str = "3" || l_str = "three" then 3
  else if  l_str = "4" || l_str = "four" then 4
  else if l_str = "0" || l_str = "zero" then 0
  else (ANSITerminal.(print_string [red] "\nNot a recognizable number! Make sure you are choosing \
                                          a number between 0 through 4\n> ");
        match read_line () with
        | line ->conv_int line
       )
let rec print_id_list idl =
  match idl with
  | [] -> ();
  | h :: t -> print_string (string_of_int h);
    print_string " ";
    print_id_list t

let rec print_player_and_score ps =
  match ps with
  | [] -> ()
  | h :: t -> print_endline ("Player id: " ^ (string_of_int (fst h)));
    let current_player = snd h in
    print_endline ("Player score: " ^ (string_of_int current_player.player_score));
    print_player_and_score t

let rec print_card_list cs =
  match cs with
  | [] -> ()
  | h :: t -> print_endline h.card_name;
    print_card_list t

let rec find_greatest ps grt =
  match ps with
  | [] -> grt
  | h :: t -> let target_player = snd h in
    let curr_score = target_player.player_score in
    if curr_score > snd grt then
      find_greatest t (target_player.player_id ,curr_score) else
      find_greatest t grt

let rec print_leaderboard ps =
  match ps with
  | [] -> ()
  | h :: t -> print_endline ("Player : " ^ (string_of_int (snd h).player_id) ^ " " ^ "Score: " ^ (string_of_int (snd h).player_score));
    print_leaderboard t


let rec end_game st =
  print_endline "";
  print_endline "
 ▄▄▄▄▄▄▄▄▄▄▄ ▄▄▄▄▄▄▄▄▄▄▄ ▄▄       ▄▄ ▄▄▄▄▄▄▄▄▄▄▄       ▄▄▄▄▄▄▄▄▄▄▄ ▄               ▄ ▄▄▄▄▄▄▄▄▄▄▄ ▄▄▄▄▄▄▄▄▄▄▄ ▄
▐░░░░░░░░░░░▐░░░░░░░░░░░▐░░▌     ▐░░▐░░░░░░░░░░░▌     ▐░░░░░░░░░░░▐░▌             ▐░▐░░░░░░░░░░░▐░░░░░░░░░░░▐░▌
▐░█▀▀▀▀▀▀▀▀▀▐░█▀▀▀▀▀▀▀█░▐░▌░▌   ▐░▐░▐░█▀▀▀▀▀▀▀▀▀      ▐░█▀▀▀▀▀▀▀█░▌▐░▌           ▐░▌▐░█▀▀▀▀▀▀▀▀▀▐░█▀▀▀▀▀▀▀█░▐░▌
▐░▌         ▐░▌       ▐░▐░▌▐░▌ ▐░▌▐░▐░▌               ▐░▌       ▐░▌ ▐░▌         ▐░▌ ▐░▌         ▐░▌       ▐░▐░▌
▐░▌ ▄▄▄▄▄▄▄▄▐░█▄▄▄▄▄▄▄█░▐░▌ ▐░▐░▌ ▐░▐░█▄▄▄▄▄▄▄▄▄      ▐░▌       ▐░▌  ▐░▌       ▐░▌  ▐░█▄▄▄▄▄▄▄▄▄▐░█▄▄▄▄▄▄▄█░▐░▌
▐░▌▐░░░░░░░░▐░░░░░░░░░░░▐░▌  ▐░▌  ▐░▐░░░░░░░░░░░▌     ▐░▌       ▐░▌   ▐░▌     ▐░▌   ▐░░░░░░░░░░░▐░░░░░░░░░░░▐░▌
▐░▌ ▀▀▀▀▀▀█░▐░█▀▀▀▀▀▀▀█░▐░▌   ▀   ▐░▐░█▀▀▀▀▀▀▀▀▀      ▐░▌       ▐░▌    ▐░▌   ▐░▌    ▐░█▀▀▀▀▀▀▀▀▀▐░█▀▀▀▀█░█▀▀▐░▌
▐░▌       ▐░▐░▌       ▐░▐░▌       ▐░▐░▌               ▐░▌       ▐░▌     ▐░▌ ▐░▌     ▐░▌         ▐░▌     ▐░▌  ▀
▐░█▄▄▄▄▄▄▄█░▐░▌       ▐░▐░▌       ▐░▐░█▄▄▄▄▄▄▄▄▄      ▐░█▄▄▄▄▄▄▄█░▌      ▐░▐░▌      ▐░█▄▄▄▄▄▄▄▄▄▐░▌      ▐░▌ ▄
▐░░░░░░░░░░░▐░▌       ▐░▐░▌       ▐░▐░░░░░░░░░░░▌     ▐░░░░░░░░░░░▌       ▐░▌       ▐░░░░░░░░░░░▐░▌       ▐░▐░▌
 ▀▀▀▀▀▀▀▀▀▀▀ ▀         ▀ ▀         ▀ ▀▀▀▀▀▀▀▀▀▀▀       ▀▀▀▀▀▀▀▀▀▀▀         ▀         ▀▀▀▀▀▀▀▀▀▀▀ ▀         ▀
";
  let winner_tuple = find_greatest (st.player_states) (0, 0) in
  print_endline ("Winner is : Player " ^ (string_of_int (fst winner_tuple)));
  print_endline ("Score is : " ^ (string_of_int (snd winner_tuple)));
  print_endline ("Leaderboard: ");
  print_leaderboard st.player_states;
  ANSITerminal.(set_autoreset true; print_string [white; on_red]
                  "\n\n Thanks for playing! Press any key to return to the menu");
  match read_line () with line -> menu ()

and

  init_game () =
  ANSITerminal.(print_string [red]
                  "\n\nHow many players in this Game? (Max 4 Players)\n>"; set_autoreset false);
  let players =  match read_line () with line -> conv_int line in
  if players < 2 then (ANSITerminal.(print_string [red]
                                       "\nYou can't have less than 2 players!";
                                     init_game ())) else
    ANSITerminal.(print_string [red] "How many humans players\n>");
  let humans = match read_line () with line -> conv_int line in
  if humans > players then (ANSITerminal.(print_string [red] "You can't have more humans players than total players! Try again.");
                            init_game () ) else
    game (init_state players humans)

and

  game_commands str st =
  if str = "help" then (print_endline "\nThis is the help menu. You can use play \
                                       by typing \"take 1\" to take option 1, \"take 2\" \
                                       to take option 2 \"take 3\" to take \
                                       option 3, \"skip\" to skip. You can find out more \
                                       about the cards by typing describe followed by the \
                                       card number (ie describe 1). You can look at other player's decks \
                                       by typing decsribe followed by the player name to see their decks \
                                       (ie describe player 1). You can also type just describe to see the \
                                       state of the game. Type quit to return to the menu.\n" ; game st )

  else if str = "quit" || str = "menu" then (print_endline "Are You sure You want to go to the \
                                            menu? You will lose all your progre\
                                            ss [Y/N]";
                             match read_line () with line -> if
                               (String.lowercase_ascii line) = "y" ||
                               (String.lowercase_ascii line) = "yes"
                               then (ANSITerminal.(set_autoreset true); menu () )

                               else if
                                 (String.lowercase_ascii line) = "n" ||
                                 (String.lowercase_ascii line) = "no"
                               then game st
                               else (print_endline "please answer y \
                                                    or n"; game_commands "quit" st))



  else if str = "describe 1" then (
    let current_cards = id_to_card_lst st st.available_picks in
    match current_cards with
    | h :: t ->
      print_string (
"_________________________________________________________________________________________________________________"
        ^ "\nDescription:\n" ^ h.flavor ^ "\n\nAbilities:\n" ^ h.card_text
                    ^ "\n\nCost:\n" ^ (string_of_int h.cost) ^ "\n\nPower:\n"
                    ^ (string_of_int h.power) ^ h.ascii_art ^ "\n" ^
"_________________________________________________________________________________________________________________\n"
                  );game st;


    | _ -> game st )

  else if str = "describe 2" then (
    let current_cards = id_to_card_lst st st.available_picks in
    match current_cards with
    | h1 :: h2 :: t -> print_string (
"_________________________________________________________________________________________________________________"
        ^ "\nDescription:\n" ^ h2.flavor ^ "\n\nAbilities:\n" ^ h2.card_text
                    ^ "\n\nCost:\n" ^ (string_of_int h2.cost) ^ "\n\nPower:\n"
                    ^ (string_of_int h2.power) ^ h2.ascii_art ^ "\n" ^
"_________________________________________________________________________________________________________________\n");
      game st;
    | h :: t ->
      print_endline "Not a valid choice in this state";
      print_endline "";
      game st;
    | _ -> game st)

  else if str = "describe 3" then (
    let current_cards = id_to_card_lst st st.available_picks in
    match current_cards with
    | h1 :: h2 :: h3 :: t -> print_string (
"_________________________________________________________________________________________________________________"
        ^ "\nDescription:\n" ^ h3.flavor ^ "\n\nAbilities:\n" ^ h3.card_text
                    ^ "\n\nCost:\n" ^ (string_of_int h3.cost) ^ "\n\nPower:\n"
                    ^ (string_of_int h3.power) ^ h3.ascii_art ^ "\n" ^
"_________________________________________________________________________________________________________________\n");
      game st;
    | h1 :: h2 :: t ->
      print_endline ("Not a valid choice in this state");
      game st;
    | h :: t ->
      print_endline ("Not a valid choice in this state");
      game st;
    | _ -> game st)

  else if str = "describe state" || str = "describe" || str = "describe game" then (
    print_endline "\nState of the Game\n_______________________________________\n";
    print_endline ("Number of Players : " ^
                   (string_of_int (List.length (st.player_states))));
    print_endline ("Cards Remaining : " ^
                   (string_of_int (List.length (st.recruit_pool))));
    print_endline ("Player_Scores: ");
    print_player_and_score st.player_states;
    print_endline "";
    game st
  )

  else if str = "describe player 1" then (
    print_endline "\nPlayer 1\n_______________________________________\n";
    let player_1_state = return_player_state st 1 in
    print_endline ("Player 1 Score: " ^ (string_of_int player_1_state.player_score));
    let player_1_deck = player_1_state.player_deck in
    let player_1_cards = id_to_card_lst st player_1_deck in
    print_endline "Player 1 Cards: ";
    if (List.length player_1_cards = 0)
    then print_endline "No Cards"
    else
      print_card_list player_1_cards;
    print_endline "";

  )
  else if str = "describe player 2" then (
    print_endline "\nPlayer 2\n_______________________________________\n";
    let player_2_state = return_player_state st 2 in
    print_endline ("Player 2 score: " ^ (string_of_int player_2_state.player_score));
    let player_2_deck = player_2_state.player_deck in
    let player_2_cards = id_to_card_lst st player_2_deck in
    print_endline "Player 2 cards: ";
    if (List.length player_2_cards = 0)
    then print_endline "No Cards"
    else
      print_card_list player_2_cards;
    print_endline "";

  )
  else if str = "describe player 3" then (
    if(st.total_players > 2) then(
      print_endline "\nPlayer 3\n_______________________________________\n";
      let player_3_state = return_player_state st 3 in
      print_endline ("Player 3 score: " ^ (string_of_int player_3_state.player_score));
      let player_3_deck = player_3_state.player_deck in
      let player_3_cards = id_to_card_lst st player_3_deck in
      print_endline "Player 3 cards: ";
      if (List.length player_3_cards = 0)
      then print_endline "No Cards"
      else
        print_card_list player_3_cards;
      print_endline "";
    )
    else
      print_endline "";
    print_endline "Player is not valid";
    print_endline "";
    game st
  )

  else if str = "describe player 4" then (
    if(st.total_players > 3) then(
      print_endline "\nPlayer 1\n_______________________________________\n";
      let player_4_state = return_player_state st 4 in
      print_endline ("Player 4 score: " ^ (string_of_int player_4_state.player_score));
      let player_4_deck = player_4_state.player_deck in
      let player_4_cards = id_to_card_lst st player_4_deck in
      print_endline "Player 4 cards: ";
      if (List.length player_4_cards = 0)
      then print_endline "No Cards"
      else
        print_card_list player_4_cards;
      print_endline "";
    )
    else
      print_endline "";
    print_endline "Player is not valid";
    print_endline "";
    game st
  )

  else if str = "take 1" then (print_endline "\nPlayer picked Option 1";
                               match st.available_picks with
                               | h1 :: t ->
                                 let inc_state = increase_resource st in
                                 let card_name = (lookup_card h1).card_name in
                                 print_string "Player picked ";
                                 print_endline card_name;
                                 let orig_player_state = return_player_state inc_state inc_state.current_player in
                                 let orig_player_resource = orig_player_state.player_resource in
                                 let selected_card = lookup_card h1 in
                                 if (selected_card.cost > orig_player_resource) then
                                   (print_endline "Player does not have the resources necessary to recruit this unit";
                                    game st;)
                                   else
                                 let substate_1 = draw_card h1 inc_state in
                                 let current_player_state = return_player_state substate_1 substate_1.current_player in
                                 print_string "New Player Score = ";
                                 print_endline (string_of_int current_player_state.player_score);
                                 (*Implement transition turn function*)
                                 let next_state = change_next_player substate_1 in
                                 game next_state (*Game substate the selected card added to the player's state*)
                               | _ -> print_endline "Option not available";
                                 game st (*Other methods not implemented yet*))

  else if str = "take 2" then (print_endline "\nPlayer picked Option 2";
                               match st.available_picks with
                               | h1 :: h2 :: t ->
                                 let inc_state = increase_resource st in
                                 let card_name = (lookup_card h2).card_name in
                                 print_string "Player picked ";
                                 print_endline card_name;
                                 let orig_player_state = return_player_state inc_state inc_state.current_player in
                                 let orig_player_resource = orig_player_state.player_resource in
                                 let selected_card = lookup_card h2 in
                                 if (selected_card.cost > orig_player_resource) then
                                   (print_endline "Player does not have the resources necessary to recruit this unit";
                                    game st;)
                                 else
                                   let substate_1 = draw_card h2 inc_state in
                                   let current_player_state = return_player_state substate_1 substate_1.current_player in
                                   print_string "New Player Score = ";
                                   print_endline (string_of_int current_player_state.player_score);
                                   (*Implement transition turn function*)
                                   let next_state = change_next_player substate_1 in
                                   game next_state (*Game substate the selected card added to the player's state*)
                               | _ -> print_endline "Option not available";
                                 game st (*Other methods not implemented yet*))

  else if str = "take 3" then (print_endline "\nPlayer picked Option 2";
                               match st.available_picks with
                               | h1 :: h2 :: h3 :: t ->
                                 let inc_state = increase_resource st in
                                 let card_name = (lookup_card h3).card_name in
                                 print_string "Player picked ";
                                 print_endline card_name;
                                 let orig_player_state = return_player_state inc_state inc_state.current_player in
                                 let orig_player_resource = orig_player_state.player_resource in
                                 let selected_card = lookup_card h3 in
                                 if (selected_card.cost > orig_player_resource) then
                                   (print_endline "Player does not have the resources necessary to recruit this unit";
                                    game st;)
                                 else
                                   let substate_1 = draw_card h3 inc_state in
                                   let current_player_state = return_player_state substate_1 substate_1.current_player in
                                   print_string "New player score = ";
                                   print_endline (string_of_int current_player_state.player_score);
                                   (*Implement transition turn function*)
                                   let next_state = change_next_player substate_1 in
                                   game next_state (*Game substate the selected card added to the player's state*)
                               | _ -> print_endline "Option not available";
                                 game st)

  else if str = "skip" then (print_endline "Player skipped turn, gain one extra resource";

                             let new_player_state =  increase_resource((increase_resource st)) in
                             let next_state = change_next_player new_player_state in
                             game next_state)

  else print_endline "not a valid command. type \"take 1\" to take option 1, \n \"take 2\" to take option 2 \"take 3\" to take option 3, \n \"skip\" to skip or \"help\" for help ";
  game st


and

  game st =
  (*Print the ids of the current picks*)
  (*let testValue = List.map (fun x -> print_string (string_of_int x ^ " ")) st.available_picks in*)
  if ((List.assoc st.current_player st.player_states).player_is_human = true) then (
    let current_player_state = (List.assoc st.current_player st.player_states) in
    let cards_remaining = List.length st.recruit_pool in
    print_endline (
      "\nPlayer " ^ string_of_int st.current_player ^"'s Turn:" ^
"                                              Score: " ^ string_of_int current_player_state.player_score ^
                                                  "     Resources: " ^ (string_of_int current_player_state.player_resource)
                                                                            ^ "     Cards Remaining: " ^ (string_of_int cards_remaining) ^
 "
***************************************************************************************************************
    ");
    match (id_to_card_lst st st.available_picks) with
    | h1 :: h2 :: h3 :: t -> print_string ("[1] " ^ h1.card_name);
      print_string "\n\n";
      print_string ("[2] " ^ h2.card_name);
      print_string "\n\n";
      print_string ("[3] " ^ h3.card_name);
      print_string "\n\n> ";
      let input_string = (String.trim (String.lowercase_ascii (read_line ()))) in
      game_commands input_string st
    | h1 :: h2 :: t -> print_string ("\n [1] " ^ h1.card_name);
      print_string "\n\n";
      print_string ("\n [2] " ^ h2.card_name);
      print_endline "\n\n";
      let input_string = (String.trim (String.lowercase_ascii (read_line ()))) in
      game_commands input_string st
    | h :: t ->
      print_endline ("\n [1]" ^ h.card_name);
      print_endline "\n\n";
      let input_string = (String.trim (String.lowercase_ascii (read_line ()))) in
      game_commands input_string st
    | [] -> print_string "No Picks";
      end_game st;


      match read_line () with
      | line -> game_commands (String.trim((String.lowercase_ascii line))) st
  ) else (
    print_endline ("Player " ^ string_of_int st.current_player ^ " is a computer set at medium difficulty.");
    match (id_to_card_lst st st.available_picks) with
    | h1 :: h2 :: h3 :: t -> print_string h1.card_name;
      print_string "\n";
      print_string h2.card_name;
      print_string "\n";
      print_string h3.card_name;
      print_string "\n ";
      (match medium_ai_next_move st with
       | None -> let input_string = "skip" in
         print_endline "The computer decided to skip";
         print_endline "Press any button to continue";
         begin match read_line () with
           | line -> game_commands input_string st
         end
       | Some x -> if (h1.card_id = x) then
           let input_string = "take " ^ string_of_int 1 in
           print_endline ("The computer's choice was " ^ input_string);
           print_endline "Press any button to continue";
           begin match read_line () with
             | line -> game_commands input_string st
           end
         else if (h2.card_id = x) then
           let input_string = "take " ^ string_of_int 2 in
           print_endline ("The computer's choice was " ^ input_string);
           print_endline "Press any button to continue";
           begin match read_line () with
             | line -> game_commands input_string st
           end
         else
           let input_string = "take " ^ string_of_int 3 in
           print_endline ("The computer's choice was " ^ input_string);
           print_endline "Press any button to continue";
           begin match read_line () with
             | line -> game_commands input_string st
           end

      )
    | h1 :: h2 :: t ->
      print_endline "";
      print_string h1.card_name;
      print_string "\n";
      print_string h2.card_name;
      print_endline "";
      (match medium_ai_next_move st with
       | None -> let input_string = "skip" in
         print_endline "The computer decided to skip";
         print_endline "Press any button to continue";
         begin match read_line () with
           | line -> game_commands input_string st
         end
       | Some x -> if (h1.card_id = x) then
           let input_string = "take " ^ string_of_int 1 in
           print_endline ("The computer's choice was " ^ input_string);
           print_endline "Press any button to continue";
           begin match read_line () with
             | line -> game_commands input_string st
           end
         else
           let input_string = "take " ^ string_of_int 2 in
           print_endline ("The computer's choice was " ^ input_string);
           print_endline "Press any button to continue";
           begin match read_line () with
             | line -> game_commands input_string st
           end
      )
    | h :: t ->
      print_endline "";
      print_string h.card_name;
      let input_string = "take " ^ string_of_int 1 in
      print_endline ("The computer's choice was " ^ input_string);
      print_endline "Press any button to continue";
      begin match read_line () with
        | line -> game_commands input_string st
      end

    | [] -> print_string "No Picks";
      end_game st;
      print_endline "";
      match read_line () with
      | line -> game_commands (String.trim((String.lowercase_ascii line))) st
  )

and


  menu () =
  ANSITerminal.(print_string [red] "
Main Menu
**************************************************************************************************************
  - Play
  - Tutorial
  - Credits
  - Quit \n
"); ANSITerminal.(print_string [white; on_red]
                    "Type the Comnmand and Press Enter                                                                            ");
  print_string "\n";

  let str = (match read_line () with line -> String.lowercase_ascii line) in
  if str = "play" then init_game ()
  else if str = "tutorial" then init_tutorial ()
  else if str = "credits" then init_credits ()
  else if str = "quit" then (
    ANSITerminal.(print_string [red]
                    "\nAre you sure you want to quit? [Y/N] ");
    let str = (match read_line () with line -> String.lowercase_ascii line) in
    if str = "yes" || str = "y" then (ANSITerminal.(print_string [red] "See you soon!\n\n"); exit 0 )
    else (print_string "\n\n"; menu () ) )
  else (
    ANSITerminal.(print_string [red] "\nCommnad not understandable. Try \
                                      typing 'Play', 'Tutorial', 'Credits', or 'Quit.'\n\n";
                  menu () ))
and

  init_tutorial () =
  ANSITerminal.(print_string [red] "
    The year is 180. The Han Empire is in chaos. The emperor has been overthrown, \
    and power-hungry warlords have taken to fighting over what remains of China. \
    You are one such warlord, looking to recruit the heroes of the age to your army, \
    and conquer China. Will you ally yourself with the noble and benevolent Shu Clan, \
    the cunning and ruthless Cao clan of Wei, the wise sages of Wu, or even ally yourself \
    with the strange heroes who have been appearing in China through interdimensional rifts?
    Only the future will tell, in Warriors of the Three Kingdoms.


    You are the a warlord! Build your army and lead your clan to \
    victory! The first command you should be familiar with is 'help.' Go on! \
    Type it and see what happens:\n");

  let rec help_tut () =
    match read_line () with
    | line -> if String.trim (String.lowercase_ascii line) <> "help"
      then (ANSITerminal.(print_string [red] "That's not right, \
                                              type in 'help'! Try again\n"); help_tut () )
      else (ANSITerminal.(print_string [red]
                            "\nThis is the help menu. You can use play \
                                       by typing \"take 1\" to take option 1, \"take 2\" \
                                       to take option 2 \"take 3\" to take \
                                       option 3, \"skip\" to skip. You can find out more \
                                       about the cards by typing describe followed by the \
                                       card number (ie describe 1). You can look at other player's decks \
                                       by typing decsribe followed by the player name to see their decks \
                                       (ie describe player 1). You can also type just describe to see the \
                                       state of the game. Type quit to return to the menu.\n"))
  in help_tut ();
  print_string "\n\n";
  ANSITerminal.(print_string [red] "Congrats! You did it! That's right. You \
                                    learned two things here. First that's exactly how you communicate \
                                    with the game. You type the command that you want to rely to the game \
                                    and then game interprets it. Secondly, if you ever forget what commands \
                                    you have at your disposal, just type 'help' and you will get a comprehensive \
                                    list of what you can do.


Now that you have a list of all the possible commands, let's look at \
                                    what the game looks like.");

  print_string "\n";
  ANSITerminal.(print_string [red] "\n
Player 1's Turn:                                              Score: 0     Resources: 0     Cards Remaining: 24
***************************************************************************************************************

[1] Wu Scout

[2] Lu Zuishen of the Drunken Fist

[3] Xiahou Dun, The One-Eyed


This is what the game looks like. In the top right we have our score and our resources and how many cards remain. \
The game ends when there are no cards left and the winner is decided by whoever has the highest score. Your score is \
determined by the cards in your deck. Resources is the count of the all the resources you have generated throughout the game. \
Each turn, your resources go up by 1. This is important because each card takes a certain amount of resources to choose.

Each card has history, value, and sometimes even special effects. Here try typing describe 1 to see a description  of \
the Wu Scout.\n");

let rec describe_tut () =
    match read_line () with
    | line -> if String.trim (String.lowercase_ascii line) <> "describe 1"
      then (ANSITerminal.(print_string [red] "That's not right, \
                                              type in 'describe 1'! Try again\n"); describe_tut () )
in describe_tut ();

ANSITerminal.(print_string [red]
"_________________________________________________________________________________________________________________
Description:
Wu scouts were experts of the forest, for the fought for their homeland

Abilities:
When you draft Wu Recruits, increase your maximum resources by 1

Cost:
1

Power:
1



          ||                          ||
          ||                          ||
          ||                          ||
          ||                          ||
          ||                          ||
          ||           ____           ||
          ||         .''''''.         ||
          ||        /   __   \\        ||
          ||\\__..-':   /\\/\\   :'-..__/||
          ||=__ =|='  |-()-|  '=|= __=||
          ||/  ''-.:   \\/\\/   :.-''  ||
          ||        \\   ''   /        ||
          ||         `.____.'         ||
          ||                          ||
          ||                          ||
          ||                          ||
          ||                          ||
          ||                          ||
          ||           tre            ||

_________________________________________________________________________________________________________________


Nice job! From the describe command, we see the special abilities, the cost, and the power of the Wu Card. Power \
is added to your score, cost trains your mana, and the ability affects what happens in your game. Now back to the
game.

Player 1's Turn:                                              Score: 0     Resources: 0     Cards Remaining: 24
***************************************************************************************************************

[1] Wu Scout

[2] Lu Zuishen of the Drunken Fist

[3] Xiahou Dun, The One-Eyed


Now we know what Wu Scout does. Add it to your deck using the take command! Press 'take 1'.

");
let rec take_tut () =
  match read_line () with
    | line -> if String.trim (String.lowercase_ascii line) <> "take 1"
      then (ANSITerminal.(print_string [red] "That's not right, \
                                              type in 'take 1'! Try again\n"); take_tut () )
  in take_tut ();

ANSITerminal.(print_string [red]
"\n\nPlayer picked Option 1
Player picked Wu Scout
New Player Score = 1

Nice job! Your score was updated now you have a Wu Scout in your deck.


You now know all the things you need to play this game! There are other commands, \
like describe player which will give you a player's score and their deck, and also \
describe game which will give you the overall state of the game. Remember to have fun \
playing this game and truly become a Warrior of the The Three Kingdoms.\n

Press Enter to Return to the Menu\n\n\n
");  match read_line () with _ -> menu ()


and

  init_credits () =
    ANSITerminal.(erase Screen);
    ANSITerminal.(print_string [red] "
   ____              _ _ _
  / ___|_ __ ___  __| (_) |_ ___
 | |   | '__/ _ \\/ _` | | __/ __|
 | |___| | |  __/ (_| | | |_\\__ \\
  \\____|_|  \\___|\\__,_|_|\\__|___/


Artificial Intelligence Design          Akira Shindo

Card Design                             Kevin Gao

Game Design                             Hwee Lin Yeo

UX/UI REPL Design                       Yang Lu


ASCII WordArt generated from http://patorjk.com/software/taag/
ASCII Art from http://ascii.co.uk/art/

Based on 'Romance of the Three Kingdom' by Luo Guanzhong

Game Idea by Kevin Gao\n\n");
  ANSITerminal.(print_string [white; on_red]
                  "Press Enter to Return to the Main Menu                                                                        ");
  print_string "\n";
  match read_line () with _ -> menu ()

let main () =
  ANSITerminal.(resize 150 50; erase Screen; resize 115 50);
  ANSITerminal.(print_string [red] "
                                                                                    ,dM
                                                                                    dMMP
                                                                                   dMMM'
                                                                                   \\MM/
                                                                                  dMMm.
██╗    ██╗ █████╗ ██████╗ ██████╗ ██╗ ██████╗ ██████╗ ███████╗                   dMMP'_\\---.
██║    ██║██╔══██╗██╔══██╗██╔══██╗██║██╔═══██╗██╔══██╗██╔════╝                  _| _  p ;88;`.
██║ █╗ ██║███████║██████╔╝██████╔╝██║██║   ██║██████╔╝███████╗                ,db; p >  ;8P|  `.
██║███╗██║██╔══██║██╔══██╗██╔══██╗██║██║   ██║██╔══██╗╚════██║               (``T8b,__,'dP |   |
╚███╔███╔╝██║  ██║██║  ██║██║  ██║██║╚██████╔╝██║  ██║███████║               |   `Y8b..dP  ;_  |
 ╚══╝╚══╝ ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝               |    |`T88P_ /  `\\;
                                                                             :_.-~|d8P'`Y/    /
 ██████╗ ███████╗    ████████╗██╗  ██╗███████╗                                \\_   TP    ;   7`\\
██╔═══██╗██╔════╝    ╚══██╔══╝██║  ██║██╔════╝                      ,,__        >   `._  /'  /   `\\_
██║   ██║█████╗         ██║   ███████║█████╗                        `._ ````~~~~------|`\\;' ;     ,'
██║   ██║██╔══╝         ██║   ██╔══██║██╔══╝                           ```~~~-----~~~'\\__[|;' _.-'  `\\
╚██████╔╝██║            ██║   ██║  ██║███████╗                                 ;--..._     .-'-._     ;
 ╚═════╝ ╚═╝            ╚═╝   ╚═╝  ╚═╝╚══════╝                                /      /`~~`'   ,'`\\_ ,/
                                                                             ;_    /'        /    ,/
████████╗██╗  ██╗██████╗ ███████╗███████╗    ██╗  ██╗██╗███╗   ██╗ ██████╗ ██████╗  ██████╗ ███╗   ███╗███████╗
╚══██╔══╝██║  ██║██╔══██╗██╔════╝██╔════╝    ██║ ██╔╝██║████╗  ██║██╔════╝ ██╔══██╗██╔═══██╗████╗ ████║██╔════╝
   ██║   ███████║██████╔╝█████╗  █████╗      █████╔╝ ██║██╔██╗ ██║██║  ███╗██║  ██║██║   ██║██╔████╔██║███████╗
   ██║   ██╔══██║██╔══██╗██╔══╝  ██╔══╝      ██╔═██╗ ██║██║╚██╗██║██║   ██║██║  ██║██║   ██║██║╚██╔╝██║╚════██║
   ██║   ██║  ██║██║  ██║███████╗███████╗    ██║  ██╗██║██║ ╚████║╚██████╔╝██████╔╝╚██████╔╝██║ ╚═╝ ██║███████║
   ╚═╝   ╚═╝  ╚═╝╚═╝  ╚═╝╚══════╝╚══════╝    ╚═╝  ╚═╝╚═╝╚═╝  ╚═══╝ ╚═════╝ ╚═════╝  ╚═════╝ ╚═╝     ╚═╝╚══════╝
");
  ANSITerminal.(print_string [red] "
***************************************************************************************************************
                                                    Created by
                                    Akira Shindo, Hwee Lin Yeo, Kevin Gao, Yang Lu
                                                      © 2017
***************************************************************************************************************
\n");
  ANSITerminal.(print_string [white; on_red]
                  "Press Enter to Begin                                                                                          ");

  print_string "\n";
  match read_line () with _ -> menu ()

let () = main()
