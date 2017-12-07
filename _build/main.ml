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
        if str = "yes" || str = "y" then (ANSITerminal.(print_string [red] "See you soon!\n\n"); () )
        else (print_string "\n\n"; menu () ) )
  else (
       ANSITerminal.(print_string [red] "\nCommnad not understandable. Try \
                      typing 'Play', 'Tutorial', 'Credits', or 'Quit.'\n\n";
                      menu () ))
and

init_tutorial () =
  ANSITerminal.(print_string [red] "
    In the final years of the Eastern Han Dynesty ....





    You are the commander of an army! Build your army and lead your clan to \
    victory! The first command you should be familiar with is 'help.' Go on! \
    Type it and see what happens:\n");

    let rec help_tut () =
      match read_line () with
        | line -> if String.trim (String.lowercase_ascii line) <> "help"
                  then (ANSITerminal.(print_string [red] "That's not right, \
                        type in 'help'! Try again\n"); help_tut () )
                  else (ANSITerminal.(print_string [red]
                                      "this is the help menu. You can use play \
                                       with these commands. you can also type \
                                       'quit' to quit to the game, 'menu' to \
                                       quit the game and return to the menu. \
                                       'score' to check the score, and 'deck' \
                                       to view your deck.\n"))
    in help_tut ();
    print_string "\n\n";
    ANSITerminal.(print_string [red] "Congrats! You did it! That's right. You \
          learned two things here. First that's exactly how you communicate \
          with the
      ")



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
                                    Akira Shindo, Kevin Gao, Yang Lu, Hwee Lin Yeo
                                                      © 2017
***************************************************************************************************************
\n");
  ANSITerminal.(print_string [white; on_red]
"Press Enter to Begin                                                                                          ");

  print_string "\n";
  match read_line () with _ -> menu ()

let () = main ()
