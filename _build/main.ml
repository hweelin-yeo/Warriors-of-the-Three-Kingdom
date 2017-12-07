(* main.ml
 *
 * CS 3110 Fall 2017 Final Project
 *
 *)


open State
open PlayerState

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
                               | h1 :: h2 :: h3 :: t ->
                                 let card_name = (find_card h1).card_name in
                                 print_string "You picked ";
                                 print_endline card_name;
                                 let substate_1 = draw_card h1 st in
                                 let current_player_state = return_player_state substate_1 substate_1.current_player in
                                 print_string "New player score = ";
                                 print_endline (string_of_int current_player_state.player_score);
                                 print_endline "";
                                 (*Implement transition turn function*)
                                 game substate_1
                               | _ ->

                               game st)
  else if str = "take 2" then (print_endline "You picked option 2"; game st)
  else if str = "take 3" then (print_endline "You picked option 3"; game st)

  else game st


and

game st =
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
    | line -> game_commands (String.lowercase_ascii line) st

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






  print_endline "This is the tutorial";
  menu ()

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
  ANSITerminal.(resize 100 50; erase Screen; resize 115 50);
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
