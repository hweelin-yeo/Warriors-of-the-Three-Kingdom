(* main.ml
 *
 * CS 3110 Fall 2017 Final Project
 *
 *)


open State

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

  else game st

and

game st =
  print_endline ("Player " ^ string_of_int st.current_player ^"'s Turn:");
  match (id_to_card_lst st st.available_picks) with
    | h::t -> print_string h.card_name;
  print_string "\n> ";
  match read_line () with
    | line -> game_commands (String.lowercase_ascii line) st

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
                    'tutorial', 'credits'"; menu ()
      )

and

init_tutorial () =
    print_endline "This is the tutorial"; menu ()

and

init_credits () =
    print_endline "This is the credits"; menu ()


let main () =
  print_string "\nWelcome to Explosion of Explosions!.\n";
  print_endline "Created by CS3110 team"; menu ()

let () = main ()