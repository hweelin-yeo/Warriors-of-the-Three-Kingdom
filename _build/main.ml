(* main.ml
 *
 * CS 3110 Fall 2017 Final Project
 *
 *)


(*open State *)


type state = {
  desc : string;
  sec_desc: string list;
  current : string;
  recruit_pool : string list;
  available_picks : string list;
  player_state : string list;
}

let init_state = {
  desc = "hello";
  sec_desc = ["a";"b"];
  current = "this";
  recruit_pool  = ["c";"d"];
  available_picks = ["e";"f"];
  player_state = ["g";"h"]
}
(*********************************************************)
(*     Real Functions                                    *)
(*********************************************************)

let rec game st =
  print_endline ("description of card");
  print_string "\n> ";
  match read_line () with
    | line -> if line = "quit" then () else game st

let init_game () =
  print_endline "How many players? (Max 4 Players)"; print_string "\n> ";
  match read_line () with
    | line -> game init_state
  (* How many humans?*)
  (* How many AI?*)
  (* Difficulty of AI*)
  (* Create new init state based on these then game*)

let rec menu () =
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