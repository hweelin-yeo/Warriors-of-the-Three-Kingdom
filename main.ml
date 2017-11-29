(* main.ml
 *
 * CS 3110 Fall 2017 Final Project
 *
 *)

type state = {
  desc : string;
  sec_desc: string list;
  current : string;
  recruit_pool : string list;
  available_picks : string list;
  player_state : string list; (*dummy until player_state is implemented *)
}


let init_state = {
  desc = "hello";
  sec_desc = ["a";"b"];
  current = "this";
  recruit_pool  = ["c";"d"];
  available_picks = ["e";"f"];
  player_state = ["g";"h"]
}

let start f =
  try init_state
  with exn -> failwith ("wat")

let rec repl st =
  print_endline ("description of card");
  print_string "\n> ";
  match read_line () with
    | line -> if line = "quit" then () else repl st


let main () =
  print_string "\n\n Welcome to repl for game.\n";
  repl start


let () = main ()



