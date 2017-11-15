(* Main.mli *)
(* you only need main.ml *)

open RecruitPile
open State
open Cards
open Ai

(*[repl st] takes in the current state and returns a new state until the
  game terminates in which it finally returns unit *)
val repl: state -> unit

(*[main] is the function that starts the game *)
val main: unit -> unit

