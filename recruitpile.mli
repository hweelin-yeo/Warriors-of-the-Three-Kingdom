(* RecruitPile.ml *)
open Yojson

(*Card is a abstract type used to represent cards*)
type card

(*init_pile represents the initial recruit pile of the game from a JSON
  file parsed into a card list *)
val init_pile : Yojson.Basic.json -> card list

(*curr_pile is the current state of the recruit deck*)
val curr_pile : card list

(*take_card is a function that represents taking a card from the recruit
pile. Basically a remove function on a list*)
val take_card: card list -> string -> card list

(*add_card will probably never be used but exists for future development
  takes a card object, a card_list, and appends the card into the list*)
val add_card : card -> card list -> card list

(*Gets the size of the remaining recruit pile. Good metadata to have*)
val get_size : card list -> int
