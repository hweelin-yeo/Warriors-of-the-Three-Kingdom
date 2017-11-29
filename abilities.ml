open PlayerState
open State
open RecruitPile

let repl_shu_deck (p: card_list) =
  match p with
  | [] -> []
  | h :: t -> if (h.faction = "Shu") then h :: h :: repl_shu_deck t
    else h :: repl_shu_deck t

let zhuge_liang_function (S : State) (p : PlayerState) (id : int) =
  let newDeck = repl_shu_deck (p.player_deck) in
  let newPlayerState = 
