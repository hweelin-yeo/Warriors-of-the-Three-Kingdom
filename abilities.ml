open PlayerState
open State
open RecruitPile

let compute_deck_score (p : card_list) acc =
  match p with
  | [] -> acc
  | h :: t -> compute_deck_score t (acc + h.power)

let zhuge_liang_genPlayerSet (pl : player_state list) (id : string) (newps : player_state) =
  match pl with
  | [] -> []
  | h :: t -> if (h.player_id = id) then newps :: zhuge_liang_genPlayerSet else
      h :: zhuge_liang_genPlayerSet

let repl_shu_deck (p: card_list) =
  match p with
  | [] -> []
  | h :: t -> if (h.faction = "Shu") then h :: h :: repl_shu_deck t
    else h :: repl_shu_deck t

let zhuge_liang_function (s : State) (p : PlayerState) (id : int) =
  let newDeck = repl_shu_deck (p.player_deck) in
  let newPlayerState = {
    player_id = p.player_id;
    player_score = compute_deck_score newDeck;
    player_deck = newDeck;
    player_resource = p.player_resource
  } in
  {
    description = s.description;
    sec_description = s.sec_description;
    current_player = s.current_player;
    recruit_pool = s.recruit_pool;
    available_picks = s.available_picks;
    player_states = zhuge_liang_genPlayerSet (p.player_states) p.player_id newPlayerState
  }
