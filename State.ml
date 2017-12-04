(* state.ml *)

open PlayerState

(* type playerID = string *)
type cardID = int

type state = {
  (* description: string;
     sec_description: string list; *)
  total_players: int;
  card_drawn: cardID option;
  current_player: int;
  (* current_player_id: playerID; *)
  recruit_pool: cardID list;
  available_picks: cardID list;
  player_states: (int * player_state) list;
}

type card = {
  card_name : string;
  card_id : int;
  cost : int;
  faction : string;
  power : int;
  flavor : string;
  card_text : string;
  abilities : state -> int -> card list-> state;
  card_type : string;
}


(*Function for vanilla cards, simulates a blank abilities box thus
the returned state is the same as the called state*)
let vanilla (s : state) (cid : cardID) (cl : card list) = s

(*id_to_card takes a card id int and returns the card object option associated with it.
  The inputs are the card id and the card list that represents the card set*)
let rec id_to_card (id : cardID) (cl : card list) =
  match cl with
  | [] -> failwith "shouldn't happen outside of debugging" (*Should not happen*)
  | h :: t -> if (h.card_id = id) then h
    else
      id_to_card id t

let id_to_card_lst st idl = 
  map_id_list idl st.recruit_pool [] 

(*map_id_list maps a list of card ids to their respective cards*)
let rec map_id_list idl  (cl : card list) acc =
  match idl with
  | [] -> List.rev acc
  | h :: t -> map_id_list t cl ((id_to_card h cl) :: acc)

(*map_card_list : maps a list of cards into a list of cardIDs*)
let rec map_card_list (cl : card list) acc =
  match cl with
  | [] -> List.rev acc
  | h :: t -> map_card_list t (h.card_id :: acc)

(*find_player, given the list of int player_state tuples, and a int player id,
  return the corresponding player_state*)
let rec find_player (psl : (int * player_state) list) (id : int) =
  match psl with
  | [] -> failwith "Player not found"
  | h :: t -> if (fst h = id) then snd h else find_player t id

(*find_opponents finds the states of all opponents*)
let rec find_opponents (psl : (int * player_state) list) (id : int) acc =
  match psl with
  | [] -> List.rev acc
  | h :: t -> if (fst h = id) then find_opponents t id acc else
      find_opponents t id (snd h :: acc)

(*Compute_effects takes a player's state and computes bonus scores offered by
  anthem functions*)
let rec compute_anthem_helper al ps acc =
  match al with
  | [] -> acc
  | h :: t -> compute_anthem_helper t ps (h ps + acc)

let compute_anthem (ps : player_state) =
  let anthem_list = ps.player_functions in
  compute_anthem_helper anthem_list ps 0



(*compute_deck_score takes in a card_list that represents the deck and returns
  the total score of the deck*)
let rec compute_deck_score (cl : card list) acc =
  match cl with
  | [] -> + acc
  | h :: t -> compute_deck_score t (h.power + acc)

let rec make_new_states (pl : player_state) (ps : (int * player_state) list) acc =
  match ps with
  | [] -> List.rev acc
  | h :: t -> if (pl.player_id = fst h)
    then make_new_states pl t ((pl.player_id, pl) :: acc) else
      make_new_states pl t (h :: acc)


(*Zhuge liang helper*)
let rec zhuge_liang_helper (pd : card list) acc =
  match pd with
  | [] -> List.rev acc
  | h :: t -> if (h.faction = "Shu") then zhuge_liang_helper t (h :: h :: acc)
    else
      zhuge_liang_helper t (h :: acc)

(*zhuge_liang_funct*)
let zhuge_liang_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player s.player_states currentPlayerInt in
  let current_deck_ids = currentPlayer.player_deck in
  let current_deck = map_id_list current_deck_ids cl [] in
  let new_deck = zhuge_liang_helper current_deck [] in
  let new_deck_score = compute_deck_score new_deck 0 in
  let new_deck_ids = map_card_list new_deck [] in
  let newPlayer1 = {currentPlayer with player_deck = new_deck_ids} in
  let newPlayer = {newPlayer1 with player_score = new_deck_score} in
  let new_deck_score_1 = newPlayer.player_score + compute_anthem newPlayer in
  let new_player_final = {newPlayer with player_score = new_deck_score_1} in
  (*Generate new state object*)
  let new_player_states = make_new_states new_player_final s.player_states [] in
  {s with player_states = new_player_states}

(*Shu Recruiter functions, pd = player deck, nc = num copies*)
let rec shu_recruiter_helper (pd : int list) (nc : int) acc =
  match nc with
  | 0 -> acc
  | x -> shu_recruiter_helper pd (nc - 1) (1 :: acc)

let shu_recruiter_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player s.player_states currentPlayerInt in
  let current_deck_ids = currentPlayer.player_deck in
  let num_units_created = Random.int 4 in
  let new_deck_ids = shu_recruiter_helper current_deck_ids num_units_created current_deck_ids in
  let new_deck = map_id_list new_deck_ids cl [] in
  let new_player_score = compute_deck_score new_deck 0 in
  let newPlayer1 = {currentPlayer with player_deck = new_deck_ids} in
  let newPlayer = {newPlayer1 with player_score = new_player_score} in
  let new_deck_score_1 = newPlayer.player_score + compute_anthem newPlayer in
  let new_player_final = {newPlayer with player_score = new_deck_score_1} in
  let new_player_states = make_new_states new_player_final s.player_states [] in
  {s with player_states = new_player_states}

(*Functions to help implement enchantment spells*)
let shu_anthem_effect (ps : player_state) =
  let pd = ps.player_deck in
  List.length pd

(*Anthem of shu helper*)
let shu_anthem_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player s.player_states currentPlayerInt in
  let newFunctionList = shu_anthem_effect :: currentPlayer.player_functions in
  let curr_deck_id = currentPlayer.player_deck in
  let curr_deck = map_id_list curr_deck_id cl [] in
  let deck_score = compute_deck_score curr_deck 0 in
  let new_score = compute_anthem currentPlayer + deck_score in
  let newPlayer1 = {currentPlayer with player_functions = newFunctionList} in
  let newPlayer = {newPlayer1 with player_score = new_score} in
  let new_player_states = make_new_states newPlayer s.player_states [] in
  {s with player_states = new_player_states}



(*Sima Yi functions*)
let sima_yi_helper (ids : int list) =
  (*h1 = 7, id of Sima Yi*)
  match ids with
  | [] -> []
  | h1 :: h2 :: h3 :: h4 :: t -> 7 :: t
  | h :: t -> [7] (*All other cases with less than 3 units get eliminated*)

let sima_yi_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player s.player_states currentPlayerInt in
  let current_deck_ids = currentPlayer.player_deck in
  let new_deck_ids = sima_yi_helper current_deck_ids in
  let new_deck = map_id_list new_deck_ids cl [] in
  let new_player_score = compute_deck_score new_deck 0 in
  let newPlayer1 = {currentPlayer with player_deck = new_deck_ids} in
  let newPlayer = {newPlayer1 with player_score = new_player_score} in
  let new_deck_score_1 = newPlayer.player_score + compute_anthem newPlayer in
  let new_player_final = {newPlayer with player_score = new_deck_score_1} in
  let new_player_states = make_new_states new_player_final s.player_states [] in
  {s with player_states = new_player_states}

(*Xiahou Dun Functions*)
(*Need to finish, not completed yet*)
let rec remove_top_element (pd : int list) =
  match pd with
  | [] -> []
  | h :: t -> t

(*opl = original_player_list cps = current_player_state, nol = new_opponent_list*)
let rec rebuild_player_list (opl : (int * player_state) list) cps nol acc =
  match opl with
  | [] -> List.rev acc
  | h :: t -> if (cps.player_id = fst h) then
      rebuild_player_list t cps nol ((cps.player_id, cps) :: acc) else (*player the current player*)
      begin match nol with
        | [] -> failwith "failed to find player to insert"
        | h1 :: t1 -> if (h1.player_id = fst h) then
            rebuild_player_list t cps nol ((h1.player_id, h1) :: acc) else
            rebuild_player_list opl cps t1 acc
      end


let rec xiahou_dun_helper (opponentList : player_state list) acc cl =
  match opponentList with
  | [] -> List.rev acc
  | h :: t -> let opponentDeck = h.player_deck in
    let new_deck_id = remove_top_element opponentDeck in
    let new_deck = map_id_list new_deck_id cl [] in
    let newScore = compute_deck_score new_deck 0 in
    let newPlayer1 = {h with player_score = newScore} in
    let newPlayer = {newPlayer1 with player_deck = new_deck_id} in
    let new_score_1 = newPlayer.player_score + compute_anthem newPlayer in
    let new_player_final = {newPlayer with player_score = new_score_1} in
    xiahou_dun_helper t (new_player_final :: acc) cl



let xiahou_dun_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let original_player_list = s.player_states in
  let current_player_state = find_player s.player_states currentPlayerInt in
  let current_deck_ids = current_player_state.player_deck in
  let current_deck = map_id_list current_deck_ids cl [] in
  let new_player_score_1 = compute_deck_score current_deck 0 in
  let new_player_score_final = new_player_score_1 + compute_anthem current_player_state in
  let new_player_state = {current_player_state with player_score = new_player_score_final} in
  let opponentList = find_opponents s.player_states currentPlayerInt [] in
  let new_opponent_list = xiahou_dun_helper opponentList [] cl in
  let new_player_list = rebuild_player_list original_player_list new_player_state new_opponent_list [] in
  {s with player_states = new_player_list}

(*Wei Recruit helpers*)
let wei_recruit_helper (cd : int list) acc =
  match cd with
  | [] -> []
  | h1 :: h2 :: t -> h1 :: t (*Minium two element list to pop bottom list*)
  | h :: [] -> [h] (*One element list, do nothing, enjoy the aggro*)

let wei_recruit_funct (s:state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let current_player_state = find_player s.player_states currentPlayerInt in
  let current_deck_ids = current_player_state.player_deck in
  let new_deck_ids = wei_recruit_helper current_deck_ids [] in
  let newPlayer1 = {current_player_state with player_deck = new_deck_ids} in
  let new_player_deck = map_id_list current_deck_ids cl [] in
  let new_deck_score = compute_deck_score new_player_deck 0 in
  let newPlayer = {newPlayer1 with player_score = new_deck_score} in
  let new_deck_score_1 = newPlayer.player_score + compute_anthem newPlayer in
  let new_player_final = {newPlayer with player_score = new_deck_score_1} in
  let new_player_states = make_new_states new_player_final s.player_states [] in
  {s with player_states = new_player_states}

(*Dian Wei Helpers*)
let dian_wei_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let original_player_list = s.player_states in
  let current_player_state = find_player s.player_states currentPlayerInt in
  let newPlayer = {current_player_state with player_resource =
                                               current_player_state.player_resource - 1} in
  let current_player = find_player s.player_states currentPlayerInt in
  let current_deck_id = current_player.player_deck in
  let current_deck = map_id_list current_deck_id cl [] in
  let new_score = compute_deck_score current_deck 0 + compute_anthem newPlayer in
  let newPlayer = {current_player with player_score = new_score} in
  let new_player_states = make_new_states newPlayer original_player_list [] in
  {s with player_states = new_player_states}

(*Wei Polluter Effect*)
let rec wei_polluter_helper opponentList acc =
  match opponentList with
  | [] -> List.rev acc
  | h :: t ->
    let new_opponent_state = {h with player_resource = h.player_resource - 1} in
    wei_polluter_helper t (new_opponent_state :: acc)


let wei_polluter_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let original_player_list = s.player_states in
  let current_player_state = find_player s.player_states currentPlayerInt in
  let current_deck_ids = current_player_state.player_deck in
  let current_deck = map_id_list current_deck_ids cl [] in
  let new_player_score_1 = compute_deck_score current_deck 0 in
  let new_player_score_final = new_player_score_1 + compute_anthem current_player_state in
  let new_player_state = {current_player_state with player_score = new_player_score_final} in
  let opponentList = find_opponents s.player_states currentPlayerInt [] in
  let new_opponent_list = wei_polluter_helper opponentList [] in
  let new_player_list = rebuild_player_list original_player_list new_player_state new_opponent_list [] in
  {s with player_states = new_player_list}

(*Wu Scout helpers*)
let wu_scout_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let original_player_list = s.player_states in
  let current_player_state = find_player s.player_states currentPlayerInt in
  let newPlayer = {current_player_state with player_resource =
                                               current_player_state.player_resource + 1} in
  let current_player = find_player s.player_states currentPlayerInt in
  let current_deck_id = current_player.player_deck in
  let current_deck = map_id_list current_deck_id cl [] in
  let new_score = compute_deck_score current_deck 0 + compute_anthem newPlayer in
  let newPlayer = {current_player with player_score = new_score} in
  let new_player_states = make_new_states newPlayer original_player_list [] in
  {s with player_states = new_player_states}

(*Wu anthem helpers*)
let wu_anthem_effect (ps : player_state) = ps.player_resource

let wu_anthem_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player s.player_states currentPlayerInt in
  let newFunctionList = wu_anthem_effect :: currentPlayer.player_functions in
  let curr_deck_id = currentPlayer.player_deck in
  let curr_deck = map_id_list curr_deck_id cl [] in
  let deck_score = compute_deck_score curr_deck 0 in
  let new_score = compute_anthem currentPlayer + deck_score in
  let newPlayer1 = {currentPlayer with player_functions = newFunctionList} in
  let newPlayer = {newPlayer1 with player_score = new_score} in
  let new_player_states = make_new_states newPlayer s.player_states [] in
  {s with player_states = new_player_states}

(*Helper function to implement Lu Zuishen*)
(*let lu_da_funct (s : state) (cid : int) (cl : card list) = 
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player s.player_states currentPlayerInt in
  let coin_flip = Random.int 1 in *)

(*The great david gries is implemented here*)
let rec obliterate (opponentList : player_state list) acc cl =
  match opponentList with
  | [] -> List.rev acc
  | h :: t -> let rekt_player = {h with player_deck = []} in
    let new_player_score = compute_deck_score [] 0 + compute_anthem rekt_player in
    let totally_rekt_player = {rekt_player with player_score = new_player_score} in
    obliterate t (totally_rekt_player :: acc) cl

let david_gries_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player s.player_states currentPlayerInt in
  let die_roll = Random.int 56 in
  match die_roll with
  | 0 -> let new_player = {currentPlayer with player_deck = []} in
    let new_player_score = compute_deck_score [] 0 + compute_anthem new_player in
    let rekt_player = {new_player with player_score = new_player_score} in
    let new_player_states = make_new_states rekt_player s.player_states [] in
    {s with player_states = new_player_states}
  | 55 ->   let opponentList = find_opponents s.player_states currentPlayerInt [] in
    let new_opponent_list = obliterate opponentList [] cl in
    let current_deck_id = currentPlayer.player_deck in
    let current_deck = map_id_list current_deck_id cl [] in
    let new_score = compute_deck_score current_deck 0 + compute_anthem currentPlayer in
    let new_player = {currentPlayer with player_score = new_score} in
    let new_player_list = rebuild_player_list s.player_states new_player new_opponent_list [] in
    {s with player_states = new_player_list}
  | _ ->     let current_deck_id = currentPlayer.player_deck in
      let current_deck = map_id_list current_deck_id cl [] in
      let new_score = compute_deck_score current_deck 0 + compute_anthem currentPlayer in
      let new_player = {currentPlayer with player_score = new_score} in
      let new_player_states = make_new_states new_player s.player_states [] in
      {s with player_states = new_player_states}



    (*List of card records that simulates every card available*)
let cardList = [
  {
    card_name = "Shu Footsoldier";
    card_id = 0;
    cost = 1;
    faction = "Shu";
    power = 1;
    flavor = "Liu Bei's soldiers were recruited from the farmhands of \n China";
    card_text = "No Abilities";
    abilities = vanilla;
      card_type = "Soldier"
  };

  {
    card_name = "Zhuge Liang, Sleeping Dragon";
    card_id = 1;
    cost = 4;
    faction = "Shu";
    power = 2;
    flavor = "The greatest strategist of the Three Kingdoms was the Sleeping \n
Dragon, Zhuge Liang";
    card_text = "When you draft Zhuge Liang, Sleeping Dragon, for each \n Shu card in your deck, copy that card and add it to your deck";
    abilities = zhuge_liang_funct;
      card_type = "Soldier"
  };

  {
    card_name = "Shu Recruiter";
    card_id = 2;
    cost = 3;
    faction = "Shu";
    power = 1;
    flavor = "The Shu army came on like a swarm of ants, \n each man who was slain was replaced with two brothers";
    card_text = "When you draft Shu Recruiter, add a random number 0-3 of \n Shu Footsoldier tokens into your deck";
    abilities = shu_recruiter_funct;
    card_type = "Soldier"
  };

  {
    card_name = "Anthem of Shu";
    card_id = 5;
    cost = 2;
    faction = "Shu";
    power = 0;
    flavor = "United We Stand";
    card_text = "Enchant Spell: Gain power equal to the number of units in your deck";
    abilities = shu_anthem_funct;
    card_type = "Spell";
  };

  {
    card_name = "Sima Yi, Traitor to Wei";
    card_id = 6;
    cost = 4;
    faction = "Wei";
    power = 10;
    flavor = "Not the Lius, not the Suns, nor the Caos would conquer China, \n for the ursurper Sima clan would conquer all";
    card_text = "When you draft Sima Yi Traitor to Wei discard the top 3 cards of your deck";
    abilities = sima_yi_funct;
    card_type = "Soldier"
  };

  {
    card_name = "Wei Elite Infantry";
    card_id = 7;
    cost = 3;
    faction = "Wei";
    power = 3;
    flavor = "Cao Cao's soldiers were all hand picked, veterans of countless \n conflicts";
    card_text = "No Abilities";
    abilities = vanilla;
    card_type = "Soldier";
  };

  {
    card_name = "Xiahou Dun, The One-Eyed";
    card_id = 8;
    cost = 4;
    faction = "Wei";
    power = 2;
    flavor = "He used to have two eyes. Until he ate one";
    card_text = "When you draft Xiahou Dun, discard the top card from each \n opponent's deck";
    abilities = xiahou_dun_funct;
    card_type = "Soldier";
  };

  {
    card_name = "Wei Recruit";
    card_id = 9;
    cost = 1;
    faction = "Wei";
    power = 3;
    flavor = "He had enthusiasm. And a murderous streak";
    card_text = "When you draft Wei Recruit, discard the top card of your deck";
    abilities = vanilla;
    card_type = "Soldier";
  };

  {
    card_name = "Dian Wei, the Berserk";
    card_id = 10;
    cost = 2;
    faction = "Wei";
    power = 4;
    flavor = "Wielding two men as axes, Dian Wei protected Cao Cao with his very \n life";
    card_text = "When you draft Dian Wei, lose 1 resource";
    abilities = dian_wei_funct;
    card_type = "Soldier";
  };

  {
    card_name = "Wei Polluter";
    card_id = 11;
    cost = 5;
    faction = "Wei";
    power = 2;
    flavor = "Nothing was too low for Cao Cao to stoop to, including the slaughter \n of the innocent populace";
    card_text = "When you draft Wei Polluter each other player loses 1 resource";
    abilities = wei_polluter_funct;
    card_type = "Soldier";
  };

  {
    card_name = "Wu Boatsman";
    card_id = 12;
    cost = 2;
    faction = "Wu";
    power = 2;
    flavor = "Soldiers of Wu fought not for glory, but for their families and for \n their homeland";
    card_text = "No Abilities";
    abilities = vanilla;
    card_type = "Solider";
  };

  {
    card_name = "Wu Scout";
    card_id = 13;
    cost = 1;
    faction = "Wu";
    power = 1;
    flavor = "Wu scouts were experts of the forest, for the fought for their homeland";
    card_text = "When you draft Wu Recruits, increase your maximum resources by 1";
    abilities = wu_scout_funct;
    card_type = "Soldier";
  };

  {
    card_name = "Anthem of Wu";
    card_id = 17;
    faction = "Wu";
    cost = 2;
    power = 0;
    flavor = "Their banners high, the fleet of Wu advanced on the Red Cliffs";
    card_text = "Gain power equal to your resources";
    abilities = wu_anthem_funct;
    card_type = "Spell";
  };

  {
    card_name = "Lu Bu, Mad Demon";
    card_id = 18;
    cost = 6;
    faction = "Other";
    power = 8;
    flavor = "Of the warriors of the Three Kingdoms, there were none stronger than \n Lu Bu";
    card_text = "No Abiitities";
    abilities = vanilla;
    card_type = "Soldier";
  };

  {
    card_name = "Tiago Chan";
    card_id = 19;
    cost = 5;
    faction = "Other";
    power = 2;
    flavor = "Snapcaster Mage, but better";
    card_text = "When you draft Tiago Chan, cast each spell in your deck again";
    abilities = vanilla; (*Will definitely be changed later*)
    card_type = "Soldier";
  };

  {
    card_name = "Lu Zuishen of the Drunken Fist";
    card_id = 20;
    cost = 2;
    faction = "Other";
    power = 4;
    flavor = "He was very drunk. And he could fight";
    card_text = "When you draft Lu Zuishen of the Drunken Fist, flip a coin \n if that coin was tails discard Lu Zuishen";
    abilities = vanilla;
    card_type = "Soldier";
  };

  {
    card_name = "David Gries";
    card_id = 23;
    cost = 6;
    faction = "Other";
    power = 1;
    flavor = "I have 56 years of programming experience. You have, perhaps, 1";
    card_text = "when you draft David Gries, roll a 56 sided dice. If the result \n
is 56 then destroy all your opponents decks. \n if the result is 1, discard your own deck";
    abilities = vanilla;
    card_type = "Solider";
  }
]

    (****************************************************************************************************)

let rec contains e lst =
  match lst with
  | [] -> false
  | h :: t -> if h = e then true else contains e t

let rec generate_nums num bound accum =
  if (List.length accum < num) then
    let new_num = Random.int bound in
    if (contains new_num accum) then generate_nums num bound accum
    else generate_nums num bound (new_num :: accum)
  else accum

(* [cpicks_from_index num_lst card_lst] returns a list of elements in
    card_lst whose positions correspond to the numbers in the num_lst *)

let rec picks_from_index num_lst card_lst =
  match num_lst with
  | [] -> []
  | h :: t -> (List.nth card_lst h) :: picks_from_index t card_lst

(* [refresh_available_picks s] returns a card list that corresponds
   to the next set of randomised available picks *)

let refresh_available_picks st =
  let n = List.length (st.recruit_pool) in
  picks_from_index (generate_nums 3 n []) (st.recruit_pool)


let return_next_player st prev =
  if prev = st.total_players then 1
  else prev+1

let change_next_player (st: state) =
  { st with card_drawn = None;
            current_player = return_next_player st (st.current_player);
            available_picks = refresh_available_picks st
  }

let change_to_player st i =
  { st with card_drawn = None;
            current_player = i;
            available_picks = refresh_available_picks st}

(* SPECIFICATION  *)

let card_f_state st f = f st

(* [change s p_id c_id f] returns a new player state after f, a card
   function of card with id [c_id] is applied to the state. *)

(* let change s p_id c_id f = failwith "unimplemented" *)
let add_card_recruit_pool st card =
  { st with recruit_pool = card :: st.recruit_pool}

let rec remove_card pool card =
  match pool with
  | [] -> []
  | h :: t -> if h = card then t else
      h :: remove_card t card

let remove_card_recruit_pool st card =
  let recpool_new = remove_card (st.recruit_pool) card in
  { st with recruit_pool = recpool_new }

  (* [return_pstate ps i] is a helper function for [return_player_state s id].
    It returns a playerstate with player_id_int [i]. *)

let rec return_pstate ps i =
  match ps with
  | [] -> failwith "No player with that id found"
  | (n, h) :: t -> if n = i then h else return_pstate t i

let rec return_player_state s i =
  return_pstate (s.player_states) i

(* [change_player_state s i] returns a state with playerstate,
   whose player_id_int is 1, changed. *)

let change_player_state s ps  =
  let i = s.current_player in
  let pstates = List.remove_assoc (i) (s.player_states) in
  let new_player_states = (i, ps) :: pstates in
  { s with player_states = new_player_states}

let draw_card (c: cardID) (st: state) =
  let remove_rec_pool = remove_card_recruit_pool st c in
  let ps = return_player_state st (st.current_player) in
  let new_ps = (add_card c ps) in
  let changed_ps = change_player_state remove_rec_pool new_ps in
  { changed_ps with card_drawn = Some c }

(* [init_player_states n h accum] returns a list of playerstates,
   where n is the number of playerstates, h is the number of human
   players *)

let rec init_player_states n h accum =
  match n with
  | 0 -> accum
  | _ -> begin
    match h with
      | 0 -> let new_ps = (n, (init_player_state n true)) in
        init_player_states (n-1) 0 (new_ps :: accum)
      | _ -> let new_ps = (n, init_player_state n false) in
        init_player_states (n-1) (h-1) (new_ps :: accum)
    end

(* [generate_0_to_n_lst n accum] returns an int list [0; ...; n]
 * requires: n is an int *)
let rec generate_0_to_n_lst n accum =
  match n with
  | -1 -> accum
  | _ -> generate_0_to_n_lst (n-1) (n :: accum)

let init_state i h =
  {
     (* description = "";
        sec_description = "" :: []; *)
    total_players = i;
    card_drawn = None;
    current_player = 1;
    (* current_player_id = "Player 1"; *)
    recruit_pool = generate_0_to_n_lst 23 [];
    available_picks = generate_nums 3 23 [];
    player_states = init_player_states i h [];
  }



(* let change_description st str =
  { st with description = str }

let change_sec_description st str =
  { st with sec_description = str } *)
