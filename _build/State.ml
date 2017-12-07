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

(**************************************************************************************)
(****************************** Observer Functions ************************************
******************* Find cards, players, opponents and compute scores******************)
(**************************************************************************************)


(* [id_to_card id cl] takes a card id int and returns the card object option
 * associated with it. The inputs are the card id and the card list
 * that represents the card set
 * requires: [id] is a cardID
             [cl] is a card list
 *)

let rec id_to_card (id : cardID) (cl : card list) =
  match cl with
  | [] -> failwith "invalid cardID: should not happen"
  | h :: t -> if (h.card_id = id) then h
    else id_to_card id t

(* [map_id_list idl cl acc] maps a list of card ids to their respective cards
 * requires: [idl] is a list of cardID
             [cl] is a list of cards
             [acc] is a list of cards
 *)

let rec map_id_list idl  (cl : card list) acc =
  match idl with
  | [] -> List.rev acc
  | h :: t -> map_id_list t cl ((id_to_card h cl) :: acc)

(* [map_card_list cl acc] maps a list of cards into a list of its
 *  corresponding cardIDs
 * requires: [cl] is a list of cards
             [acc] is a list of cardIDs
 *)

let rec map_card_list (cl : card list) acc =
  match cl with
  | [] -> List.rev acc
  | h :: t -> map_card_list t (h.card_id :: acc)

(* [find_player psl id] returns the corresponding player_state given
 * the list of int player_state tuples, and a int player id,
 * requires: [psl] is a list of (int * player states)
             [id] is an int
 *)

let rec find_player (psl : (int * player_state) list) (id : int) =
  match psl with
  | [] -> failwith "Player not found"
  | h :: t -> if (fst h = id) then snd h else find_player t id

(* [find_opponents psl id acc] finds the states of all opponent s
 * requires: [psl] is a list of (int * player_state)
             [id] is an int
             [accum] is a list of player_state *)

let rec find_opponents (psl : (int * player_state) list) (id : int) acc =
  match psl with
  | [] -> List.rev acc
  | h :: t -> if (fst h = id) then find_opponents t id acc else
      find_opponents t id (snd h :: acc)

(* [compute_effects al ps acc] takes a player's state and computes
   bonus scores offered by anthem functions *)
let rec compute_anthem_helper al ps acc =
  match al with
  | [] -> acc
  | h :: t -> compute_anthem_helper t ps (h ps + acc)

let compute_anthem (ps : player_state) =
  let anthem_list = ps.player_functions in
  compute_anthem_helper anthem_list ps 0

(* [compute_deck_score cl acc] takes in a card_list that represents
  the deck and returns the total score of the deck *)
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


(**************************************************************************************)
(****************************** Card Functions ****************************************
************************ Vanilla and Customised functions of cards ********************)
(**************************************************************************************)


(* Function for vanilla cards, simulates a blank abilities box thus
  the returned state is the same as the called state *)

let vanilla (s : state) (cid : cardID) (cl : card list) =
  let current_player_id = s.current_player in
  let current_player = find_player s.player_states current_player_id in
  let current_deck_id = current_player.player_deck in
  let current_deck = map_id_list current_deck_id cl [] in
  let new_score = compute_deck_score current_deck 0 + compute_anthem current_player in
  let new_player_state = {current_player with player_score = new_score} in
  let new_player_states = make_new_states new_player_state s.player_states [] in
  {s with player_states = new_player_states}

(* Zhuge liang helper *)
let rec zhuge_liang_helper (pd : card list) acc =
  match pd with
  | [] -> List.rev acc
  | h :: t -> if (h.faction = "Shu") then zhuge_liang_helper t (h :: h :: acc)
    else
      zhuge_liang_helper t (h :: acc)

(* Zhuge_liang_funct *)
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

(* Shu Recruiter functions, pd = player deck, nc = num copies *)
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

(* Shu Sergant function *)
let shu_sergant_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player s.player_states currentPlayerInt in
  let current_deck_ids = currentPlayer.player_deck in
  let new_deck_ids = 1 :: current_deck_ids in
  let new_player_1 = {currentPlayer with player_deck = new_deck_ids} in
  let new_deck = map_id_list new_deck_ids cl [] in
  let new_score = compute_deck_score new_deck 0 + compute_anthem new_player_1 in
  let new_player_final = {new_player_1 with player_score = new_score} in
  let new_player_states = make_new_states new_player_final s.player_states [] in
  {s with player_states = new_player_states}

(* Functions to help implement enchantment spells *)
let shu_anthem_effect (ps : player_state) =
  let pd = ps.player_deck in
  List.length pd

(* Anthem of shu helper *)
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

(* Liu Bei Function *)
let rec liu_bei_helper cd acc =
  match cd with
  | [] -> if (acc > 2) then true else false
  | h :: t -> if (h = 1) then liu_bei_helper t (acc + 1) else
      liu_bei_helper t acc

let liu_bei_funct (s : state) (cid : int) (cl : card list) =
  let current_player_id = s.current_player in
  let current_player = find_player s.player_states current_player_id in
  let current_deck_id = current_player.player_deck in
  let current_deck = map_id_list current_deck_id cl [] in
  let new_score = compute_deck_score current_deck 0 + compute_anthem current_player in
  let new_player_state = {current_player with player_score = new_score} in
  let new_player_states = make_new_states new_player_state s.player_states [] in
  let liu_bei_condition = liu_bei_helper current_deck_id 0 in
  match liu_bei_condition with
  | true -> shu_anthem_funct {s with player_states = new_player_states} cid cl
  | false -> {s with player_states = new_player_states}

(* Sima Yi functions *)
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

(* Xiahou Dun Functions*)
(* Need to finish, not completed yet*)
let rec remove_top_element (pd : int list) =
  match pd with
  | [] -> []
  | h :: t -> t

(* opl = original_player_list cps = current_player_state, nol = new_opponent_list
onol = original new opponent list*)
                (*NTS: Need to fix*)
let rec rebuild_player_list (opl : (int * player_state) list) cps nol acc =
  let state_list = List.map (fun x -> (x.player_id, x)) nol in
  let current_player_unit = (cps.player_id, cps) in
  current_player_unit :: state_list
  (*match opl with
  | [] -> List.rev acc
  | h :: t -> if (cps.player_id = fst h) then
      rebuild_player_list t cps nol ((cps.player_id, cps) :: acc) else (*player the current player*)
      begin match nol with
        | [] -> failwith "failed to find player to insert"
        | h1 :: t1 -> if (h1.player_id = fst h) then
            rebuild_player_list t cps nol ((h1.player_id, h1) :: acc) else
            rebuild_player_list opl cps t1 acc
      end*)

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

(* Wei Recruit helpers *)
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

(* Dian Wei Helpers *)
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

(* Wei Polluter Effect *)
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

(* Wu Scout helpers *)
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

let lu_meng_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let original_player_list = s.player_states in
  let current_player_state = find_player s.player_states currentPlayerInt in
  let resource_total = Random.int 3 in
  let newPlayer = {current_player_state with player_resource =
                    current_player_state.player_resource + resource_total} in
  let current_player = find_player s.player_states currentPlayerInt in
  let current_deck_id = current_player.player_deck in
  let current_deck = map_id_list current_deck_id cl [] in
  let new_score = compute_deck_score current_deck 0 + compute_anthem newPlayer in
  let newPlayer = {current_player with player_score = new_score} in
  let new_player_states = make_new_states newPlayer original_player_list [] in
  {s with player_states = new_player_states}

(* Lady Sun functions *)
let rec lady_sun_helper cl =
  match cl with
  | [] -> false
  | h :: t -> if h = 4 then true else lady_sun_helper t

let lady_sun_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let original_player_list = s.player_states in
  let current_player_state = find_player s.player_states currentPlayerInt in
  let current_id_list = current_player_state.player_deck in
  match lady_sun_helper current_id_list with
  | true -> let new_id_list = 1 :: 1 :: 1 :: current_id_list in
    let current_deck = map_id_list new_id_list cl [] in
    let newPlayer = {current_player_state with player_deck = new_id_list} in
    let new_score = compute_deck_score current_deck 0 + compute_anthem newPlayer in
    let newPlayer1 = {newPlayer with player_score = new_score} in
    let new_player_states = make_new_states newPlayer1 original_player_list [] in
    {s with player_states = new_player_states}
  | false ->
    let current_deck = map_id_list current_id_list cl [] in
    let new_score = compute_deck_score current_deck 0 + compute_anthem current_player_state in
    let newPlayer1 = {current_player_state with player_score = new_score} in
    let new_player_states = make_new_states newPlayer1 original_player_list [] in
    {s with player_states = new_player_states}
(* Todo still need to finish *)

let lu_su_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let original_player_list = s.player_states in
  let current_player_state = find_player s.player_states currentPlayerInt in
  let newPlayer = {current_player_state with player_resource = 4} in
  let current_player = find_player s.player_states currentPlayerInt in
  let current_deck_id = current_player.player_deck in
  let current_deck = map_id_list current_deck_id cl [] in
  let new_score = compute_deck_score current_deck 0 + compute_anthem newPlayer in
  let newPlayer = {current_player with player_score = new_score} in
  let new_player_states = make_new_states newPlayer original_player_list [] in
  {s with player_states = new_player_states}

(* Wu anthem helpers *)
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

(* Helper function to implement Lu Zuishen *)
let lu_da_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player s.player_states currentPlayerInt in
  let current_deck_ids = currentPlayer.player_deck in
  let coin_flip = Random.int 2 in
  match coin_flip with
  | 0 -> let removed_deck = List.tl current_deck_ids in
    let new_deck = map_id_list removed_deck cl [] in
    let new_deck_ids = map_card_list new_deck [] in
    let new_score_1 = compute_deck_score new_deck 0 in
    let new_player_state = {currentPlayer with player_deck = new_deck_ids} in
    let new_player_state_1 = {new_player_state with player_score = new_score_1} in
    let new_score = new_player_state_1.player_score + compute_anthem new_player_state_1 in
    let final_player_state = {new_player_state_1 with player_score = new_score}; in
    let new_player_states = make_new_states final_player_state s.player_states [] in
    {s with player_states = new_player_states}
  | 1 -> let current_deck = map_id_list current_deck_ids cl [] in
    let new_score = compute_deck_score current_deck 0 + compute_anthem currentPlayer in
    let new_player_state = {currentPlayer with player_score = new_score} in
    let new_player_states = make_new_states new_player_state s.player_states [] in
    {s with player_states = new_player_states}
  | _ -> failwith "Random number doesnt generate > 1"

(* Functions to implement Hundred-Faced Hassad *)
let hassan_helper ids =
  match ids with
  | [] -> []
  | h :: h1 :: t -> h :: h1 :: h1 :: t
  | h :: t -> h :: t

let hassan_funct (s : state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player s.player_states currentPlayerInt in
  let current_deck_ids = currentPlayer.player_deck in
  let new_deck_ids = hassan_helper current_deck_ids in
  let new_deck = map_id_list new_deck_ids cl [] in
  let new_deck_score = compute_deck_score new_deck 0 in
  let new_player_1 = {currentPlayer with player_deck = new_deck_ids} in
  let new_player_2 = {new_player_1 with player_score = new_deck_score} in
  let new_score = new_deck_score + compute_anthem new_player_2 in
  let new_player_final = {new_player_2 with player_score = new_score} in
  let new_player_states = make_new_states new_player_final s.player_states [] in
  {s with player_states = new_player_states}

(* Atilla the conquerer function, dl = deck list, odl = original deck list *)
let rec atilla_helper (dl : card list) (odl : card list) (sl : string list) =
  match sl with
  | [] -> true
  | h :: t -> begin match dl with
      | [] -> false
      | h1 :: t1 -> if (h1.faction = h) then
          atilla_helper odl odl t else
          atilla_helper t1 odl sl
    end


let atilla_funct (s: state) (cid : int) (cl : card list) =
  let currentPlayerInt = s.current_player in
  let currentPlayer = find_player s.player_states currentPlayerInt in
  let current_deck_id = currentPlayer.player_deck in
  let current_deck = map_id_list current_deck_id cl [] in
  let condition_verified = atilla_helper current_deck current_deck ["Shu" ; "Wu" ; "Wei"] in
  match condition_verified with
  | true -> let new_deck_id = 18 :: current_deck_id in
    let new_player_state = {currentPlayer with player_deck = new_deck_id} in
    let new_deck = map_id_list new_deck_id cl [] in
    let new_score = compute_deck_score new_deck 0 + compute_anthem new_player_state in
    let new_player_final = {currentPlayer with player_score = new_score} in
    let new_player_states = make_new_states new_player_final s.player_states [] in
    {s with player_states = new_player_states}
  | false -> let new_deck = map_id_list current_deck_id cl [] in
    let new_score = compute_deck_score new_deck 0 + compute_anthem currentPlayer in
    let new_player_final = {currentPlayer with player_score = new_score} in
    let new_player_states = make_new_states new_player_final s.player_states [] in
    {s with player_states = new_player_states}

(* Tiago Chan functions here *)
let tiago_chan_helper current_deck_ids =
  match current_deck_ids with
  | [] -> None
  | h1 :: h2 :: t -> Some h2
  | h :: t -> None

let rec tiago_chan_funct (s : state) (cid : int) (cl : card list) =
  let current_player_int = s.current_player in
  let current_player = find_player s.player_states current_player_int in
  let current_deck_ids = current_player.player_deck in
  let target_card = tiago_chan_helper current_deck_ids in
  match target_card with
  | None -> vanilla s cid cl
  | Some c -> let target_card = id_to_card c cl in
    target_card.abilities s cid cl

(* The great david gries is implemented here *)
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


(**************************************************************************************)
(****************************** Card List***** ****************************************
***************************************************************************************)
(**************************************************************************************)


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
    card_name = "Shu Sergant";
    card_id = 3;
    cost = 2;
    faction = "Shu";
    power = 1;
    flavor = "The Sergant wasn't so much a drill leader in the Shu Army so much \n as a brother and advisor";
    card_text = "When you draft Shu Sergant, add a 1 power Shu Footsolider token \n to your deck ";
    abilities = shu_sergant_funct;
    card_type = "Soldier";
  };

  {
    card_name = "Liu Bei, Lord of Shu";
    card_id = 4;
    cost = 5;
    faction = "Shu";
    power = 2;
    flavor = "Liu Bei fought for the people rather than personal ambition";
    card_text = "When you draft Liu Bei, if you have more than 3 Shu Footsoliders \n in your deck, gain an anthem of Shu";
    abilities = liu_bei_funct;
    card_type = "Soldier";
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
    card_name = "Lu Meng, Wu Admiral";
    card_id = 14;
    cost = 3;
    faction = "Wu";
    power = 2;
    flavor = "Lu Meng was responsible for mantaining the impressive supply lines \n of Wu";
    card_text = "When you draft Lu Meng, gain 0, 1 or 2 resource, chosen at random";
    abilities = lu_meng_funct;
    card_type = "Soldier";
  };

  {
    card_name = "Lady Sun";
    card_id = 15;
    cost = 2;
    faction = "Wu";
    power = 1;
    flavor = "Caught between her brother Sun Quan and her husband Liu Bei, \n Lady Sun was caught between Wu and Shu";
    card_text = "When you draft Lady Sun, if you have Liu Bei, gain a 3 Shu   \n Footsolider tokens";
    abilities = lady_sun_funct;
    card_type = "Solider";
  };

  {
    card_name = "Lu Su, Cunning Strategist";
    card_id = 16;
    cost = 3;
    faction = "Wu";
    power = 4;
    flavor = "As crafty as he was bold, Lu Su was the right hand of Wu";
    card_text = "When you draft Lu Su, set your current resources to 4";
    abilities = lu_su_funct;
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
    card_text = "When you draft Tiago Chan, use the ability of the top card of your deck";
    abilities = tiago_chan_funct;
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
    abilities = lu_da_funct;
    card_type = "Soldier";
  };

  {
    card_name = "Hassan of the Hundred Faces";
    card_id = 21;
    cost = 3;
    faction = "Other";
    power = 0;
    flavor = "It used it's own multiple personality disorder to become anyone \n and everyone";
    card_text = "When you draft Hassan of the Hundred Faces, add a copy of the top \n card of your deck to your deck";
    abilities = hassan_funct;
    card_type = "Soldier";
  };

  {
    card_name = "Atilla, the Conquerer";
    card_id = 22;
    cost = 4;
    faction = "Other";
    power = 4;
    flavor = "His empire expanded from China to Europe, the single largest empire \n of the history of mankind";
    card_text = "If your deck contains a card of Shu, Wu and Wei, add a 12 power \n token into your deck";
    abilities = atilla_funct;
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
    abilities = david_gries_funct;
    card_type = "Solider";
  }
]

(**************************************************************************************)
(****************************** State Functions ****************************************
************************ RNG for cards, Update States,  ....... ********************)
(**************************************************************************************)

(* [contains e lst] returns true if e is an element of lst, false otherwise
 * requires: [e] is type 'a -> [lst] is a 'a list
 *)

let rec contains e lst =
  match lst with
  | [] -> false
  | h :: t -> if h = e then true else contains e t

(* [generate_nums num bound lst accum] returns a list, l1, with [num] numbers
 * less than bound. Each element in l1 should be a member of lst. If lst has
 * 3 or fewer elements, it returns lst.
 * requires: [num] is an int, [bound] is an int, [lst] is an int list,
            [accum] is an int list.  *)

let rec generate_nums num bound lst accum =
  if List.length lst <= 3 then lst
  else
    if (List.length accum < num) then
        let new_num = Random.int bound in
        if (contains new_num accum) then
          generate_nums num bound lst accum
        else
          if contains new_num lst then
            generate_nums num bound lst (new_num :: accum)
          else generate_nums num bound lst accum
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
  let rp = st.recruit_pool in
  let n = List.length rp in
  picks_from_index (generate_nums 3 n rp []) (rp)

(* [return_next_player] returns an int reflecting the
 * the next player *)

let return_next_player st prev =
  if prev = st.total_players then 1
  else prev+1

(* [change_next_player st ] returns a new state for the next player *)

let change_next_player (st: state) =
  { st with card_drawn = None;
            current_player = return_next_player st (st.current_player);
            available_picks = refresh_available_picks st
  }

(* [change_current_player st p] returns a new state with
   the next player p
 * Use case: skip function
 * requires: p <= state.total_players *)
let change_to_player st i =
  { st with card_drawn = None;
            current_player = i;
            available_picks = refresh_available_picks st}

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
 * Returns a playerstate with player_id_int [i]. *)

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

(* [cardID_to_card id] returns a card corresponding to id
 * requires: [id] is an cardID. *)

let cardID_to_card id =
  id_to_card id cardList

(* [draw_card c st] returns a new state with card [c] drawn
 * requires: [c] is an cardID
             [st] is a state *)

let draw_card (c: cardID) (st: state) =
  let remove_rec_pool_st = remove_card_recruit_pool st c in
  let current_player = find_player st.player_states st.current_player in
  let current_player_deck = (find_player st.player_states st.current_player).player_deck in
  let new_player_deck = c :: current_player_deck in
  let new_player_card_drawn = {current_player with player_deck = new_player_deck} in
  let new_player_states = make_new_states new_player_card_drawn remove_rec_pool_st.player_states [] in
  let new_substate_1 = {remove_rec_pool_st with player_states = new_player_states} in
  let card_drawn = id_to_card c cardList in
  let card_function = card_drawn.abilities in
  let new_substate_2 = card_function new_substate_1 c cardList in
  {new_substate_2 with card_drawn = Some c}

  (*let ps = return_player_state st (st.current_player) in
  let new_ps = (add_card c ps) in
  let changed_ps = change_player_state remove_rec_pool new_ps in
    { changed_ps with card_drawn = Some c }*)

(* [init_player_states n h accum] returns a list of playerstates,
   where n is the number of playerstates, h is the number of human
   players *)

let rec init_player_states n h accum =
  match n with
  | 0 -> accum
  | _ -> begin
    match h with
      | 0 -> let new_ps = (n, (init_player_state n false)) in
        init_player_states (n-1) 0 (new_ps :: accum)
      | _ -> let new_ps = (n, init_player_state n true) in
        init_player_states (n-1) (h-1) (new_ps :: accum)
    end

(* [i_to_j i j accum] returns an int list [i; ...; j-1]
 * requires: i < j *)

let rec i_to_j i j accum =
  if i = j then accum
  else i_to_j i (j-1) (j-1 :: accum)

(* [init_state i h] initialises the state. it takes in [i],
   the number of players, and [h], the number of human players *)

let init_state i h =
  {
     (* description = "";
        sec_description = "" :: []; *)
    total_players = i;
    card_drawn = None;
    current_player = 1;
    (* current_player_id = "Player 1"; *)
    recruit_pool = i_to_j 0 24 [];
    available_picks = generate_nums 3 24 (i_to_j 0 24 []) [];
    player_states = init_player_states i h [];
  }

(* [id_to_card id cl] takes a card id int and returns the card object option
 * associated with it. The inputs are the card id and the card list
 * that represents the card set
 * requires: [id] is a cardID, [cl] is a card list *)

let id_to_card_lst st idl =
  map_id_list idl cardList []

let lookup_card id =
  id_to_card id cardList

let increase_resource st =
  let current_player = find_player st.player_states st.current_player in
  let current_resource = current_player.player_resource in
  let new_resource = current_resource + 1 in
  let new_player_state = {current_player with player_resource = new_resource} in
  let new_player_states = make_new_states new_player_state st.player_states [] in
  {st with player_states = new_player_states}

(* let change_description st str =
  { st with description = str }

let change_sec_description st str =
  { st with sec_description = str } *)
