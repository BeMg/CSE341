(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option (patten, target) =
  let
      fun check (patten, target) =
	case (patten, target) of
	    (p, []) => false
	  | (p, t::other) => if same_string(p, t)
			     then true
			     else check(p, other);
      fun get (patten, target) =
	case (patten, target) of
	    (p, []) => []
	  | (p, t::other) => if same_string(p, t)
			     then other
			     else t::get(p, other);
  in
      if check(patten, target)
      then SOME (get(patten, target))
      else NONE
  end
      
fun get_substitutions1 (s, patten) =
  case (s, patten) of
      ([], p) => []
    | (t::other, p) => case all_except_option(p, t) of
			   NONE => get_substitutions1(other, p)
			 | SOME res => res @ get_substitutions1(other, p)

fun get_substitutions2 (s, patten) =
  let
      fun subf(s, patten, ans) =
	case (s, patten, ans) of
	    ([], p, ans) => ans
	  | (t::other, p, ans) => case all_except_option(p, t) of
				      NONE => subf(other, p, ans)
				    | SOME res => subf(other, p, res @ ans)
  in
      subf(s, patten, [])
  end

fun similar_names (str, name) =
  let
      val {first=x, middle=y, last=z} = name
      fun getName (str, name, order) =
	let
	    val {first=x, middle=y, last=z} = name
	in
	    case (str, name, order) of
		([], name, _) => []
	      | (t::other, name, 1) => {first=t, middle=y, last=z}::getName(other, name, 1)
	      | (t::other, name, 2) => {first=x, middle=t, last=z}::getName(other, name, 2)
	      | (t::other, name, 3) => {first=x, middle=y, last=t}::getName(other, name, 3)
	end
  in
      name :: (getName(get_substitutions1(str, x), name, 1) @
	       getName(get_substitutions1(str, y), name, 2) @
	       getName(get_substitutions1(str, z), name, 3))
  end
      
(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (c: card) =
  case c of
      (Clubs, _) => Black
    | (Spades, _) => Black
    | (_, _) => Red

fun card_value (c: card) =
  case c of
      (_, Num i) => i
    | (_, Jack) => 10
    | (_, Queen) => 10
    | (_, King) => 10
    | (_, Ace) => 11

fun remove_card (cs: card list, c: card, e) =
  case (cs, c) of
      ([], c) => raise e
    | (t::other, c) => if t = c
		       then other
		       else t::remove_card(other, c, e)

fun all_same_color (cs: card list) =
  case cs of
      [] => true
   |  c::[] => true
   |  c1::c2::other => if card_color(c1) = card_color(c2)
		       then all_same_color(c2::other)
		       else false

fun sum_cards (cs: card list) =
  let
      fun fsum (cs, ans) =
	case cs of
	    [] => ans
	  | (t::other) => fsum(other, ans+card_value(t))
  in
      fsum(cs, 0)
  end

fun score (cs: card list, goal) =
  let
      val sum = sum_cards(cs)
      val div_num = if all_same_color(cs)
		    then 2
		    else 1
  in
      if sum > goal
      then 3 * (sum - goal) div div_num
      else (goal - sum) div div_num
  end

fun officiate (cs: card list, moves: move list, goal) =
  let
      fun game_run(cs: card list, hcs: card list, moves: move list, goal) =
	case (cs, hcs, moves, goal) of
	    (cs, hcs, [], goal) => score(hcs, goal)
	  | (cs, hcs, t::other, goal) =>
	    case (cs, t) of
		([], Draw) => score(hcs, goal)
	      | (c::css, Draw) => game_run(css, c::hcs, other, goal)
	      | (_, Discard(c)) => game_run(cs, remove_card(hcs, c, IllegalMove), other, goal) 
  in
      game_run(cs, [], moves, goal)
  end
