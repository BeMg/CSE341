(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

			       
fun only_capitals str =
  let
      fun capital s =
	Char.isUpper (String.sub(s, 0));
  in
      List.filter capital str
  end

fun longest_string1 str =
  let
      fun select_longer (x, acc) =
	if String.size(acc) >= String.size(x)
	then acc
	else x
  in
      List.foldl select_longer "" str
  end


fun longest_string2 str =
  let
      fun select_longer (x, acc) =
	if String.size(acc) > String.size(x)
	then acc
	else x
  in
      List.foldl select_longer "" str
  end

fun longest_string_helper f str =
  let
      fun select_longer (x, acc) =
	if f(String.size(acc), String.size(x))
	then acc
	else x
  in
      List.foldl select_longer "" str
  end

fun longest_string3 str =
  longest_string_helper (fn (x,y) => x>=y) str

fun longest_string4 str =
  longest_string_helper (fn (x,y)=> x>y) str

fun longest_capitalized str =
  longest_string1 (only_capitals str)

fun rev_string str =
  String.implode (List.rev (String.explode str))
		 
fun first_answer f inp =
  case inp of
      [] => raise NoAnswer
    | x::y => case f(x) of
		  SOME a => a
		| _ => first_answer f y
				    
fun all_answers f inp =
  let
      fun foo (x, acc) =
	case f(x) of
	    NONE => acc
	  | SOME y => acc @ y
  in
      case inp of
	  [] => SOME []
	| _ => case SOME (List.foldl foo [] inp) of
		   SOME [] => NONE
		 | y => y
  end

      
fun count_wildcards p =
  g (fn x => 1) (fn x => String.size(x)) p

fun count_wild_and_variable_lengths p =
  g (fn x => 1) (fn x => String.size(x)) p

fun count_some_var (str, p) =
  g (fn x => 1) (fn x => if x = str then 1 else 0) p

fun check_pat p =
  let
      fun getVar p =
	case p of
	    Variable x => [x]
	  | Wildcard => []
	  | TupleP ps => List.foldl (fn (x, acc) => getVar(x) @ acc) [] ps
	  | ConstructorP(_, p) => getVar(p)
	  | _ => []
      fun check s1 =
	let
	    fun checkred s s2 =
	      case s of
		  [] => true
		| x::y => if (List.foldl (fn (z, acc) => if z=x then 1+acc else 0+acc) 0 s2)=1
			  then checkred y s2
			  else false
	in
	    checkred s1 s1
	end
  in
      check (getVar p)
  end


fun match (v, p) =
  case p of
      Wildcard => SOME []
    | Variable s => SOME [(s,v)]
    | UnitP => if v=Unit then SOME [] else NONE
    | ConstP n => if v=Const(n) then SOME [] else NONE
    | TupleP ps =>
      let
	  val Tuple(vs) = v
      in
	  all_answers match (ListPair.zip(vs, ps))
      end
    | ConstructorP(s1, p2) => let val Constructor(s2, v2) = v
			      in
				  if s1=s2
				  then match (v2, p2)
				  else NONE
			      end
fun first_match v ps =
  SOME (first_answer (fn p => match(v,p)) ps)
  handle NoAnswer => NONE
      
      
