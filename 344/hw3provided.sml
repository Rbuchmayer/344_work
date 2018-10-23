(* Dan Grossman, CSE341, HW3 Provided Code *)

exception NoAnswer

datatype pattern = WildcardP
                 | VariableP of string
                 | UnitP
                 | ConstantP of int
                 | ConstructorP of string * pattern
                 | TupleP of pattern list

datatype valu = Constant of int
              | Unit
              | Constructor of string * valu
              | Tuple of valu list

fun g f1 f2 p =
    let 
        val r = g f1 f2 
    in
        case p of
            WildcardP         => f1 ()
          | VariableP x       => f2 x
          | ConstructorP(_,p) => r p
          | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
          | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = AnythingT
             | UnitT
             | IntT
             | TupleT of typ list
             | DatatypeT of string


(* Returns a string list containing only the strings of str that begin 
with a lower case letter *)
fun only_lowercase ls =  List.filter (fn str => 
					 Char.isLower(String.sub(str,0))) ls

(* Returns the longest string in list ls. Returns "" if 
ls is empty. Ties go to the earlier string in the list *)
fun longest_string1 ls = List.foldl (fn (a, b) =>
					if  String.size(b) >= String.size(a) then b else a) "" ls

(* Returns the longest string in list ls. Returns "" if 
ls is empty. Ties go to the later string in the list *)
fun longest_string2 ls = List.foldl (fn (a, b) =>
					if  String.size(b) > String.size(a) then b else a) "" ls

(* Takes a function and applies it to a list in a higher order function *)				    
fun longest_string_helper f = List.foldl (fn (a, b) =>
					     if f(String.size(a), String.size(b)) then b else a) "" 

(* Returns the longest string in list ls. Returns "" if 
ls is empty. Ties go to the earlier string in the list *)				
val longest_string3 = longest_string_helper (fn (a, b) =>
						if b >= a then true else false)
					    
(* Returns the longest string in list ls. Returns "" if 
ls is empty. Ties go to the later string in the list *)
val longest_string4 = longest_string_helper (fn (a, b) =>
						if b > a then true else false)

(* Returnst the longest string is a given list that begins 
with a lowercase letter. Returns "" if there is no such string *)			
val longest_lowercase  = longest_string1 o only_lowercase   

(* Returns the given string in all upper case and all "x"s and 
"X"s are removed *)
val caps_no_X_string  = String.implode o  List.filter
					      (fn x => x <> String.sub("X", 0)) o List.map (fn x => Char.toUpper x) o String.explode

(* Applies f to all elements of xs and returns the first SOME v.
Returns NONE if f returns NONE for all elements of xs. *) 
fun first_answer f xs = case xs of
			    [] => raise NoAnswer
			  | x :: xs' => case (f x) of
					    SOME v => v
					  | NONE => first_answer f xs'

(*Applies f to all elements of xs and returns a list option of all the outputs.
 Returns NONE if any output is NONE. *)							 
fun all_answers f xs = 		   
    let fun aux (acc, ls) =
	    case ls of
		[] => SOME acc
	      | x :: xs' =>  case (f x) of
				 SOME v => aux(v @ acc, xs')
			       | NONE => NONE
					     
    in aux([], xs)
    end

(* Function g takes two functions and a pattern. It returns an int whose 
value depends on the functions f1 and f2. f1 is applied to all WildcardPs 
in pattern p, while f2 is applied to all VaraiblesPs in pattern p. 
Function g can be used to check many characteristics of a pattern given 
specific functions f1 and f2. *)

(* returns the number of WildcardPs in a patter *)
val count_wildcards = g (fn x => 1) (fn x => 0)

(* Returns the number of WildcardPs plus the sum of all the VariableP
string lengths. *)			
val count_wild_and_variable_lengths = g (fn x => 1) (fn s => String.size s) 

(* Returns the number of times a given string appears as VariableP
strings in a given pattern. *)					
fun count_a_var (s, p) = g (fn x => 0) (fn x => if s = x then 1 else 0) p

(* Returns true if all the VariableP strings in the given pattern are unique *)		
fun check_pat p = 
    let fun make_list p = 
            case p of
		TupleP xs  => List.foldl (fn (a,b) => make_list a @ b) [] xs
	      | ConstructorP (_, pat) => make_list pat
	      | VariableP x => [x] 
              | _ => []
			 
	fun is_unique xs =
	    case xs of
		[] => true
              | x :: xs' => if List.exists(fn y => x = y ) xs'
			    then false
			    else is_unique xs'	      
    in
	is_unique(make_list p)
    end 

(* Returns a list option of all the bindings if the pattern and value match.
Returns NONE if they do not match. *)	
fun match (v, p) =
    case (v, p) of
	(_, WildcardP) => SOME []
      | (_, VariableP s) => SOME [(s, v)]
      | (Unit, UnitP) => SOME []
      | (Constant x, ConstantP y) => if x = y then SOME [] else NONE
      | (Constructor(s2, w), ConstructorP(s1, x))  => if s1 = s2
						      then match (w, x) else NONE			      
      | (Tuple vs, TupleP ps) => if List.length vs = List.length ps
				 then	 let val check = all_answers match(ListPair.zip(vs, ps))
					 in case check of
						SOME z => SOME z
					      | NONE => NONE
					 end
				 else NONE
      | _  => NONE 

(* Takes a value and a pattern list. Returns a list option of the first pattern
in the list that matches the value. *) 
fun first_match v ps =
    SOME (first_answer (fn x => match(v, x)) ps)
    handle NoAnswer => NONE
