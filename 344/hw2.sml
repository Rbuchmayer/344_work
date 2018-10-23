(* CSE 341, HW2 Provided Code *)

(* main datatype definition we will use throughout the assignment *)
datatype json =
         Num of real (* real is what SML calls floating point numbers *)
	 | String of string
	 | False
	 | True
	 | Null
	 | Array of json list
	 | Object of (string * json) list

(* some examples of values of type json *)
val json_pi    = Num 3.14159
val json_hello = String "hello"
val json_false = False
val json_array = Array [Num 1.0, String "world", Null]
val json_obj   = Object [("foo", json_pi), ("bar", json_array), ("ok", True)]

(* some provided one-liners that use the standard library and/or some features
   we have not learned yet. (Only) the challenge problem will need more
   standard-library functions. *)

(* dedup : string list -> string list -- it removes duplicates *)
fun dedup xs = ListMergeSort.uniqueSort String.compare xs

(* strcmp : string * string -> order compares strings alphabetically
   where datatype order = LESS | EQUAL | GREATER *)
val strcmp = String.compare

(* convert an int to a real *)
val int_to_real = Real.fromInt

(* absolute value of a real *)
val real_abs = Real.abs

(* convert a real to a string *)
val real_to_string = Real.toString

(* return true if a real is negative : real -> bool *)
val real_is_negative = Real.signBit

       (* We now load 3 files with police data represented as values of type json.
   Each file binds one variable: small_incident_reports (10 reports),
   medium_incident_reports (100 reports), and large_incident_reports
   (1000 reports) respectively.

   However, the large file is commented out for now because it will take
   about 15 seconds to load, which is too long while you are debugging
   earlier problems.  In string format, we have ~10000 records -- if you
   do the challenge problem, you will be able to read in all 10000 quickly --
   it's the "trick" of giving you large SML values that is slow.
	*)

       (* Make SML print a little less while we load a bunch of data. *)
       ; (* this semicolon is important -- it ends the previous binding *)
	 Control.Print.printDepth := 3;
       Control.Print.printLength := 3;

       use "parsed_small_police.sml";
       use "parsed_medium_police.sml";

       (*  uncomment when you are ready to do the problems needing the large report*)
       use "parsed_large_police.sml";

       val large_incident_reports_list =
	   case large_incident_reports of
               Array js => js
	     | _ => raise (Fail "expected large_incident_reports to be an array")
			  

       (* Now make SML print more again so that we can see what we're working with. *)
       ; Control.Print.printDepth := 20;
       Control.Print.printLength := 20;

       (* Returns an Array json of Object jsons, each numbered *)
       fun make_silly_json i =
	   let  fun add (l, i) =
		    case i of
			0 =>  []
		      | _  =>  Object [("n", Num (int_to_real(i))), ("b", True)] :: add(l, (i-1))
	   in 
	       let val l = add([],i)
	       in Array l
	       end
	   end

       (* Returns an option containing the data of the tuple in the list with field k*)
       fun assoc (k, xs) =
	   case xs of
	       [] => NONE
	     | (k1, v1) :: xs' => if k1 = k
				  then SOME v1
				  else assoc (k, xs')
					     
       (* Returns an option containing the data of the Object with field f *)
       fun dot (j, f) =
	   case (j, f) of
	       (Object j ,f) =>  assoc(f, j)
	     | _  => NONE  

       (* Returns a string list holding all the field names of j, if j is an Object*)
       fun one_fields j =
	   case j of
	       Object j =>
	       let
		   fun compute (xs, acc) =
		       case (xs, acc) of
			   ([], acc) => acc
			 | (((a, b) :: xs'), acc)  => compute(xs', a :: acc)
	       in
		   compute(j, [])
	       end
	     | _ => []   

       (* Returns true if the given string list contains no duplicates*)
       fun no_repeats ls =
	   length(ls) = length(dedup(ls))

       (* Returns true if no Object fields are repeated anywhere in json j*)
       fun recursive_no_field_repeats j =
	   case j of
	       Array [] => true
	     | Array (x :: xs') =>
	       recursive_no_field_repeats(x) andalso recursive_no_field_repeats(Array xs')
									       
	     |  Object obj => let fun check (Object ob) =
				      case ob of
					  [] => true
				       |  x :: xs' =>  case x of
							   (a, Object b) =>  no_repeats(one_fields(Object ob))  andalso
									     check(Object b)  andalso check(Object xs')
													   
							 | (a, Array b) =>  no_repeats(one_fields(Object ob))  andalso
									    recursive_no_field_repeats(Array b)  andalso check (Object xs')
															       
							 | _ => no_repeats(one_fields(Object ob))  andalso check(Object xs')
			      in check (Object obj)
			      end
	     | _ =>  true
			 
			 
			 
       (* Returns a (String * int) list holding every string in xs
and its number of occurrences. Raises exn if list is not sorted *)	 
       fun count_occurrences (xs, exn) =
	   case xs of
	       [] => []
	     | x :: xs' => let fun compute (str, count, new_xs, old_xs) =
				   case old_xs of
				       [] => new_xs		 
				     | x :: [] =>
				       let val check = String.compare(str, x)
				       in
					   case check of
					       LESS =>  (x, 1) :: (str, count) :: new_xs
					     | EQUAL  => (str, count + 1) :: new_xs
					     | _  =>  raise exn			    		    
				       end
					   
				     | x :: xs' =>
				       let val check = String.compare(str, x)
				       in
					   case check of
					       LESS =>  compute(x, 1, (str, count):: new_xs, xs')
					     | EQUAL  =>  compute(str, count + 1, new_xs, xs')
					     | _ =>  raise exn
							   
				       end
					   
			   in compute(x, 0, [], xs)
			   end

       (* Returns a string list holding all json string data for any 
json Object in j who's field matches str*)
       fun string_values_for_field (str, j) =
	   case j of
	       [] => []
	     | Object x :: xs' => let val opt = dot(Object x, str)
				  in   case opt of
					   SOME (String i) => i :: string_values_for_field(str, xs') 
					 | _ => string_values_for_field(str, xs')
				  end
	     | _ :: xs' =>  string_values_for_field(str, xs')


       (* histogram and historgram_for_field are provided, but they use your
   count_occurrences and string_values_for_field, so uncomment them
   after doing earlier problems *)

       (* histogram_for_field takes a field name f and a list of objects js and
   returns counts for how often a string is the contents of f in js. *)
						   
       exception SortIsBroken

       fun histogram (xs : string list) : (string * int) list =
	   let
	       fun compare_strings (s1 : string, s2 : string) : bool = s1 > s2

	       val sorted_xs = ListMergeSort.sort compare_strings xs
	       val counts = count_occurrences (sorted_xs,SortIsBroken)

	       fun compare_counts ((s1 : string, n1 : int), (s2 : string, n2 : int)) : bool =
		   n1 < n2 orelse (n1 = n2 andalso s1 < s2)
	   in
	       ListMergeSort.sort compare_counts counts
	   end   

       fun histogram_for_field (f,js) =
	   histogram (string_values_for_field (f, js))
		     
       (* Returns a json list containing all the json Objects in the input list
that have field str1 and data str2 *)
       fun filter_field_value (str1, str2, xs) =
	   case xs of
	       [] => []
	     | Object x :: xs'  => let val data = dot(Object x, str1)
				   in case data of
					  SOME (String i) => if i = str2
							     then Object x :: filter_field_value(str1, str2, xs')
							     else  filter_field_value(str1, str2, xs')
					| _ => filter_field_value(str1, str2, xs') 
				   end
	     | _ :: xs' => filter_field_value(str1, str2, xs')
					     
					     


       val large_event_clearance_description_histogram =
	   histogram_for_field ("event_clearance_description", large_incident_reports_list)

       val large_hundred_block_location_histogram =
	   histogram_for_field("hundred_block_location", large_incident_reports_list)
			      

			      
       ;Control.Print.printDepth := 3;
       Control.Print.printLength := 3;

       val forty_third_and_the_ave_reports =
	   filter_field_value("hundred_block_location",
			      "43XX BLOCK OF UNIVERSITY WAY NE", large_incident_reports_list)

       val forty_third_and_the_ave_clearance_description_histogram =
	   histogram_for_field("event_clearance_description",
			       forty_third_and_the_ave_reports)

       val nineteenth_and_forty_fifth_reports =
	   filter_field_value("hundred_block_location",
			      "45XX BLOCK OF 19TH AVE NE", large_incident_reports_list)

       val nineteenth_and_forty_fifth_event_clearance_description_histogram =
	   histogram_for_field("event_clearance_description",
			       nineteenth_and_forty_fifth_reports)

       ;Control.Print.printDepth := 20;
       Control.Print.printLength := 20;

       (* Returns a string that is made of all the strings in the list
separated by str *)
       fun concat_with (str, xs) =
	   case xs of
	       [] => ""
	     | x :: [] => x 
	     | x :: xs' => x ^ str ^ concat_with (str, xs')

       (* Returns the same string str, but with an extra set of quotes *)
       fun quote_string(str) =
	   "\"" ^ str ^ "\""

       (* Returns the string version of a real number with "-" as negative *)
       fun real_to_string_for_json (r) =
	   if  Real.signBit(r) then "-" ^ Real.toString(Real.abs(r))	
	   else Real.toString(r)

       (* Returns the string version of a json in the correct format *)
       fun json_to_string j =
	   case j of
	       String j => j
	     | Num j => real_to_string_for_json(j)
	     | False => "false"
	     | True  => "true"
	     | Null => "null"
	     | Object ob  => let fun make_list (obj) =
				     case obj of
					 [] => []
				       | (a, b) :: xs' => case b of
							      String b => (quote_string(a) ^ " : " ^ quote_string(b)) :: make_list(xs')  
							    | _ => (quote_string(a) ^ " : " ^ json_to_string(b)) :: make_list(xs') 					     
			     in  "{" ^ concat_with(", ", make_list(ob)) ^ "}"
			     end
	     | Array xs => let fun make_list (jl) =
				   case jl of
				       [] => []
				     | x :: xs' => case x of
						       String x => quote_string(x) :: make_list(xs')
						     | _ =>  json_to_string(x) :: make_list(xs')							   
			   in  "[" ^ concat_with(", ", make_list(xs)) ^ "]"
			   end

