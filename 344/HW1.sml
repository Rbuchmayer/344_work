fun is_older (tp1 : (int * int * int), tp2 : (int * int * int)) =
    if #3 tp1 < #3 tp2
    then true
    else
	if #1 tp1 < #1 tp2 andalso #3 tp1 = #3 tp2
	then true
	else
	    if #2 tp1 < #2 tp2 andalso #1 tp1 = #1 tp2 andalso #3 tp1 = #3 tp2
	    then true
	    else
		false
		    
fun number_in_month (l : (int * int * int) list, month : int) =
    if null l
    then 0
    else
	if #1 (hd l)  = month
	then 1 + number_in_month(tl l, month)
	else 0 + number_in_month(tl l, month)

fun number_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then 0
    else number_in_month(dates, hd months) + number_in_months(dates, tl months)
							     

fun dates_in_month (dates : (int * int * int) list, month : int) =
    if null dates
    then []
    else
	if #1 (hd dates) = month
	then (hd dates) :: dates_in_month(tl dates, month)
	else dates_in_month((tl dates), month)
			   

fun dates_in_months (dates : (int * int * int) list, months : int list) =
    if null months
    then []
    else dates_in_month(dates, (hd months)) @ dates_in_months(dates, (tl months))
							     
							     
fun get_nth (strings : string list, n : int) =
    if n = 1
    then hd strings
    else get_nth((tl strings), n - 1)

val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]


fun date_to_string (date : (int * int * int)) =
    get_nth(months, (#1 date)) ^ "-" ^ Int.toString(#2 date) ^ "-" ^ Int.toString(#3 date)
										 

fun number_before_reaching_sum (sum : int, l : int list) =

    let
	fun compute (i : int, sum : int, l : int list) =
	    if sum - (hd l) <= 0
	    then i - 1
	    else compute (i + 1, sum - (hd l), (tl l))
    in
	compute (1, sum, l)
    end

val month_list = [31,28,31,30,31,30,31,31,30,31,30,31]
		     
fun what_month (day : int) =
    number_before_reaching_sum (day, month_list) + 1
						       
fun month_range (day1 : int, day2 : int) =
    if day1 > day2
    then []
    else
	let
	    fun count (month1 : int, month2 : int) =
		if month1 = month2
		then [month2]
		else month1 :: count(month1 + 1, month2)
	in
	    count(what_month(day1), what_month(day2))
	end
	    
fun oldest (dates : (int * int * int) list) =
    if null dates
    then NONE
    else
	let
	    fun find_oldest (dates : (int * int * int) list) =
		if null (tl dates)
		then hd dates
		else
		    let  val x = find_oldest (tl dates)
			 val y = (hd dates)
		    in if is_older (y, x)
		       then y
		       else x
		    end
	in
	    SOME (find_oldest(dates))
	end

fun cumulative_sum (nums : int list) =
    if null nums
    then []
    else
	let
	    fun compute (nums : int list, cur : int) =
		if null (tl nums)
		then cur + (hd nums) :: []
		else cur + (hd nums) :: compute(tl nums, (hd nums) + cur)    
	in
	    compute(nums, 0)
	end




fun remove_dups (nums : int list) =
    if null nums
    then []
    else
	let
	    fun contains (nums : int list, n : int) =
		if null nums
		then false
		else
		    if (hd nums) = n
		    then true
		    else contains ((tl nums), n)
	in
	    if contains((tl nums), (hd nums))
	    then remove_dups ((tl nums))
	    else (hd nums) :: remove_dups ((tl nums))
	end
	    
	    
fun number_in_months_challenge (dates : (int * int * int) list, months : int list) =
    number_in_months(dates, remove_dups(months))

fun dates_in_months_challenge (dates : (int * int * int) list, months : int list) =
    dates_in_months(dates, remove_dups(months))

fun is_leap_year (date : (int * int * int)) =
    ((#3 date) mod 400 = 0) orelse ((#3 date) mod 4 = 0 andalso (#3 date) mod 100 <> 0 )

fun reasonable_date (date : (int * int * int)) =
    if is_leap_year (date) andalso #1 date = 2
    then (#3 date > 0) andalso (#2 date > 0 andalso #2 date < 30)
    else
	let
	    fun get_nth_int (l : int list, n : int) =
		if n = 1
		then hd l
		else get_nth_int((tl l), n - 1)
	in
	    (#3 date > 0) andalso (#1 date >= 1) andalso (#1 date <= 12) andalso (#2 date > 0) andalso (#2 date <= get_nth_int(month_list, #1 date))
	end


fun f (xs,ys) =
case (xs,ys) of
(* 1 *) ([],[]) => SOME 0
(* 2 *) | (x::[], y::[]) => SOME (x+y)
(* 3 *) | (x1::x2::[], y1::y2::[]) => SOME (x1 + x2 + y1 + y2)
(* 4 *) | (x1::x2::xs’, y1::y2::ys’) => f (xs’,ys’)
(* 5 *) | _ => NONE
