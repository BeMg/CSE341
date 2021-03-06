fun is_older(x: (int * int * int), y: (int * int * int)) =
  if (#1 x > #1 y)
     orelse (#1 x = #1 y andalso #2 x > #2 y)
     orelse (#1 x = #1 y andalso #2 x = #2 y andalso #3 x > #3 y)
     orelse x = y
  then false
  else true;

fun number_in_month(date: (int * int * int) list, m: int) =
  if null date
  then 0
  else
      if (#2 (hd date))=m
      then 1 + number_in_month(tl date, m)
      else number_in_month(tl date, m);

fun number_in_months(date: (int * int * int) list, m: int list) =
  if null m
  then 0
  else number_in_month(date, hd m) + number_in_months(date, tl m);

fun dates_in_month(date: (int * int * int) list, m: int) =
  if null date
  then []
  else
      let
	  val tmp = hd date
      in
	  if #2 tmp = m
	  then tmp :: dates_in_month(tl date, m)
	  else dates_in_month(tl date, m)
      end;

fun dates_in_months(date: (int * int * int) list, m: int list) =
  if null m
  then []
  else
      let
	  val tmp = hd m
      in
	  dates_in_month(date, tmp) @ dates_in_months(date, tl m)
      end;

fun get_nth(str: string list, d: int) =
  if null str
  then ""
  else
      if d=1
      then hd str
      else get_nth(tl str, d-1);

fun date_to_string(y: int, m: int, d: int) =
  let
      val month_string = ["January", "February", "March", "April",
			  "May", "June", "July", "August", "September", "October", "November", "December"]
  in
      get_nth(month_string, m) ^ " " ^ Int.toString(d) ^ ", " ^ Int.toString(y)
  end;

fun number_before_reaching_sum(x: int, y: int list) =
  if x < 0 orelse x = 0
  then ~1
  else 1 + number_before_reaching_sum(x - (hd y), tl y);

fun what_month(x: int) =
  let
      val month_date = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
  in
      1 + number_before_reaching_sum(x, month_date)
  end;

fun month_range(x: int, y: int) =
  if x > y
  then []
  else what_month(x) :: month_range(x+1, y);

fun oldest(date: (int * int * int) list) =
  if null date
  then NONE
  else
      let
	  fun foo(date: (int * int * int) list, ans: (int * int * int)) =
	    if null date
	    then SOME ans
	    else
		if is_older(hd date, ans)
		then foo(tl date, hd date)
		else foo(tl date, ans)
      in
	  foo(tl date, hd date)
      end;

