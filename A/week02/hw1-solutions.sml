(* PLA Homework 01 Solutions  *)
(* Author: Khadidja Arezki *)

(**************************************************************)

(* Exercise 01 *)

fun is_older(date1: int*int*int, date2: int*int*int) =
  if #1 date1 < #1 date2 then true
  else
    if #1 date1 = #1 date2 then
      if #2 date1 < #2 date2 then true
      else
        if #2 date1 = #2 date2 then
          #3 date1 < #3 date2
        else false
    else false

(**************************************************************)

(* Exercise 02 *)

fun number_in_month(ds: (int*int*int) list, month: int) =
  if null ds then 0
  else
    let val number_in_tl = number_in_month(tl ds, month)
    in 
      if #2 (hd ds) = month
      then 1 + number_in_tl 
      else number_in_tl
    end

(**************************************************************)

(* Exercise 03 *)

fun number_in_months(ds: (int*int*int) list, ms: int list) =
  if null ms then 0
  else number_in_month(ds, hd ms) + number_in_months(ds, tl ms)

(**************************************************************)

(* Exercise 04 *)

fun dates_in_month(ds: (int*int*int) list, month: int) = 
  if null ds then []
  else
    let val dates_in_tl = dates_in_month(tl ds, month)
        val fst_date    = hd ds
    in 
      if #2 (fst_date) = month
      then  fst_date :: dates_in_tl 
      else dates_in_tl
    end

(**************************************************************)

(* Exercise 05 *)

fun dates_in_months(ds: (int*int*int) list, ms: int list) =
  if null ms then []
  else dates_in_month(ds, hd ms) @ dates_in_months(ds, tl ms)

(**************************************************************)

(* Exercise 06 *)

fun get_nth(ss: string list, n: int) =
  if n <= 1 then hd ss
  else get_nth(tl ss, n-1)

(**************************************************************)

(* Exercise 07 *)

fun date_to_string(date: int*int*int) = 
  let val months = ["January", "February", "March", "April",
                    "May", "June", "July", "August", "September", 
                    "October", "November", "December"]
      val month = get_nth(months, (#2 date))
      val day   = Int.toString (#3 date)
      val year  = Int.toString (#1 date)
  in
   month ^ " " ^ day ^ ", " ^ year
  end

(**************************************************************)

(* Exercise 08 *)

fun number_before_reaching_sum(sum: int, ns: int list) =
  if null ns then 0
  else
    let
      fun number_before_reaching_sum_helper (tempSum: int, ns_rest: int list) =
        if null ns_rest then
          if tempSum < sum then 1 
          else 0
        else
          if tempSum  >= sum then 0
          else 1 + number_before_reaching_sum_helper ((tempSum + hd ns_rest) , (tl ns_rest))
    in
      number_before_reaching_sum_helper (hd ns, (tl ns))
    end

(**************************************************************)

(* Exercise 09 *)

fun what_month (day: int) =
  let val months = [1,2,3,4,5,6,7,8,9,10,11,12]
  
      fun days_in_month (month: int) =
      if month = 2 then 28
      else 
        if month >= 1 andalso month <= 7 then
          if month mod 2 = 0 then 30
          else 31
        else
          if month mod 2 = 0 then 31
          else 30

      fun days_in_all_months (ms: int list) =
        if null ms then []
        else days_in_month (hd ms) :: days_in_all_months (tl ms)

  in
    1 + number_before_reaching_sum (day, days_in_all_months months)
  end

(**************************************************************)

(* Exercise 10 *)

fun month_range (day1: int, day2: int) =
  if day1 > day2 then []  
  else what_month day1 :: month_range (day1 + 1, day2)

(**************************************************************)

(* Exercise 11 *)

fun oldest (ds: (int*int*int) list) =
  let 
    fun oldest_helper (oldest_date: int*int*int, ds_rest: (int*int*int) list) =
      if null ds_rest then SOME oldest_date
      else 
        let val current_date = hd ds_rest
            val ds_rest_rest = tl ds_rest
        in 
          if is_older (current_date, oldest_date) then oldest_helper (current_date, ds_rest_rest)
          else oldest_helper (oldest_date, ds_rest_rest)
        end

  in 
    if null ds then NONE
    else
      oldest_helper (hd ds, tl ds)
  end

(**************************************************************)

(* Exercise 12 *)

fun is_found (n: int, ns: int list) =
  if null ns then true
  else 
    if n = hd ns then false
    else is_found (n, tl ns)

fun remove_duplicates_from_list (ms: int list) =
  if null ms then []
  else
    let 
      val hd_ms = hd ms
      val tl_ms = tl ms
      val unique_ms_rest = remove_duplicates_from_list(tl_ms)
    in
      if is_found (hd_ms, tl_ms) then hd_ms :: unique_ms_rest
      else unique_ms_rest
    end

fun number_in_months_challenge (ds: (int*int*int) list, ms: int list) =
  number_in_months (ds, (remove_duplicates_from_list ms))

fun dates_in_months_challenge (ds: (int*int*int) list, ms: int list) = 
  dates_in_months(ds, remove_duplicates_from_list ms)

(**************************************************************)

(* Exercise 13 *)

fun reasonable_date (date: int*int*int) =
  let
    fun days_in_month (month: int, year: int) =
      if month = 2 then
        if (year mod 400 = 0) orelse ((year mod 4 = 0) andalso (year mod 100 <> 0)) then 29
        else 28
      else 
        if month >= 1 andalso month <= 7 then
          if month mod 2 = 0 then 30
          else 31
        else
          if month mod 2 = 0 then 31
          else 30
  in
    if #1 date >= 1 then
      if #2 date >= 1 andalso #2 date <= 12 then
        if #3 date >= 1 andalso #3 date <= days_in_month (#2 date, #1 date) then true
        else false
      else false
    else false
  end

(**************************************************************)