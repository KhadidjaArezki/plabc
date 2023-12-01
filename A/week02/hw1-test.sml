(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw1-sample-solutions.sml";

val test_is_older_1 = is_older((1,2,3),(2,3,4))               = true
val test_is_older_2 = is_older((2022, 10, 20), (2021, 10,20)) = false
val test_is_older_3 = is_older((2021,10,20), (2022,10,20))    = true
val test_is_older_4 = is_older((2022,9,20),(2022,10,20))      = true
val test_is_older_5 = is_older((2022,10,19),(2022,9,20))      = false
val test_is_older_6 = is_older((2022,10,21),(2022,9,20))      = false

val test_number_in_month_1 = number_in_month ([(2012,2,28),(2013,12,1)], 2)  = 1
val test_number_in_month_2 = number_in_month ([(2012,2,28),(2013,2,1)], 2)   = 2
val test_number_in_month_3 = number_in_month ([(2012,11,28),(2013,12,1)], 2) = 0
val test_number_in_month_4 = number_in_month ([(2012,2,28)], 2)              = 1
val test_number_in_month_5 = number_in_month ([(2012,12,28)], 2)             = 0
val test_number_in_month_6 = number_in_month([], 2)                          = 0

val test_number_in_months_1 = number_in_months([], [])                                       = 0
val test_number_in_months_2 = number_in_months([(2011,4,28)], [])                            = 0
val test_number_in_months_3 = number_in_months([(2012,2,28),(2011,4,28)], [])                = 0
val test_number_in_months_4 = number_in_months([], [1])                                      = 0
val test_number_in_months_5 = number_in_months([(2011,4,28)], [1])                           = 0
val test_number_in_months_6 = number_in_months([(2011,1,28)], [1])                           = 1
val test_number_in_months_7 = number_in_months([(2012,2,28),(2011,4,28)], [3])               = 0
val test_number_in_months_8 = number_in_months([(2012,3,28),(2011,4,28)], [3])               = 1
val test_number_in_months_9 = number_in_months([(2012,3,28),(2011,3,28)], [3])               = 2
val test_number_in_months_10 = number_in_months([],[2,3])                                    = 0
val test_number_in_months_11 = number_in_months([(2011,4,31)],[2,3])                         = 0
val test_number_in_months_12 = number_in_months([(2011,2,31)],[2,3])                         = 1
val test_number_in_months_13 = number_in_months([(2012,4,28),(2011,5,31)],[2,3])             = 0
val test_number_in_months_14 = number_in_months([(2012,2,28),(2011,5,31)],[2,3])             = 1
val test_number_in_months_15 = number_in_months([(2012,2,28),(2011,3,31)],[2,3])             = 2
val test_number_in_months_16 = number_in_months([(2012,2,28),(2012,2,28),(2011,3,31)],[2,3]) = 3

val test_dates_in_month_1 = dates_in_month([(2012,2,28),(2013,12,1)],2)  = [(2012,2,28)]
val test_dates_in_month_2 = dates_in_month([(2012,2,28),(2013,2,1)],2)   = [(2012,2,28),(2013,2,1)]
val test_dates_in_month_3 = dates_in_month([(2012,11,28),(2013,12,1)],2) = [] 
val test_dates_in_month_4 = dates_in_month([(2012,2,28)],2)              = [(2012,2,28)]
val test_dates_in_month_5 = dates_in_month([(2012,12,28)], 2)            = []
val test_dates_in_month_6 = dates_in_month([], 2)                        = []

val test_dates_in_months_1  = dates_in_months([], [])                                        = [] 
val test_dates_in_months_2  = dates_in_months([(2011,4,28)], [])                             = []
val test_dates_in_months_3  = dates_in_months([(2012,2,28),(2011,4,28)], [])                 = []
val test_dates_in_months_4  = dates_in_months([], [1])                                       = []                                      
val test_dates_in_months_5  = dates_in_months([(2011,4,28)], [1])                            = []
val test_dates_in_months_6  = dates_in_months([(2011,1,28)], [1])                            = [(2011,1,28)]
val test_dates_in_months_7  = dates_in_months([(2012,2,28),(2011,4,28)], [3])                = []
val test_dates_in_months_8  = dates_in_months([(2012,3,28),(2011,4,28)], [3])                = [(2012,3,28)]
val test_dates_in_months_9  = dates_in_months([(2012,3,28),(2011,3,28)], [3])                = [(2012,3,28),(2011,3,28)]
val test_dates_in_months_10 = dates_in_months([],[2,3])                                      = []                                   
val test_dates_in_months_11 = dates_in_months([(2011,4,31)],[2,3])                           = []
val test_dates_in_months_12 = dates_in_months([(2011,2,31)],[2,3])                           = [(2011,2,31)]
val test_dates_in_months_13 = dates_in_months([(2012,4,28),(2011,5,31)],[2,3])               = [] 
val test_dates_in_months_14 = dates_in_months([(2012,2,28),(2011,5,31)],[2,3])               = [(2012,2,28)]
val test_dates_in_months_15 = dates_in_months([(2012,2,28),(2011,3,31)],[2,3])               = [(2012,2,28),(2011,3,31)]
val test_dates_in_months_16 = dates_in_months([(2012,2,28),(2012,2,28),(2011,3,31)],[2,3])   = [(2012,2,28),(2012,2,28),(2011,3,31)]

val test_get_nth_1 = get_nth(["hi"], 1)          = "hi"
val test_get_nth_2 = get_nth(["hi", "there"], 1) = "hi"
val test_get_nth_3 = get_nth(["hi", "there"], 2) = "there"

val test_date_to_string_1 = date_to_string (2013, 6, 30)  = "June 30, 2013"
val test_date_to_string_2 = date_to_string (2013, 1, 1)   = "January 1, 2013"
val test_date_to_string_3 = date_to_string (2013, 12, 10) = "December 10, 2013"

val test_number_before_reaching_sum_1  = number_before_reaching_sum (10, [1,2,3,4,5])    = 3
val test_number_before_reaching_sum_2  = number_before_reaching_sum (10, [])             = 0
val test_number_before_reaching_sum_3  = number_before_reaching_sum (10, [2])            = 1
val test_number_before_reaching_sum_4  = number_before_reaching_sum (10, [1,2])          = 2
val test_number_before_reaching_sum_5  = number_before_reaching_sum (10, [11,2])         = 0
val test_number_before_reaching_sum_6  = number_before_reaching_sum (10, [9,2])          = 1

val test_what_month_1 = what_month 70  = 3
val test_what_month_2 = what_month 1   = 1
val test_what_month_3 = what_month 31  = 1
val test_what_month_4 = what_month 32  = 2
val test_what_month_5 = what_month 59  = 2
val test_what_month_6 = what_month 365 = 12
val test_what_month_7 = what_month 334 = 11

val test_month_range_1 = month_range (31, 34) = [1,2,2,2]
val test_month_range_2 = month_range (31, 31) = [1]
val test_month_range_3 = month_range (32, 31) = []

val test_oldest_1 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test_oldest_2 = oldest([])                                    = NONE
val test_oldest_3 = oldest([(2011,3,31)])                         = SOME (2011,3,31)

(* val test_is_found_1 = is_found (2, [1,3])    = true *)
(* val test_is_found_2 = is_found (2, [1,2,3])  = false *)
(* val test_is_found_3 = is_found (2, [])       = true *)
(*  *)
(* val test_remove_duplicates_from_list_1 = remove_duplicates_from_list ([1,2,3])   = [1,2,3] *)
(* val test_remove_duplicates_from_list_2 = remove_duplicates_from_list ([1,2,2,3]) = [1,2,3] *)
(* val test_remove_duplicates_from_list_3 = remove_duplicates_from_list ([])        = [] *)
(* val test_remove_duplicates_from_list_4 = remove_duplicates_from_list ([2])       = [2] *)

val test_number_in_months_challenge_1 = number_in_months_challenge ([(2012,2,28),(2012,2,28),(2011,3,31)],[2,3]) = 3
val test_number_in_months_challenge_2 = number_in_months_challenge ([(2012,2,28),(2012,2,28),(2011,3,31)],[2,2,3]) = 3

val test_dates_in_months_challenge_1 = dates_in_months_challenge ([(2012,2,28),(2012,2,28),(2011,3,31)],[2,3])   = [(2012,2,28),(2012,2,28),(2011,3,31)]
val test_dates_in_months_challenge_2 = dates_in_months_challenge ([(2012,2,28),(2012,2,28),(2011,3,31)],[2,2,3]) = [(2012,2,28),(2012,2,28),(2011,3,31)]

val test_reasonale_date_1 = reasonable_date (2020,2,29)  = true
val test_reasonale_date_2 = reasonable_date (2021,2,29)  = false
val test_reasonale_date_3 = reasonable_date (2021,2,28)  = true
val test_reasonale_date_4 = reasonable_date (0,2,29)     = false
val test_reasonale_date_5 = reasonable_date (2021,13,29) = false
val test_reasonale_date_6 = reasonable_date (2021,0,29)  = false
val test_reasonale_date_7 = reasonable_date (2021,2,0)   = false
val test_reasonale_date_8 = reasonable_date (2021,4,31)  = false

(*

val test5 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"

val test7 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3

val test9 = what_month 70 = 3

val test10 = month_range (31, 34) = [1,2,2,2]

val test11 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)

*)
