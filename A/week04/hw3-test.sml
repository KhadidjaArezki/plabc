(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw3-provided.sml";

val test1 = only_capitals ["A","B","C"] = ["A","B","C"]

val test2 = longest_string1 ["A","B","C"] = "A"

val test3 = longest_string2 ["A","B","C"] = "C"

val test4a = longest_string3 ["A","B","C"] = "A"

val test4b = longest_string4 ["A","B","C"] = "C"

val test5 = longest_capitalized ["A","bc","C"] = "A"

val test6 = rev_string "abc" = "cba"

val test7 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4

val test_all_answers_1 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test_all_answers_2 = all_answers (fn x => if x = "Algiers" then SOME [x] else NONE)
        ["Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut",
        "Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas",
        "Kentucky","Louisiana","Maine","Maryland","massachusetts","Michigan","Minnesota",
        "Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey2",
        "New Mexico","New York","NorthCarolina","North Dakota","Ohio","Oklahoma","Oregon",
        "Pennsylvania","Rhode Island","southCarolina","South Dakota","Tennessee","Texas",
        "Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"]       = NONE

val test9a = count_wildcards Wildcard = 1

val test9b = count_wild_and_variable_lengths (Variable("a")) = 1

val test9c = count_some_var ("x", Variable("x")) = 1

val test_check_pat_1 = check_pat (Variable("x"))                                                             = true
val test_check_pat_2 = check_pat (TupleP[Variable "x",Variable "x"])                                         = false
val test_check_pat_3 = check_pat (ConstructorP ("egg",ConstructorP ("egg",ConstP 4)))                        = true
val test_check_pat_4 = check_pat (TupleP[Variable "x",ConstructorP ("wild",Wildcard)])                       = true
val test_check_pat_5 = check_pat (TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Wildcard])      = true
val test_check_pat_6 = check_pat (TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),     
                                    ConstructorP ("egg",ConstructorP ("egg",ConstP 4))])                     = true
val test_check_pat_7 = check_pat (TupleP[Wildcard,Wildcard])                                                 = true
val test_check_pat_8 = check_pat (TupleP[ConstP 4,Wildcard,Variable "ba",TupleP[Variable "ab"]])             = true
val test_check_pat_9 = check_pat (TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)])       = true
val test_check_pat_10 = check_pat (ConstructorP ("hi",TupleP[Variable "x",Variable "x"]))                    = false
val test_check_pat_11 = check_pat (TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],Variable "x"]) = false
val test_check_pat_12 = check_pat (TupleP[TupleP[TupleP[Variable "x",ConstructorP ("wild",Wildcard)],
                                Wildcard],Variable "x"])                                                     = false
val test_check_pat_13 = check_pat (ConstructorP ("hi",TupleP[Variable "x",
                                ConstructorP ("yo",TupleP[Variable "x",UnitP])]))                            = false
val test_check_pat_14 = check_pat (TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),
            ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,ConstP 4,
            ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],
            TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),
            ConstructorP ("egg",ConstP 4)]])                                                                 = true

val test_match_1 = match (Const(1), UnitP)                                                                 = NONE
val test_match_2 = match (Const 17,ConstP 4)                                                               = NONE
val test_match_3 = match ((Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),                      
        Constructor ("egg",Constructor ("egg",Const 4))],TupleP[Wildcard,Wildcard]))                       = NONE
val test_match_4 = match ((Constructor ("egg",Const 4),ConstructorP ("egg",ConstP 4)))                     = SOME []
val test_match_5 = match ((Constructor ("egg",Constructor ("egg",Const 4)),                    
                        ConstructorP ("egg",ConstructorP ("egg",ConstP 4))))                               = SOME []
val test_match_6 = match ((Constructor ("egg",Constructor ("egg",Const 4)),ConstructorP ("egg",ConstP 4))) = NONE
val test_match_7 = match ((Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),
        Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,
        Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],
        Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),
        Constructor ("egg",Const 4)]],TupleP[ConstP 17,Wildcard,ConstP 4,
        ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),
        TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),
        ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],
        TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]))  = SOME []

val test_first_match_1 = first_match Unit [UnitP]                                                  = SOME []
val test_first_match_2 = first_match (Constructor ("egg",Const 4)) [ConstructorP ("egg",ConstP 4)] = SOME []
val test_first_match_3 = first_match (Tuple[Const 17,Unit,Const 4,Constructor ("egg",Const 4),
      Constructor ("egg",Constructor ("egg",Const 4)),Tuple[Const 17,Unit,Const 4,
      Constructor ("egg",Const 4),Constructor ("egg",Constructor ("egg",Const 4))],
      Tuple[Unit,Unit],Tuple[Const 17,Const 4],Tuple[Constructor ("egg",Const 4),
      Constructor ("egg",Const 4)]])
      [ConstP 17,ConstP 4,ConstructorP ("egg",ConstP 4),
      ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),TupleP[ConstP 17,Wildcard,
      ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4))],
      TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],TupleP[ConstructorP ("egg",ConstP 4),
      ConstructorP ("egg",ConstP 4)],TupleP[ConstP 17,Wildcard,ConstP 4,
      ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstructorP ("egg",ConstP 4)),
      TupleP[ConstP 17,Wildcard,ConstP 4,ConstructorP ("egg",ConstP 4),ConstructorP ("egg",
      ConstructorP ("egg",ConstP 4))],TupleP[Wildcard,Wildcard],TupleP[ConstP 17,ConstP 4],
      TupleP[ConstructorP ("egg",ConstP 4),ConstructorP ("egg",ConstP 4)]]]                       = SOME []
