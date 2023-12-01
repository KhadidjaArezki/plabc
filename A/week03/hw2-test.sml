(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

use "hw2-provided.sml";

val test_all_except_option_1  = all_except_option ("string", ["string"])                  = SOME []
val test_all_except_option_2  = all_except_option ("", ["string"])                        = NONE
val test_all_except_option_3  = all_except_option ("", [])                                = NONE
val test_all_except_option_4  = all_except_option ("string", [])                          = NONE
val test_all_except_option_5  = all_except_option ("string", ["string", "anotherString"]) = SOME ["anotherString"]
val test_all_except_option_6  = all_except_option ("", ["string", "anotherString"])       = NONE
val test_all_except_option_7  = all_except_option ("string", ["anotherString"])           = NONE

val test_get_substitutions1_1 = get_substitutions1 ([["foo"],["there"]], "foo")              = []
val test_get_substitutions1_2 = get_substitutions1 ([["foo", "bar"],["there"]], "foo")       = ["bar"]
val test_get_substitutions1_3 = get_substitutions1 ([["foo", "bar"],["foo","there"]], "foo") = ["bar", "there"]
val test_get_substitutions1_4 = get_substitutions1 ([], "foo")                               = []
val test_get_substitutions1_5 = get_substitutions1 ([["there"]], "foo")                      = []
val test_get_substitutions1_6 = get_substitutions1 ([["foo", "bar"],["foo","bar"]], "foo")   = ["bar", "bar"]

val test_get_substitutions2_1 = get_substitutions2 ([["foo"],["there"]], "foo")              = []
val test_get_substitutions2_2 = get_substitutions2 ([["foo", "bar"],["there"]], "foo")       = ["bar"]
val test_get_substitutions2_3 = get_substitutions2 ([["foo", "bar"],["foo","there"]], "foo") = ["bar", "there"]
val test_get_substitutions2_4 = get_substitutions2 ([], "foo")                               = []
val test_get_substitutions2_5 = get_substitutions2 ([["there"]], "foo")                      = []
val test_get_substitutions2_6 = get_substitutions2 ([["foo", "bar"],["foo","bar"]], "foo")   = ["bar", "bar"]

val test_similar_names_1 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fred", last="Smith", middle="W"}, {first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}]

val test_card_color_1 = card_color (Clubs, Num 2)    = Black
val test_card_color_2 = card_color (Spades, Num 2)   = Black
val test_card_color_3 = card_color (Diamonds, Num 2) = Red
val test_card_color_4 = card_color (Hearts, Num 2)   = Red

val test_card_value_1 = card_value (Clubs, Num 2) = 2
val test_card_value_2 = card_value (Clubs, Ace)   = 11
val test_card_value_3 = card_value (Clubs, Jack)  = 10

val test_remove_card_1 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []

val test_all_same_color_1 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true

val test_sum_cards_1 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4

val test_score_1 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4

val test_getGameScore_1 = getGameScore ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test_getGameScore_2 = getGameScore ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
                     = 3

val test_getGameScore_3 = ((getGameScore([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
             

