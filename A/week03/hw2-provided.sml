(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
  string), then you avoid several of the functions in problem 1 having
  polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
  s1 = s2

(* put your solutions for problem 1 here *)

(* Sub-problem a *)
fun all_except_option (s,ss) =
  let fun aux (_, [], _)             = NONE
        | aux (s, hd::ss', NONE)      = if same_string (s, hd) then SOME ss'
                                       else aux (s, ss', SOME ([hd]))
        | aux (s, hd::ss', (SOME ys)) = if same_string (s, hd) then SOME (ys@ss')
                                       else aux (s, ss', SOME (ys@[hd]))
  in aux (s, ss, NONE)
  end

(* Sub-problem b *)
fun get_substitutions1 ([], _)      = []
  | get_substitutions1 (ss::xss, s) =
      let val all_except_s = all_except_option (s, ss)
      in
        case all_except_s of
          NONE       => get_substitutions1 (xss, s)
          |(SOME ys) => ys @ (get_substitutions1 (xss, s))
      end

(* Sub-problem c *)
fun get_substitutions2 (sss, s) =
  let fun aux ([], _, acc)      = acc
        | aux (ss::xss, s, acc) =
            let val all_except_s = all_except_option (s, ss)
            in
              case all_except_s of
                NONE       => aux (xss, s, acc)
                |(SOME ys) => aux (xss, s, acc @ ys)
            end
  in aux (sss, s, [])
  end

(* Sub-problem d *)
fun similar_names (sss, {first=fst, middle=mid, last=lst}) =
  let
    fun aux ([], acc)     = acc
      | aux (hd::ss, acc) = aux (ss, acc @ [{first=hd, last=lst, middle=mid}])
  in
    aux (get_substitutions1 (sss, fst), [{first=fst, last=lst, middle=mid}])
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

(* Sub-problem a *)
fun card_color (Spades, _)   = Black
  | card_color (Clubs, _)    = Black
  | card_color (Diamonds, _) = Red
  | card_color (Hearts, _)   = Red

(* Sub-problem b *)
fun card_value (_, Ace)     = 11
  | card_value (_, (Num n)) = n
  | card_value (_, _)       = 10

(* Sub-problem c *)
fun remove_card (cs, c, e) =
  let fun aux ([], c, acc)     = raise e
        | aux (hd::xs, c, acc) = if hd = c then acc @ xs
                                 else aux (xs, c, hd::acc)
  in
    aux (cs, c, [])
  end

(* Sub-problem d *)
fun all_same_color ([])         = true
  | all_same_color ([_])        = true
  | all_same_color (hd::nk::cs) = card_color hd = card_color nk
                                  andalso all_same_color (nk::cs)

(* Sub-problem e *)
fun sum_cards (cs) =
  let fun aux ([], acc) = acc
        | aux (hd::cs', acc) = aux(cs', card_value hd + acc)
  in
    aux (cs, 0)
  end

(* Sub-problem f *)
fun score (cs, n) =
  let val sum = sum_cards cs
      val preliminary_score = if sum > n then (sum - n) * 3
                              else (n - sum)
  in
    if all_same_color cs then preliminary_score div 2
    else preliminary_score 
  end

(* Sub-problem g *)
fun getGameScore (cs, ms, n) =
  let fun game (_, [], acc)                = score (acc, n)
        | game ([], Draw::ms', acc)        = score (acc, n)
        | game (cs, (Discard c)::ms', acc) = game (cs, ms', remove_card (acc, c, IllegalMove))
        | game (c::cs', Draw::ms', acc)    = if (sum_cards (c::acc)) > n
                                               then score ((c::acc), n)
                                             else game (cs', ms', (c::acc))
  in
    game (cs, ms, [])
  end

(* Challenge problem b *)
fun careful_player (cs, n) =
  let
    fun getCardsFromMoves (m::ms) =
      case m of
        (Discard) => getCardsFromMoves ms
      | (Draw c)  => c::getCardsFromMoves ms

    fun play (c::cs', n, acc) = 
      if getGameScore (c::cs', acc, n) = 0 then acc
      else
        let
          val discard_draw_moves = (Discard)::(Draw c)
          val sum = sum_cards (getCardsFromMoves acc)
        in
          if getGameScore (c::cs', discard_draw_moves::acc, n) = 0 then discard_draw_moves::acc
          else
            if sum > n then play(c::cs', (Discard)::acc, n)
            else
              if n - sum > 10 then play (cs', n, (Draw c)::acc)
              else 
                let val draw_move = (Draw c)
                in
                  if getGameScore (c::cs', draw_move::acc, n) <= n then  play (cs', n, draw_move::acc)
                  else play (c::cs', n, (Discard)::acc)
        end
  in
    play (cs, n, [])
  end

