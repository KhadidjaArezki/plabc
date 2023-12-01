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

val only_capitals = List.filter
  (fn s => Char.isUpper (String.sub (s,0)))

val longest_string1 = foldl 
  (fn (current, longest) =>
    if String.size current > String.size longest
      then current
    else longest
  )
  ""


val longest_string2 = foldl 
  (fn (current, longest) =>
    if String.size current >= String.size longest
      then current
    else longest
  )
  ""

fun longest_string_helper _ []          = ""
  | longest_string_helper p (s::[])     = s
  | longest_string_helper p (s::s'::ss) = 
      if p (String.size s', String.size s)
        then longest_string_helper p (s'::ss)
      else longest_string_helper p (s::ss)

val longest_string3 = longest_string_helper 
      (fn (currentStringSize, longestStringSize) => 
        currentStringSize > longestStringSize
      )

val longest_string4 = longest_string_helper
      (fn (currentStringSize, longestStringSize) => 
        currentStringSize >= longestStringSize
      )

val longest_capitalized = 
  foldl (fn (current, longest) =>
    if
      (Char.isUpper o String.sub )
      (current, 0) andalso String.size current > String.size longest
      then current
    else longest
  )
  ""

val rev_string = String.implode o List.rev o String.explode

fun first_answer _ []      = raise NoAnswer
  | first_answer f (x::xs) =
      case f x of
        NONE   => first_answer f xs
      | SOME v => v

fun all_answers _ []      = SOME []
  | all_answers f (x::xs) =
      case f x of
        NONE    => NONE
      | SOME ys =>
          let val (SOME zs) = all_answers f xs
          in SOME (ys@zs)
          end

fun count_wildcards p = g
  (fn () => 1)
  (fn x => 0)
  p

fun count_wild_and_variable_lengths p = g
  (fn () => 1)
  String.size
  p

fun count_some_var (s,p) = g
  (fn () => 0)
  (fn str => if str = s then 1 else 0)
  p

fun check_pat p =
  let
    fun getStrings (Variable s)          = [s]
      | getStrings (ConstructorP (_,p')) = getStrings p'
      | getStrings (TupleP ps)           = foldl
          (fn (p', acc) => 
            case p' of
              Variable s => s::acc
            | _          => acc@getStrings p'
          )
          [] ps
      | getStrings _                     = []

    fun all_distinct []      = true
      | all_distinct (s::ss) = not (List.exists (fn s' => s' = s) ss)
                              andalso all_distinct ss

  in
    all_distinct (getStrings p)
  end

fun match (_, Wildcard)                            = SOME []
  | match (v, Variable s)                          = SOME [(s, v)]
  | match (Unit, UnitP)                            = SOME []             
  | match (Const n, ConstP n')                     = if n = n' then SOME [] else NONE
  | match (Tuple vs, TupleP ps)                    = if List.length vs = List.length ps 
                                                      then all_answers match (ListPair.zip (vs, ps))
                                                    else NONE
  | match (Constructor (s,v), ConstructorP (s',p)) =
      if s = s'
        then match (v,p)
      else NONE
  | match (_,_)                                    = NONE

fun first_match v ps = SOME (
                      first_answer
                        match 
                        (ListPair.zip (
                          (List.tabulate (List.length ps, fn _ => v)),
                          ps)
                        )
                      )
                      handle NoAnswer => NONE
