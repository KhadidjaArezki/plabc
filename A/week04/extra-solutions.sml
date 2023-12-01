fun compose_opt f g x = 
  case g x of
    NONE     => NONE
  | (SOME y) => 
      let val z = f y
      in if z = NONE then NONE
        else z
      end

fun do_until f p x =
  if (p x) then x
  else do_until f p (f x)

fun factorial_1 n =
  let val (fact, _) = do_until
    (fn (x, y) => (x*(y-1), (y-1)))
    (fn (_, y) => y = 1)
    (n, n)
  in fact
  end

(* TODO: Redo *)
fun fixed_point f x = 
  let val (result, _) = do_until
    (fn (y,z) => (f y, z))
    (fn (y,z) => y = z)
    (x,1)
  in result
  end

fun map2 f (x,y) = (f x, f y)

fun flat_map f []        = []
  | flat_map f (x::xs)   = f x @ flat_map f xs

fun app_all f g x = flat_map f (g x)

fun foldl _ acc []      = acc
  | foldl f acc (x::xs) = foldl f (f (x, acc)) xs

fun foldr _ acc []      = acc
  | foldr f acc (x::[]) = f (x, acc)
  | foldr f acc (x::xs) = f (x, (foldr f acc xs))

(* fun foldl_2 =  *)

fun partition _ []      = ([], [])
  | partition p (x::xs) = 
      let val (evens, odds) = partition p xs
      in if p x then (x::evens, odds)
        else (evens, x::odds)
      end

fun unfold f s =
  case (f s) of 
    NONE        => []
  | SOME (n, m) => n::unfold f m

fun factorial_2 n =
  let val f = foldl (fn (x, acc) => x*acc) 1
      val g = unfold
        (fn y => if y = 0 then NONE else SOME (y, (y-1)))
  in (f o g) n
  end

fun map f = foldr (fn (x, acc) => f x::acc) []

fun filter p = foldr (fn (x, acc) => if (p x) then x::acc else acc) []

datatype 'a tree = leaf 
                  | node of { value : 'a, left : 'a tree, right : 'a tree }

fun tmap f (leaf)                                = leaf
  | tmap f (node { value=v, left=lt, right=rt }) =
     node { value=(f v), left=(tmap f lt), right=(tmap f rt) }

fun tfold _ acc leaf                                 = acc
  | tfold f acc (node { value=v, left=lt, right=rt}) = tfold f (tfold f (f (v,acc)) lt) rt

fun tfilter _ leaf                                  = leaf
  | tfilter p (node { value=v, left=lt, right=rt }) =
      if p v then
        node { value=v, left=(tfilter p lt), right=(tfilter p rt)}
      else leaf

(***************************************************)

val f = (fn (x, y) => if x > y then x else y)
val test_foldl_1 = foldl f 0 [4,3,6,1] = 6
val test_foldr_1 = foldr f 0 [4,3,6,1] = 6

val test_compose_opt_1 = compose_opt (fn y => if y < 0 then NONE else SOME (y,y))
      (fn x => if x = 0 then NONE else (SOME (x*x*x)))
      0 = NONE
val test_compose_opt_2 = compose_opt (fn y => if y < 0 then NONE else SOME (y,y))
      (fn x => if x = 0 then NONE else (SOME (x*x*x)))
      (~1) = NONE
val test_compose_opt_3 = compose_opt (fn y => if y < 0 then NONE else SOME (y,y))
      (fn x => if x = 0 then NONE else (SOME (x*x*x)))
      1 = SOME (1,1)

val test_do_until_1 = do_until (fn x => x*2) (fn x => x > 100) 2 = 128
val test_do_until_2 = do_until (fn x => x div 2) (fn x => x mod 2 = 1) 36 = 9

val test_factorial_1_1 = factorial_1 6 = 720
val g = (fn x => if x mod 2 = 0 then x div 2 else (3*x) + 1 )
val test_fixed_point_1 = fixed_point g 7 = 1

val test_map2_1 = map2 (fn x => if (x mod 2) = 0 then "even" else "odd") (2,1) = ("even", "odd")

val h = (fn n => [n, n*2, n*3])
val test_app_all_1 = app_all h h 1 = [1,2,3,2,4,6,3,6,9]

val k = (fn n => if (n mod 2) = 0 then true else false)
val test_partition_1 = partition k [0,1,2,3,4,5] = ([0,2,4], [1,3,5])

val test_unfold_1 = unfold (fn n => if n = 0 then NONE else SOME(n, (n-1))) 5 = [5,4,3,2,1]
val test_fasctorial_2_1 = factorial_2 6 = 720

val test_map_1 = map (fn n => n*n) [1,2,3,4,5,6] = [1,4,9,16,25,36]
val test_filter_1 = filter k [0,1,2,3,4,5,6] = [0,2,4,6]

val t1 = node {value=1, left=leaf, right=leaf}
val t2 = node {value=1, left=leaf, right=node {value=2, left=leaf, right=leaf}}
val t3 = node {value=1, left=leaf, right=node {
  value=2, left=node {value=3, left=leaf, right=leaf}, right=leaf
}}
val t4 = node {value=2, left=leaf, right=node {value=1, left=leaf, right=leaf}}
val t5 = node {value=2, left=leaf, right=node {value=4, left=leaf, right=leaf}}

val f = fn n => n*2
val g = fn (n,acc) => n+acc

val test_tmap_1 = tmap f leaf = leaf
val test_tmap_2 = tmap f t1   = node {value=2, left=leaf, right=leaf}
val test_tmap_3 = tmap f t2   = node {value=2, left=leaf, right=node {value=4, left=leaf, right=leaf}}
val test_tmap_4 = tmap f t3   = node {value=2, left=leaf, right=node {
  value=4, left=node {value=6, left=leaf, right=leaf}, right=leaf
}}
val test_tfold_1 = tfold g 0 leaf = 0
val test_tfold_2 = tfold g 0 t1   = 1
val test_tfold_3 = tfold g 0 t2   = 3
val test_tfold_4 = tfold g 0 t3   = 6

val test_tfilter_1 = tfilter k leaf = leaf
val test_tfilter_2 = tfilter k t1   = leaf
val test_tfilter_3 = tfilter k t4   = node {value=2, left=leaf, right=leaf}
val test_tfilter_4 = tfilter k t5   = node {value=2, left=leaf, right=node {value=4, left=leaf, right=leaf}}