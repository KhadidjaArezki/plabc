fun min (n1, n2) = if (n1 < n2) then n1 else n2
fun max (n1, n2) = if (n1 > n2) then n1 else n2

exception EmptyListException

fun min_max ([])         = raise EmptyListException
  | min_max (hd::[])      = (hd, hd)
  | min_max (hd::ns)     =
      let val (mn_tl, mx_tl) = min_max ns
      in
        (min (hd, mn_tl), max(hd, mx_tl)) 
      end

fun cumsum ([])     = []
  | cumsum (hd::ns) =
  let fun aux ([], tmp)      = [tmp]
        | aux (hd::ns', tmp) = tmp::aux(ns', hd + tmp)
  in aux (ns, hd)
  end

fun repeat ([], _)        = []
  | repeat (ns, [])       = ns
  | repeat (n::ns, m::ms) =
      let
        fun n_times (x, 0) = []
          | n_times (x, n) = x::n_times(x, n-1)
      in n_times(n, m) @ repeat (ns, ms)
      end

fun any ([])    = false
  | any (b::bs) = b orelse any bs

fun all ([])    = true
  | all (b::bs) = b andalso all bs

fun zip ([], _) = []
  | zip (_, []) = []
  | zip (x::xs, y::ys) = (x, y)::zip(xs, ys)

fun zip_recycle ([], []) = []
  | zip_recycle (xs, ys) =
      let
        fun aux_patch_xs ([], _, acc)         = acc
          | aux_patch_xs (ys', [], acc)       = aux_patch_xs (ys', xs, acc)
          | aux_patch_xs (y::ys', z::zs, acc) = aux_patch_xs (ys', zs, acc @ [(z, y)])
        
        fun aux_patch_ys ([], _, acc)         = acc
          | aux_patch_ys (xs', [], acc)       = aux_patch_ys (xs', ys, acc)
          | aux_patch_ys (x::xs', z::zs, acc) = aux_patch_ys (xs', zs, acc @ [(x, z)])

        fun aux ([], [], acc)         = acc
          | aux ([], ys', acc)        = aux_patch_xs (ys', xs, acc)
          | aux (xs', [], acc)        = aux_patch_ys (xs', ys, acc)
          | aux (x::xs', y::ys', acc) = aux (xs', ys', acc @ [(x, y)])
      in
        aux (xs, ys, [])
      end

fun splitAt ([], _)    = ([], [])
  | splitAt (x::xs, n) = 
      let val (firstHalf, secondHalf) = splitAt (xs, n)
      in 
        if x < n then (x::firstHalf, secondHalf)
        else (firstHalf, x::secondHalf)
      end

fun splitUp (xs) = splitAt (xs, 0)

fun isSorted []         = true
  | isSorted (_::[])    = true
  | isSorted (x::y::xs) = x <= y andalso isSorted (y::xs)

fun isSortedDec []         = true
  | isSortedDec (_::[])    = true
  | isSortedDec (x::y::xs) = x >= y andalso isSortedDec (y::xs)

fun isAnySorted (xs) = isSorted xs orelse isSortedDec (xs)

fun sortedMerge ([], ys) = ys
  | sortedMerge (xs, []) = xs
  | sortedMerge (x::xs, y::ys) =
      if x <= y then x::sortedMerge (xs, y::ys)
      else y::sortedMerge (x::xs, ys)

fun qSort ([])    = []
  | qSort (x::[]) = [x]
  | qSort (x::xs) =
      let
        val (left, right) = splitAt (xs, x)
      in
        qSort left @ x::qSort right
      end

fun divide []         = ([], [])
  | divide (x::[])    = ([x], [])
  | divide (x::y::ys) =
      let val (left, right) = divide ys
      in (x::left, y::right)
      end

fun not_so_quick_sort ([])     = []
  | not_so_quick_sort (x::[])  = [x]
  | not_so_quick_sort (xs) =
      let
        val (left, right) = divide xs
      in
        sortedMerge (not_so_quick_sort left, not_so_quick_sort right)
      end

fun fullDivide (n, k) = 
  if k mod n <> 0
    then (0, k)
  else
    let val (d, m) = fullDivide (n, k div n)
    in (d + 1, m)
    end

fun factorize n =
  let 
    fun aux (d, n') =
      if n' = 1 then []
      else
        let val (m, r) = fullDivide (d, n')
        in
          if m = 0 then aux (d + 1, n')
          else  
            if (Real.fromInt d) > Math.sqrt (Real.fromInt n)
              then [(d, m)]
            else (d, m)::aux (d + 1, r)
        end
  in
    aux (2, n)
  end

fun multiply []          = 1
  | multiply ((d,k)::ps) =
      let
        fun pow (d',1)  = d 
          | pow (d',k') = d' * pow (d',k'- 1)
      in
        pow (d,k) * multiply ps
      end

fun unique [] = []
  | unique (x::xs) =
      let val ys = List.filter (fn y => y <> x) xs
      in x::unique ys
      end

(*wrong*)
fun all_products lst =
  let fun aux ([], acc)                   = acc
        | aux ((d,k)::[], acc)            = d::k::(d*k)::acc
        | aux ((d1,k1)::(d2,k2)::ps, acc) = 
            d1::k1::d2::k2::(d1*k1)::(d1*d2)::(d1*k2)::(k1*d2)::(k1*k2)::(d1*k1*d2)::(d1*k1*k2)::(d1*d2*k2)::(k1*d2*k2)::acc
  in qSort(unique(aux (lst, [])))
  end
                
(* fun all_products (xs : (int * int) list) = *)
  (* if null xs *)
  (* then [1] *)
  (* else *)
    (* let *)
      (* fun power (d : int, k : int) = *)
        (* if k = 0 *)
        (* then 1 *)
        (* else d * power (d, k - 1) *)
      (* fun mulToAll (xs : int list, d : int) = *)
        (* if null xs *)
        (* then [] *)
        (* else ((hd xs) * d) :: mulToAll (tl xs, d) *)
      (* fun mulPowersToAll (xs : int list, d : int, k : int) = *)
        (* if k = 0 *)
        (* then xs *)
        (* else sortedMerge (mulToAll (xs, power (d, k)), *)
                            (* mulPowersToAll (xs, d, k - 1)) *)
    (* in *)
      (* mulPowersToAll (all_products (tl xs), #1 (hd xs), #2 (hd xs)) *)
    (* end *)


val test_min_max_1 = min_max [1,2,3]     = (1, 3)
val test_min_max_2 = min_max [1,1]       = (1, 1)
val test_min_max_4 = min_max [2]         = (2, 2)
val test_min_max_5 = min_max [2,1]       = (1, 2)
val test_min_max_6 = min_max [2,1,9,6,0] = (0, 9)

val test_cumsum_1 = cumsum [1,4,20] = [1,5,25]

val test_repeat_1 = repeat ([1,2,3], [4,0,3]) = [1,1,1,1,3,3,3]

val test_any_1 = any([false, false]) = false
val test_any_3 = any ([false, true]) = true

val test_all_1 = all ([true, true])  = true
val test_all_2 = all ([true, false]) = false

val test_zip_1 = zip ([1,2,3],[4,6]) = [(1,4),(2,6)]

val test_zip_recycle_1 = zip_recycle ([1,2,3], [1,2,3,4,5,6,7]) = [(1,1),(2,2),(3,3),(1,4),(2,5),(3,6),(1,7)]

val test_splitUp_1 = splitUp ([1,~2, ~5, 4, 0]) = ([~2,~5], [1,4,0])

val test_isSorted_1 = isSorted [0,2,3,6,0]   = false
val test_isSorted_2 = isSorted [4,6,8,11,18] = true

val test_isSortedDec_1 = isSortedDec [9,6,3,1] = true
val test_isSortedDec_2 = isSortedDec [9,6,7,1] = false

val test_isAnySorted_1 = isAnySorted [0,2,3,6,9] = true
val test_isAnySorted_2 = isAnySorted [0,2,3,6,0] = false
val test_isAnySorted_3 = isAnySorted [9,6,3,1]   = true
val test_isAnySorted_4 = isAnySorted [9,6,7,1]   = false

val test_sorted_merge_1 = sortedMerge ([1,4,7],[5,8,9]) = [1,4,5,7,8,9]

val test_qSort_1 = qSort [4,2,6,1,9] = [1,2,4,6,9]

val test_divide_1 = divide ([1,2,3,4,5,6,7]) = ([1,3,5,7], [2,4,6])

val test_not_so_quick_sort_1 = not_so_quick_sort [4,2,6,1,9] = [1,2,4,6,9]

val test_fullDivide_1 = fullDivide (2,40) = (3,5)
val test_fullDivide_2 = fullDivide (3,10) = (0, 10)

val test_factorize_1 = factorize(20) = [(2,2), (5,1)]
val test_factorize_2 = factorize(36) = [(2,2), (3,2)]
val test_factorize_3 = factorize(1)  = []

val test_multiply_1 = multiply [(2,2), (5,1)] = 20
val test_multiply_2 = multiply [(2,2), (3,2)] = 36
val test_multiply_3 = multiply [(2,3), (5,1)] = 40 
val test_multiply_4 = multiply []             = 1

val test_all_products_1 = all_products([(2,2), (5,1)]) = [1,2,4,5,10,20]
(* val test_all_products_2 = all_products([(2,3), (5,1)]) = [1,2,3,4,5,6,9,10,15,25,30,40] *)
(* val test_all_products_2 = all_products([(2,3), (5,1)]) = [1,2,3,5,6,10,15,30] *)
(* val test_all_products_2 = all_products([(2,3), (5,1)]) = [1,2,4,5,8,10,20,40] *)