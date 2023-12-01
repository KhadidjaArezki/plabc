datatype 'a tree = leaf 
                  | node of { value : 'a, left : 'a tree, right : 'a tree }
datatype flag = leave_me_alone | prune_me

fun tree_height (leaf)                              = 0
  | tree_height (node {value=v, left=lt, right=rt}) =
      Int.max (1 + (tree_height lt), 1 + (tree_height rt))

fun sum_tree (leaf) = 0
  | sum_tree (node {value=n, left=lt, right=rt}) =
      n + sum_tree lt + sum_tree rt

fun gardener (leaf) = leaf
  | gardener (node {value=prune_me, left=lt, right=rt}) = leaf
  | gardener (node {value=leave_me_alone, left=lt, right=rt}) =
      node {value=leave_me_alone, left=gardener lt, right=gardener rt}

(* Re-implement last, take, drop, concat, getOpt, and join. *)
exception EmptyList

fun last []      = raise EmptyList
  | last (x::[]) = x
  | last (x::xs) = last xs

fun take (_,0)      = []
  | take ([], n)    = raise EmptyList
  | take (x::xs, n) = x::take (xs, n-1)

fun drop (xs,0)     = xs
  | drop ([], n) = raise EmptyList
  | drop (x::xs, n) = drop (xs, n-1)

fun concat []        = []
  | concat (xs::xss) = xs @ concat xss

fun append ([],ys)      = ys
  | append (x::xs, ys)  = x::append (xs,ys)

fun rev lst =
  let fun aux ([], acc) = acc
        | aux (x::xs, acc) = aux (xs, x::acc)
  in aux (lst, [])
  end

fun unique [] = []
  | unique (x::xs) =
      let val ys = List.filter (fn y => y <> x) xs
      in x::unique ys
      end

fun list_intersection (lst1, lst2) =
  let 
    val (xs, ys) = (unique lst1, unique lst2)

    fun aux ([], _, acc)       = acc
      | aux (_, [], acc)       = acc
      | aux (x::xs', ys, acc)  =
          if (List.exists(fn y => y = x) ys)
            then aux (xs', ys, x::acc)
          else aux (xs', ys, acc)
  in aux (lst1, lst2, [])
  end

(****************************************************************)

datatype nat = ZERO | SUCC of nat
exception Negative

fun is_positive (ZERO) = false
  | is_positive (_)    = true

fun pred (ZERO)      = raise Negative
  | pred (SUCC n)    = n

fun nat_to_int (ZERO) = 0
  | nat_to_int (SUCC n) = 1 + nat_to_int n

fun int_to_nat 0 = ZERO
  | int_to_nat n = 
      if n < 0 then raise Negative
      else SUCC (int_to_nat (n-1))

fun add (ZERO, ZERO)     = ZERO
  | add (ZERO, (SUCC n)) = (SUCC n)
  | add ((SUCC n), ZERO) = (SUCC n)
  | add ((SUCC n), (SUCC n')) = add ((SUCC (SUCC n)), n')

fun sub ((SUCC n), ZERO) = (SUCC n)
  | sub (ZERO, _)        = ZERO
  | sub (n, n')          = sub (pred n, pred n')

fun mult (ZERO, _) = ZERO
  | mult (_, ZERO) = ZERO
  | mult (n, n')   = add (n, mult (n, pred n'))

fun less_than (n, n') = nat_to_int n < nat_to_int n'

(****************************************************************)

datatype intSet = 
  Elems of int list (*list of integers, possibly with duplicates to be ignored*)
| Range of { from : int, to : int }  (* integers from one number to another *)
| Union of intSet * intSet (* union of the two sets *)
| Intersection of intSet * intSet (* intersection of the two sets *)

exception InvalidRange

fun contains ((Elems xs), n)                = List.exists (fn x => x = n) xs
  | contains ((Range {from=n1, to=n2}), n)  = n >= n1 andalso n <= n2
  | contains ((Union (is1, is2)), n)        = contains (is1, n) orelse contains (is2, n)
  | contains ((Intersection (is1, is2)), n) = contains (is1, n) andalso contains (is2, n)

fun toList (Elems xs)                = unique xs
  (* Should a Range with n1 = n2 be empty or not ? *)
  | toList (Range {from=n1, to=n2})  = 
      if n1 > n2 then []
      else
        List.tabulate ((n2-n1)+1, (fn n => n + n1))
  | toList (Union (is1, is2))        = unique (toList is1 @ toList is2)
  | toList (Intersection (is1, is2)) = unique (List.filter (fn i => contains (is2, i)) (toList is1))

fun set_intersection (is1, is2) = (Elems (list_intersection (toList is1, toList is2)))

fun isEmpty (Elems xs)                = null xs
  (* If n1 = n2, is the Range empty or not ? *)
  | isEmpty (Range {from=n1, to=n2})  = n1 > n2 
  | isEmpty (Union (is1, is2))        = isEmpty is1 andalso isEmpty is2
  | isEmpty (Intersection (is1, is2)) = isEmpty (set_intersection (is1, is2))

fun set_intersection_2 ((Elems xs), (Elems ys)) = (Elems (list_intersection (xs, ys)))

  | set_intersection_2 ((Range {from=n1, to=n2}), (Range {from=n3, to=n4})) =
      if n1 <= n3 andalso n2 <= n4
        then (Range {from=n3, to=n2})
      else
        if n3 <= n1 andalso n4 <= n2
          then (Range {from=n1, to=n4})
        else (Elems [])

  | set_intersection_2 ((Union (is1, is2)), (Union (is3, is4))) = 
      set_intersection_2 (set_intersection_2 (is1, is2), set_intersection_2 (is3, is4))

  | set_intersection_2 ((Intersection (is1,is2)), (Intersection (is3,is4))) =
      set_intersection_2 (set_intersection_2 (is1, is2), set_intersection_2 (is3, is4))
  
  | set_intersection_2 ((Union (is1,is2)), (Intersection (is3,is4))) = 
      set_intersection_2 (set_intersection_2 (is1, is2), set_intersection_2 (is3, is4))

  | set_intersection_2 ((Intersection (is3,is4)), (Union (is1,is2))) = 
      set_intersection_2 (set_intersection_2 (is1, is2), set_intersection_2 (is3, is4))
  (* ...... Keep Coing ..... or not *)

(****************************************************************)

val test_tree_height_1 = tree_height (leaf) = 0
val test_tree_height_2 = tree_height (node {value=1, left=leaf, right=leaf}) = 1
val test_tree_height_3 = tree_height (node {value=1, left=leaf, right=node {
  value=2, left=leaf, right=leaf
}}) = 2
val test_tree_height_4 = tree_height (node {value=1, left=leaf, right=node {
  value=2, left=node {value=3, left=leaf, right=leaf}, right=leaf
}}) = 3

val test_sum_tree_1 = sum_tree (leaf)                                  = 0
val test_sum_tree_2 = sum_tree (node {value=1, left=leaf, right=leaf}) = 1
val test_sum_tree_3 = sum_tree (node {value=1, left=leaf, right=node {
  value=2, left=leaf, right=leaf
}})                                                                    = 3
val test_sum_tree_4 = sum_tree (node {value=1, left=leaf, right=node {
  value=2, left=node {value=3, left=leaf, right=leaf}, right=leaf
}}) = 6

val test_gardener_1 = gardener (leaf)                             = leaf
val test_gardener_2 = gardener (node {value=prune_me, left=leaf, 
  right=leaf})                                                    = leaf 
val test_gardener_3 = gardener (node {value=leave_me_alone,
  left=leaf, right=node {value=prune_me, left=leaf, right=leaf}}) = node {value=leave_me_alone, left=leaf, right=leaf}

val test_last_1 = last [3]   = 3
val test_last_2 = last [1,2] = 2
val test_take_1 = take ([2], 0)     = []
val test_take_2 = take ([2], 1)     = [2]
val test_take_3 = take ([1,2,3], 2) = [1,2]
val test_drop_1 = drop ([1],0)   = [1]
val test_drop_2 = drop ([1],1)   = []
val test_drop_3 = drop ([1,2],1) = [2]
val test_concat_1 = concat [[1]] = [1]
val test_concat_2 = concat [[1],[2]] = [1,2]
val test_concat_3 = concat [[1],[2,3]] = [1,2,3]
val test_append_1 = append ([1],[]) = [1]
val test_append_2 = append ([1],[2]) = [1,2]
val test_append_3 = append ([1],[2,3]) = [1,2,3]
val test_rev_1 = rev ([1])   = [1]
val test_rev_2 = rev ([1,2]) = [2,1]
val test_rev_3 = rev ([])    = []
val test_unique_1 = unique [1,2,2,3] = [1,2,3]
val test_unique_2 = unique [1,2,3] = [1,2,3]
val test_unique_3 = unique [1] = [1]
val test_list_intersection_1 = list_intersection ([1,2], [2,3]) = [2]
val test_list_intersection_2 = list_intersection ([1,2], [3,4]) = []
val test_list_intersection_3 = list_intersection ([1,2], [1,2]) = [2,1]

val test_pred_1 = pred (SUCC ZERO)        = ZERO
val test_pred_2 = pred (SUCC (SUCC ZERO)) = (SUCC ZERO)
val test_nat_to_int_1 = nat_to_int (SUCC ZERO)        = 1
val test_nat_to_int_2 = nat_to_int (SUCC (SUCC ZERO)) = 2
val test_int_to_nat_1 = int_to_nat 1 = (SUCC ZERO)
val test_int_to_nat_2 = int_to_nat 2 = (SUCC (SUCC ZERO))
val test_add_1 = add ((SUCC (SUCC ZERO)), (SUCC ZERO)) = (SUCC (SUCC (SUCC ZERO)))
val test_add_2 = add ((SUCC ZERO), (SUCC ZERO))        = (SUCC (SUCC ZERO))
val test_sub_1 = sub ((SUCC ZERO), (SUCC ZERO))        = ZERO
val test_sub_2 = sub ((SUCC (SUCC ZERO)), (SUCC ZERO)) = (SUCC ZERO)
val test_sub_3 = sub ((SUCC ZERO), (SUCC (SUCC ZERO))) = ZERO
val test_mult_1 = mult ((SUCC (SUCC ZERO)), (SUCC ZERO)) = (SUCC (SUCC ZERO))
val test_mult_2 = mult ((SUCC ZERO), (SUCC ZERO))        = (SUCC ZERO)
val test_less_than_1 = less_than ((SUCC ZERO), (SUCC (SUCC ZERO))) = true
val test_less_than_2 = less_than ((SUCC ZERO), (SUCC ZERO))        = false
val test_less_than_3 = less_than ((SUCC (SUCC ZERO)), (SUCC ZERO)) = false

val test_isEmpty_1 = isEmpty (Elems [])                                           = true
val test_isEmpty_2 = isEmpty (Elems [1])                                          = false
val test_isEmpty_3 = isEmpty (Range {from=1, to=1})                               = false
val test_isEmpty_4 = isEmpty (Range {from=2, to=1})                               = true
val test_isEmpty_5 = isEmpty (Range {from=1, to=2})                               = false
val test_isEmpty_6 = isEmpty (Union ((Elems [1]), (Range {from=2, to=1})))        = false
val test_isEmpty_7 = isEmpty (Union ((Elems []), (Range {from=4, to=3})))         = true
val test_isEmpty_8 = isEmpty (Intersection ((Elems [1]), (Range {from=1, to=2}))) = false 
val test_isEmpty_9 = isEmpty (Intersection ((Elems [1]), (Range {from=2, to=3}))) = true 
val test_contains_1 = contains ((Range {from=1, to=1}), 1) = true
val test_contains_2 = contains ((Range {from=1, to=1}), 2) = false
val test_contains_3 = contains ((Range {from=1, to=2}), 3) = false
val test_contains_4 = contains ((Elems [2,3,1]), 1)        = true
val test_contains_5 = contains ((Elems [2,3,1]), 4)        = false
val test_contains_6 = contains ((Elems []), 1)             = false
val test_contains_7 = contains ((Union ((Elems [2]), (Range {from=3, to=4}))), 3) = true
val test_contains_8 = contains ((Union ((Elems [2]), (Range {from=3, to=4}))), 1) = false
val test_contains_9 = contains ((Intersection ((Elems [2]), (Range {from=1, to=2}))), 2) = true
val test_contains_10 = contains ((Intersection ((Elems [2]), (Range {from=1, to=2}))), 3) = false
val test_toList_1 = toList (Elems [1,2])          = [1,2]
val test_toList_2 = toList (Range {from=1, to=2}) = [1,2]
val test_toList_3 = toList (Range {from=1, to=1}) = [1]
val test_toList_4 = toList (Union (Range {from=1, to=2}, (Elems [2,3]))) = [1,2,3]
val test_toList_5 = toList (Intersection (Range {from=1, to=2}, (Elems [2,3]))) = [2]
