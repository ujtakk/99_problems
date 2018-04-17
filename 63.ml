(*
 * 63. Construct a complete binary tree. (*medium*)
 *******************************************************************************
 *
 * A *complete* binary tree with height H is defined as follows: The levels
 * 1,2,3,...,H-1 contain the maximum number of nodes (i.e 2<sup>i-1</sup>
 * at the level i, note that we start counting the levels from 1 at the
 * root). In level H, which may contain less than the maximum possible
 * number of nodes, all the nodes are "left-adjusted". This means that in a
 * levelorder tree traversal all internal nodes come first, the leaves come
 * second, and empty successors (the nil's which are not really nodes\!)
 * come last.
 *
 * Particularly, complete binary trees are used as data structures (or
 * addressing schemes) for heaps.
 *
 * We can assign an address number to each node in a complete binary tree
 * by enumerating the nodes in levelorder, starting at the root with
 * number 1. In doing so, we realize that for every node X with address A
 * the following property holds: The address of X's left and right
 * successors are 2\*A and 2\*A+1, respectively, supposed the successors do
 * exist. This fact can be used to elegantly construct a complete binary
 * tree structure. Write a function `is_complete_binary_tree` with the
 * following specification: `is_complete_binary_tree n t` returns `true`
 * iff `t` is a complete binary tree with `n` nodes.
 *
 * # complete_binary_tree [1;2;3;4;5;6];;
 * - : int binary_tree =
 * Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
 *  Node (3, Node (6, Empty, Empty), Empty))
 *
 *******************************************************************************
 *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let complete_binary_tree_0 dict =
  let rec power a = function
    | 0 -> 1
    | k -> a * power a (k-1)
  in
  let rec insert tree value index depth =
    if depth = 0 then
      match tree with
      | Empty -> Node (value, Empty, Empty)
      | _ -> raise Exit
    else
      match tree with
      | Node (id, a, b) ->
          if index / (power 2 depth + power 2 (depth-1)) = 0 then
            Node (id, insert a value (index-power 2 (depth-1)) (depth-1), b)
          else
            Node (id, a, insert b value (index-power 2 depth) (depth-1))
      | _ -> raise Exit
  in
  let rec iter acc index depth = function
    | [] -> acc
    | h :: t ->
        if index+1 = power 2 (depth+1) then
          iter (insert acc h index depth) (index+1) (depth+1) t
        else
          iter (insert acc h index depth) (index+1) depth t
  in
  iter Empty 1 0 dict
;;

let complete_binary_tree = complete_binary_tree_0

let () =
  assert (
    complete_binary_tree [1;2;3;4;5;6]
    = Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
       Node (3, Node (6, Empty, Empty), Empty))
  );
;;

