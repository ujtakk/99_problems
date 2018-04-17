(*
 * 61. Count the leaves of a binary tree. (*easy*)
 *******************************************************************************
 *
 * A leaf is a node with no successors. Write a function `count_leaves` to
 * count them.
 *
 * # count_leaves Empty;;
 * - : int = 0
 * # count_leaves example_tree;;
 * - : int = 3
 *
 *******************************************************************************
 *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec count_leaves_0 = function
  | Empty -> 0
  | Node (_, Empty, Empty) -> 1
  | Node (_, a, b) -> (count_leaves_0 a) + (count_leaves_0 b)
;;

let count_leaves = count_leaves_0

let () =
  assert (count_leaves Empty = 0);

  let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)))
  in
  assert (count_leaves example_tree = 3);
;;

