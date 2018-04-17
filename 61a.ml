(*
 * 61A. Collect the leaves of a binary tree in a list. (*easy*)
 *******************************************************************************
 *
 * A leaf is a node with no successors. Write a function `leaves` to
 * collect them in a list.
 *
 * # leaves Empty;;
 * - : 'a list = []
 * # leaves example_tree;;
 * - : char list = ['d'; 'e'; 'g']
 *
 *******************************************************************************
 *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec leaves_0 = function
  | Empty -> []
  | Node (id, Empty, Empty) -> [id]
  | Node (id, a, b) -> (leaves_0 a) @ (leaves_0 b)
;;

let leaves = leaves_0

let () =
  assert (leaves Empty = []);

  let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)))
  in
  assert (leaves example_tree = ['d'; 'e'; 'g']);
;;

