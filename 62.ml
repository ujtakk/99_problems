(*
 * 62. Collect the internal nodes of a binary tree in a list. (*easy*)
 *******************************************************************************
 *
 * An internal node of a binary tree has either one or two non-empty
 * successors. Write a function `internals` to collect them in a list.
 *
 * # internals (Node('a', Empty, Empty));;
 * - : char list = []
 * # internals example_tree;;
 * - : char list = ['b'; 'a'; 'c'; 'f']
 *
 *******************************************************************************
 *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec internals_0 = function
  | Empty -> []
  | Node (_, Empty, Empty) -> []
  | Node (id, a, b) -> internals_0 a @ [id] @ internals_0 b

;;

let internals = internals_0

let () =
  assert (internals (Node('a', Empty, Empty)) = []);

  let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)))
  in
  assert (internals example_tree = ['b'; 'a'; 'c'; 'f']);
;;

