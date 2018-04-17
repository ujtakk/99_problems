(*
 * 62B. Collect the nodes at a given level in a list. (*easy*)
 *******************************************************************************
 *
 * A node of a binary tree is at level N if the path from the root to the
 * node has length N-1. The root node is at level 1. Write a function
 * `at_level t l` to collect all nodes of the tree `t` at level `l` in a
 * list.
 *
 * # at_level example_tree 2;;
 * - : char list = ['b'; 'c']
 * # at_level example_tree 5;;
 * - : char list = []
 *
 * Using `at_level` it is easy to construct a function `levelorder` which
 * creates the level-order sequence of the nodes. However, there are more
 * efficient ways to do that.
 *
 *******************************************************************************
 *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec at_level_0 tree level =
  match tree, level with
  | Empty, _ -> []
  | Node (id, a, b), 1 -> [id]
  | Node (id, a, b), n -> at_level_0 a (n-1) @ at_level_0 b (n-1)
;;

let at_level = at_level_0

let () =
  let example_tree =
    Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
         Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)))
  in

  assert (at_level example_tree 2 = ['b'; 'c']);
  assert (at_level example_tree 5 = []);
;;

