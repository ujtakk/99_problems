(*
 * 66. Layout a binary tree (3). (*hard*)
 *******************************************************************************
 *
 * ![](../../img/tree-layout3.gif "Binary Tree Grid")
 *
 * Yet another layout strategy is shown in the above illustration. The
 * method yields a very compact layout while maintaining a certain symmetry
 * in every node. Find out the rules and write the corresponding predicate.
 *
 * Hint: Consider the horizontal distance between a node and its successor
 * nodes. How tight can you pack together two subtrees to construct the
 * combined binary tree? This is a difficult problem. Don't give up too
 * early\!
 *
 * # layout_binary_tree_3 example_layout_tree;;
 * - : (char * int * int) binary_tree =
 * Node (('n', 5, 1),
 *  Node (('k', 3, 2),
 *   Node (('c', 2, 3), Node (('a', 1, 4), Empty, Empty),
 *    Node (('e', 3, 4), Node (('d', 2, 5), Empty, Empty),
 *     Node (('g', 4, 5), Empty, Empty))),
 *   Node (('m', 4, 3), Empty, Empty)),
 *  Node (('u', 7, 2),
 *   Node (('p', 6, 3), Empty, Node (('q', 7, 4), Empty, Empty)), Empty))
 * # let example3_layout_tree =
 *     Node('a', Node('b', Empty, Node('e', Empty, Node('f', Empty, Empty))),
 *          Node('c', Empty, Node('d', Node('g', Empty, Empty), Empty)));;
 * val example3_layout_tree : char binary_tree =
 *   Node ('a', Node ('b', Empty, Node ('e', Empty, Node ('f', Empty, Empty))),
 *    Node ('c', Empty, Node ('d', Node ('g', Empty, Empty), Empty)))
 * # layout_binary_tree_3 example3_layout_tree;;
 * - : (char * int * int) binary_tree =
 * Node (('a', 3, 1),
 *  Node (('b', 1, 2), Empty,
 *   Node (('e', 2, 3), Empty, Node (('f', 3, 4), Empty, Empty))),
 *  Node (('c', 5, 2), Empty,
 *   Node (('d', 6, 3), Node (('g', 5, 4), Empty, Empty), Empty)))
 *
 * Which layout do you like most?
 *
 *******************************************************************************
 *)

let () =
;;

