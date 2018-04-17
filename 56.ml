(*
 * 56. Symmetric binary trees. (*medium*)
 *******************************************************************************
 *
 * Let us call a binary tree symmetric if you can draw a vertical line
 * through the root node and then the right subtree is the mirror image of
 * the left subtree. Write a function `is_symmetric` to check whether a
 * given binary tree is symmetric.
 *
 * Hint: Write a function `is_mirror` first to check whether one tree is
 * the mirror image of another. We are only interested in the structure,
 * not in the contents of the nodes.
 *
 *******************************************************************************
 *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec equal tree_a tree_b =
  match tree_a, tree_b with
  | Empty, Empty -> true
  | Node (_, x0, y0), Node (_, x1, y1) ->
      equal x0 x1 && equal y0 y1
  | _ -> false
;;

let rec reverse = function
  | Empty -> Empty
  | Node (id, a, b) -> Node (id, reverse b, reverse a)
;;

let is_mirror_0 left right =
  equal left (reverse right)
;;

let is_mirror = is_mirror_0

let is_symmetric_0 = function
  | Empty -> true
  | Node (_, x0, y0) -> is_mirror x0 y0
;;

let is_symmetric = is_symmetric_0

let () =
  assert (
    is_symmetric ( Node (0, Node (1, Node (2, Empty, Empty), Empty),
                            Node (3, Empty, Node (4, Empty, Empty))) )
  );

  assert (
    not @@ is_symmetric ( Node (0, Node (1, Node (2, Empty, Empty), Empty),
                                   Node (3, Node (4, Empty, Empty), Empty)) )
  );
;;

