(*
 * ![](../../img/binary-tree.gif "Binary Tree")
 *
 * ## Binary Trees
 *
 * *A binary tree is either empty or it is composed of a root element and
 * two successors, which are binary trees themselves.*
 *
 * In OCaml, one can define a new type `binary_tree` that carries an
 * arbitrary value of type `'a` (thus is polymorphic) at each node.
 *
 * # type 'a binary_tree =
 *     | Empty
 *     | Node of 'a * 'a binary_tree * 'a binary_tree;;
 * type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree
 *
 * An example of tree carrying `char` data is:
 *
 * # let example_tree =
 *     Node('a', Node('b', Node('d', Empty, Empty), Node('e', Empty, Empty)),
 *          Node('c', Empty, Node('f', Node('g', Empty, Empty), Empty)));;
 * val example_tree : char binary_tree =
 *   Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
 *    Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))
 * # let example_int_tree =
 *     Node(1, Node(2, Node(4, Empty, Empty), Node(5, Empty, Empty)),
 *          Node(3, Empty, Node(6, Node(7, Empty, Empty), Empty)));;
 * val example_int_tree : int binary_tree =
 *   Node (1, Node (2, Node (4, Empty, Empty), Node (5, Empty, Empty)),
 *    Node (3, Empty, Node (6, Node (7, Empty, Empty), Empty)))
 *
 * In OCaml, the strict type discipline *guarantees* that, if you get a
 * value of type `binary_tree`, then it must have been created with the two
 * constructors `Empty` and `Node`.
 *
 * 55. Construct completely balanced binary trees. (*medium*)
 *******************************************************************************
 *
 * In a completely balanced binary tree, the following property holds for
 * every node: The number of nodes in its left subtree and the number of
 * nodes in its right subtree are almost equal, which means their
 * difference is not greater than one.
 *
 * Write a function `cbal_tree` to construct completely balanced binary
 * trees for a given number of nodes. The function should generate all
 * solutions via backtracking. Put the letter `'x'` as information into all
 * nodes of the tree.
 *
 * # cbal_tree 4;;
 * - : char binary_tree list =
 * [Node ('x', Node ('x', Empty, Empty),
 *   Node ('x', Node ('x', Empty, Empty), Empty));
 *  Node ('x', Node ('x', Empty, Empty),
 *   Node ('x', Empty, Node ('x', Empty, Empty)));
 *  Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
 *   Node ('x', Empty, Empty));
 *  Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
 *   Node ('x', Empty, Empty))]
 * # List.length(cbal_tree 40);;
 * - : int = 524288
 *
 *******************************************************************************
 *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec cbal_tree_0 = function
  | 0 -> [Empty]
  | 1 -> [Node ('x', Empty, Empty)]
  | n ->
      let rev_append l1 l2 =
        let rec loop acc l1 l2 =
          match l1, l2 with
          | [], [] -> acc
          | [], h :: t -> loop (h :: acc) [] t
          | h :: t, l -> loop (h :: acc) t l
          in
          loop [] l1 l2
      in

      let merge_tree tree_a tree_b =
        let rec merge_base acc base = function
          | [] -> acc
          | node :: rest ->
              merge_base (Node ('x', node, base) :: acc) base rest
        in
        let rec merge acc = function
          | [] -> acc
          | node :: rest ->
              merge (merge_base acc node tree_a) rest
        in
        List.rev @@ merge [] tree_b
      in

      if n mod 2 = 1 then
        let sub_tree = cbal_tree_0 ((n-1)/2) in
        merge_tree sub_tree sub_tree
      else
        let sub_tree0 = cbal_tree_0 (n/2) in
        let sub_tree1 = cbal_tree_0 ((n-2)/2) in
        let left = merge_tree sub_tree0 sub_tree1 in
        let right = merge_tree sub_tree1 sub_tree0 in
        rev_append left right
;;

let rec cbal_tree_1 n =
  let add_trees_with left right all =
    let add_right_tree all l =
      List.fold_left (fun a r -> Node('x', l, r) :: a) all right in
    List.fold_left add_right_tree all left
  in

  if n = 0 then
    [Empty]
  else if n mod 2 = 1 then
    let t = cbal_tree_1 (n / 2) in
    add_trees_with t t []
  else (* n even: n-1 nodes for the left & right subtrees altogether. *)
    let t1 = cbal_tree_1 (n / 2 - 1) in
    let t2 = cbal_tree_1 (n / 2) in
    add_trees_with t1 t2 (add_trees_with t2 t1 [])
;;

let cbal_tree = cbal_tree_1

let () =
  assert (
    cbal_tree 4
    = [Node ('x', Node ('x', Empty, Empty),
        Node ('x', Node ('x', Empty, Empty), Empty));
       Node ('x', Node ('x', Empty, Empty),
        Node ('x', Empty, Node ('x', Empty, Empty)));
       Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
        Node ('x', Empty, Empty));
       Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
        Node ('x', Empty, Empty))]
  );

  assert (List.length(cbal_tree 40) = 524288);
;;

