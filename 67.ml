(*
 * 67. A string representation of binary trees. (*medium*)
 *******************************************************************************
 *
 * ![](../../img/binary-tree.gif "Binary Tree")
 *
 * Somebody represents binary trees as strings of the following type (see
 * example): `"a(b(d,e),c(,f(g,)))"`.
 *
 * - Write an OCaml function `string_of_tree` which generates this string
 *   representation, if the tree is given as usual (as `Empty` or
 *   `Node(x,l,r)` term). Then write a function `tree_of_string` which
 *   does this inverse; i.e. given the string representation, construct
 *   the tree in the usual form. Finally, combine the two predicates in a
 *   single function `tree_string` which can be used in both directions.
 * - Write the same predicate `tree_string` using difference lists and a
 *   single predicate `tree_dlist` which does the conversion between a
 *   tree and a difference list in both directions.
 *
 * For simplicity, suppose the information in the nodes is a single letter
 * and there are no spaces in the string.
 *
 * # let example_layout_tree =
 *     let leaf x = Node (x, Empty, Empty) in
 *     Node('a', Node('b', leaf 'd', leaf 'e'),
 *     Node('c', Empty, Node('f', leaf 'g', Empty)));;
 * val example_layout_tree : char binary_tree =
 *   Node ('a', Node ('b', Node ('d', Empty, Empty), Node ('e', Empty, Empty)),
 *    Node ('c', Empty, Node ('f', Node ('g', Empty, Empty), Empty)))
 * # string_of_tree example_layout_tree;;
 * - : string = "a(b(d,e),c(,f(g,)))"
 * # tree_of_string "a(b(d,e),c(,f(g,)))" = example_layout_tree;;
 * - : bool = true
 * # tree_of_string "";;
 * - : char binary_tree = Empty
 *
 *******************************************************************************
 *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec string_of_tree_0 = function
  | Empty -> ""
  | Node (id, Empty, Empty) ->
      (Char.escaped id)
  | Node (id, a, b) ->
      (Char.escaped id)^"("^(string_of_tree_0 a)^","^(string_of_tree_0 b)^")"
;;

let tree_of_string_0 s =
  let rec parse str ptr =
    match str.[ptr] with
    | ',' | ')' -> Empty, ptr
    | id ->
        let base_ptr = ptr + 1 in
        match str.[base_ptr] with
        | ',' | ')' -> Node (id, Empty, Empty), base_ptr
        | '(' ->
            let left_tree, left_ptr = parse str (base_ptr+1) in
            let right_tree, right_ptr = parse str (left_ptr+1) in
            Node (id, left_tree, right_tree), right_ptr+1
        | _ -> raise Exit
  in
  match String.length s with
  | 0 -> Empty
  | 1 -> Node (s.[0], Empty, Empty)
  | _ -> let tree, ptr = parse s 0 in tree
;;

let string_of_tree = string_of_tree_0
let tree_of_string = tree_of_string_0

let () =
  let example_layout_tree =
    let leaf x = Node (x, Empty, Empty) in
    Node('a', Node('b', leaf 'd', leaf 'e'),
    Node('c', Empty, Node('f', leaf 'g', Empty)))
  in
  assert (string_of_tree example_layout_tree = "a(b(d,e),c(,f(g,)))");
  assert (tree_of_string "a(b(d,e),c(,f(g,)))" = example_layout_tree);
  assert (tree_of_string "" = Empty);
;;

