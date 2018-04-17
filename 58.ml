(*
 * 58. Generate-and-test paradigm. (*medium*)
 *******************************************************************************
 *
 * Apply the generate-and-test paradigm to construct all symmetric,
 * completely balanced binary trees with a given number of nodes.
 *
 * # sym_cbal_trees 5;;
 * - : char binary_tree list =
 * [Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
 *   Node ('x', Empty, Node ('x', Empty, Empty)));
 *  Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
 *   Node ('x', Node ('x', Empty, Empty), Empty))]
 *
 * How many such trees are there with 57 nodes? Investigate about how many
 * solutions there are for a given number of nodes? What if the number is
 * even? Write an appropriate function.
 *
 * # List.length (sym_cbal_trees 57);;
 * - : int = 256
 * # List.map (fun n -> n, List.length(sym_cbal_trees n)) (range 10 20);;
 * - : (int * int) list =
 * [(10, 0); (11, 4); (12, 0); (13, 4); (14, 0); (15, 1); (16, 0); (17, 8);
 *  (18, 0); (19, 16); (20, 0)]
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

let is_mirror left right =
  equal left (reverse right)
;;

let is_symmetric = function
  | Empty -> true
  | Node (_, x0, y0) -> is_mirror x0 y0
;;

let rec cbal_tree n =
  let add_trees_with left right all =
    let add_right_tree all l =
      List.fold_left (fun a r -> Node('x', l, r) :: a) all right in
    List.fold_left add_right_tree all left
  in

  if n = 0 then
    [Empty]
  else if n mod 2 = 1 then
    let t = cbal_tree (n / 2) in
    add_trees_with t t []
  else (* n even: n-1 nodes for the left & right subtrees altogether. *)
    let t1 = cbal_tree (n / 2 - 1) in
    let t2 = cbal_tree (n / 2) in
    add_trees_with t1 t2 (add_trees_with t2 t1 [])
;;

let sym_cbal_trees_0 n =
  let cand = cbal_tree n in
  List.filter is_symmetric cand
;;

let sym_cbal_trees = sym_cbal_trees_0

let () =
  assert (
    sym_cbal_trees 5
    = [Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
        Node ('x', Empty, Node ('x', Empty, Empty)));
       Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
        Node ('x', Node ('x', Empty, Empty), Empty))]
  );

  assert (List.length (sym_cbal_trees 57) = 256);

  let rec range a b = match a, b with
    | x, y when x = y -> [x]
    | x, y when x < y -> x :: range (x+1) y
    | x, y when x > y -> x :: range (x-1) y
    | _ -> []
  in
  assert (
    List.map (fun n -> n, List.length(sym_cbal_trees n)) (range 10 20)
    = [(10, 0); (11, 4); (12, 0); (13, 4); (14, 0); (15, 1); (16, 0); (17, 8);
       (18, 0); (19, 16); (20, 0)]
  );
;;

