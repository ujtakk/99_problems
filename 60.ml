(*
 * 60. Construct height-balanced binary trees with a given number of nodes. (*medium*)
 *******************************************************************************
 *
 * Consider a height-balanced binary tree of height `h`. What is the
 * maximum number of nodes it can contain? Clearly,
 * max\_nodes = 2<sup>`h`</sup> - 1.
 *
 * # let max_nodes h = 1 lsl h - 1;;
 * val max_nodes : int -> int = <fun>
 *
 * However, what is the minimum number min\_nodes? This question is more
 * difficult. Try to find a recursive statement and turn it into a function
 * `min_nodes` defined as follows: `min_nodes h` returns the minimum number
 * of nodes in a height-balanced binary tree of height `h`.
 *
 * On the other hand, we might ask: what are the minimum (resp. maximum)
 * height H a height-balanced binary tree with N nodes can have?
 * `min_height` (resp. `max_height n`) returns the minimum (resp. maximum)
 * height of a height-balanced binary tree with `n` nodes.
 *
 * Now, we can attack the main problem: construct all the height-balanced
 * binary trees with a given number of nodes. `hbal_tree_nodes n` returns a
 * list of all height-balanced binary tree with `n` nodes.
 *
 * Find out how many height-balanced trees exist for `n = 15`.
 *
 * # List.length (hbal_tree_nodes 15);;
 * - : int = 1553
 * # List.map hbal_tree_nodes [0; 1; 2; 3];;
 * - : char binary_tree list list =
 * [[Empty]; [Node ('x', Empty, Empty)];
 *  [Node ('x', Node ('x', Empty, Empty), Empty);
 *   Node ('x', Empty, Node ('x', Empty, Empty))];
 *  [Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))]]
 *
 *******************************************************************************
 *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec hbal_tree n =
  let add_trees_with left right all =
    let add_right_tree all l =
      List.fold_left (fun a r -> Node('x', l, r) :: a) all right in
    List.fold_left add_right_tree all left
  in
  if n = 0 then [Empty]
  else if n = 1 then [Node('x', Empty, Empty)]
  else
  (* [add_trees_with left right trees] is defined in a question above. *)
    let t1 = hbal_tree (n - 1)
    and t2 = hbal_tree (n - 2) in
    add_trees_with t1 t1 (add_trees_with t1 t2 (add_trees_with t2 t1 []))
;;

let clog2 n =
  let rec power a = function
    | 0 -> 1
    | b -> a * power a (b-1)
  in
  let rec iter acc n =
    if power 2 acc <= n && n < power 2 (acc+1) then
      acc + 1
    else
      iter (acc+1) n
  in
  match n with
  | 0 -> 0
  | _ -> iter 0 n
;;

let rec count_nodes = function
  | Empty -> 0
  | Node (_, a, b) -> 1 + count_nodes a + count_nodes b
;;

let hbal_tree_nodes_0 n =
  let cand = (hbal_tree (clog2 n)) @ (hbal_tree (1+clog2 n)) in
  List.filter (fun t -> count_nodes t = n) cand
;;

let hbal_tree_nodes = hbal_tree_nodes_0

let () =
  assert (List.length (hbal_tree_nodes 15) = 1553);

  assert (
    List.map hbal_tree_nodes [0; 1; 2; 3]
    = [[Empty]; [Node ('x', Empty, Empty)];
       [Node ('x', Node ('x', Empty, Empty), Empty);
        Node ('x', Empty, Node ('x', Empty, Empty))];
       [Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty))]]
  );
;;

