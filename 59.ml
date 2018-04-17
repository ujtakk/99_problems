(*
 * 59. Construct height-balanced binary trees. (*medium*)
 *******************************************************************************
 *
 * In a height-balanced binary tree, the following property holds for every
 * node: The height of its left subtree and the height of its right subtree
 * are almost equal, which means their difference is not greater than one.
 *
 * Write a function `hbal_tree` to construct height-balanced binary trees
 * for a given height. The function should generate all solutions via
 * backtracking. Put the letter `'x'` as information into all nodes of the
 * tree.
 *
 * # let t = hbal_tree 3;;
 * val t : char binary_tree list =
 *   [Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
 *     Node ('x', Empty, Node ('x', Empty, Empty)));
 *    Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
 *     Node ('x', Node ('x', Empty, Empty), Empty));
 *    Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
 *     Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
 *    Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
 *     Node ('x', Empty, Node ('x', Empty, Empty)));
 *    Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
 *     Node ('x', Node ('x', Empty, Empty), Empty));
 *    Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
 *     Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
 *    Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
 *     Node ('x', Empty, Node ('x', Empty, Empty)));
 *    Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
 *     Node ('x', Node ('x', Empty, Empty), Empty));
 *    Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
 *     Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
 *    Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
 *     Node ('x', Empty, Empty));
 *    Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
 *     Node ('x', Empty, Empty));
 *    Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
 *     Node ('x', Empty, Empty));
 *    Node ('x', Node ('x', Empty, Empty),
 *     Node ('x', Empty, Node ('x', Empty, Empty)));
 *    Node ('x', Node ('x', Empty, Empty),
 *     Node ('x', Node ('x', Empty, Empty), Empty));
 *    Node ('x', Node ('x', Empty, Empty),
 *     Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)))]
 * # let x = 'x';;
 * val x : char = 'x'
 * # List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
 *                  Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)))) t;;
 * - : bool = true
 * # List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
 *                  Node(x, Node(x, Empty, Empty), Empty))) t;;
 * - : bool = true
 * # List.length t;;
 * - : int = 15
 *
 *******************************************************************************
 *)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

let rec remove base = function
  | [] -> base
  | h :: t ->
    let rec iter acc = function
      | [] -> List.rev acc
      | h' :: t' ->
          if h' = h then
            iter acc t'
          else
            iter (h' :: acc) t'
    in
    let base_wo_h = iter [] base in
    remove base_wo_h t
;;

let rec base_tree = function
  | 0 -> Empty
  | n -> Node ('x', base_tree (n-1), base_tree (n-1))
;;

let rec hbal_tree_0 = function
  | 0 -> [Empty]
  | 1 -> [Node ('x', Empty, Empty)]
  | n ->
      let join set_x set_y =
        let rec iter_join acc set = function
          | [] -> acc
          | h :: t ->
              let join_map = List.map (fun y -> (h, y)) set in
              iter_join (acc @ join_map) set t
        in
        iter_join [] set_y set_x
      in
      let rec iter n acc = function
        | [] -> acc
        | (a, b) :: t when a = b && b = base_tree (n-2) -> iter n acc t
        | (a, b) :: t -> iter n (Node ('x', a, b) :: acc) t
      in
      let sub_tree = hbal_tree_0 (n-2) @ hbal_tree_0 (n-1) in
      iter n [] (join sub_tree sub_tree)
;;

let hbal_tree = hbal_tree_0

let () =
  let t = hbal_tree 3 in
  (*
  assert (
    t
    = [Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
        Node ('x', Empty, Node ('x', Empty, Empty)));
       Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
        Node ('x', Node ('x', Empty, Empty), Empty));
       Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
        Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
       Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
        Node ('x', Empty, Node ('x', Empty, Empty)));
       Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
        Node ('x', Node ('x', Empty, Empty), Empty));
       Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
        Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
       Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
        Node ('x', Empty, Node ('x', Empty, Empty)));
       Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
        Node ('x', Node ('x', Empty, Empty), Empty));
       Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
        Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)));
       Node ('x', Node ('x', Empty, Node ('x', Empty, Empty)),
        Node ('x', Empty, Empty));
       Node ('x', Node ('x', Node ('x', Empty, Empty), Empty),
        Node ('x', Empty, Empty));
       Node ('x', Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)),
        Node ('x', Empty, Empty));
       Node ('x', Node ('x', Empty, Empty),
        Node ('x', Empty, Node ('x', Empty, Empty)));
       Node ('x', Node ('x', Empty, Empty),
        Node ('x', Node ('x', Empty, Empty), Empty));
       Node ('x', Node ('x', Empty, Empty),
        Node ('x', Node ('x', Empty, Empty), Node ('x', Empty, Empty)))]
  );
  *)

  let x = 'x' in
  assert (
    List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
                   Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)))) t
  );
  assert (
    List.mem (Node(x, Node(x, Node(x, Empty, Empty), Node(x, Empty, Empty)),
                   Node(x, Node(x, Empty, Empty), Empty))) t
  );

  assert (List.length t = 15);
;;

