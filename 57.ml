(*
 * 57. Binary search trees (dictionaries). (*medium*)
 *******************************************************************************
 *
 * Construct a [binary search
 * tree](http://en.wikipedia.org/wiki/Binary_search_tree) from a list of
 * integer numbers.
 *
 * # construct [3;2;5;7;1];;
 * - : int binary_tree =
 * Node (3, Node (2, Node (1, Empty, Empty), Empty),
 *  Node (5, Empty, Node (7, Empty, Empty)))
 *
 * Then use this function to test the solution of the previous problem.
 *
 * # is_symmetric(construct [5;3;18;1;4;12;21]);;
 * - : bool = true
 * # not(is_symmetric(construct [3;2;5;7;4]));;
 * - : bool = true
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

let construct_0 set =
  let rec insert value = function
    | Empty -> Node (value, Empty, Empty)
    | Node (base, lower, upper) ->
        if base < value then
          Node (base, lower, insert value upper)
        else
          Node (base, insert value lower, upper)
  in
  let rec iter tree = function
    | [] -> tree
    | value :: rest -> iter (insert value tree) rest
  in
  iter Empty set
;;

let construct = construct_0

let () =
  assert (
    construct [3;2;5;7;1]
    = Node (3, Node (2, Node (1, Empty, Empty), Empty),
       Node (5, Empty, Node (7, Empty, Empty)))
  );

  assert (is_symmetric(construct [5;3;18;1;4;12;21]));
  assert (not(is_symmetric(construct [3;2;5;7;4])));
;;

