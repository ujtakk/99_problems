(*
 * 71. Determine the internal path length of a tree. (*easy*)
 *******************************************************************************
 *
 * We define the internal path length of a multiway tree as the total sum
 * of the path lengths from the root to all nodes of the tree. By this
 * definition, the tree `t` in the figure of the previous problem has an
 * internal path length of 9. Write a function `ipl tree` that returns the
 * internal path length of `tree`.
 *
 * # ipl t;;
 * - : int = 9
 *
 *******************************************************************************
 *)

type 'a mult_tree = T of 'a * 'a mult_tree list

let rec count_nodes = function
  | T (_, trees) ->
    let rec iter acc = function
      | [] -> acc
      | h :: t -> iter (acc + count_nodes h) t
    in
    iter 1 trees
;;

let rec ipl_0 = function
  | T (_, trees) ->
      let rec iter acc = function
        | [] -> acc
        | h :: t -> iter (acc + count_nodes h + ipl_0 h) t
      in
      iter 0 trees
;;

let ipl = ipl_0

let () =
  let t = T('a', [T('f',[T('g',[])]); T('c',[]);
            T('b',[T('d',[]); T('e',[])])])
  in
  assert (ipl t = 9);
;;

