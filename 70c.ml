(*
 * ## Multiway Trees
 *
 * ![](../../img/multiway-tree.gif "Multiway Tree")
 *
 * *A multiway tree is composed of a root element and a (possibly empty)
 * set of successors which are multiway trees themselves. A multiway tree
 * is never empty. The set of successor trees is sometimes called a
 * forest.*
 *
 * To represent multiway trees, we will use the following type which is a
 * direct translation of the definition:
 *
 * # type 'a mult_tree = T of 'a * 'a mult_tree list;;
 * type 'a mult_tree = T of 'a * 'a mult_tree list
 *
 * The example tree depicted opposite is therefore represented by the
 * following OCaml expression:
 *
 * # T('a', [T('f',[T('g',[])]); T('c',[]); T('b',[T('d',[]); T('e',[])])]);;
 * - : char mult_tree =
 * T ('a',
 *  [T ('f', [T ('g', [])]); T ('c', []); T ('b', [T ('d', []); T ('e', [])])])
 *
 * 70C. Count the nodes of a multiway tree. (*easy*)
 *******************************************************************************
 *
 * # count_nodes (T('a', [T('f',[]) ]));;
 * - : int = 2
 *
 *******************************************************************************
 *)

type 'a mult_tree = T of 'a * 'a mult_tree list

let rec count_nodes_0 = function
  | T (_, trees) ->
      let rec iter acc = function
        | [] -> acc
        | h :: t -> iter (acc + count_nodes_0 h) t
      in
      iter 1 trees
;;

let count_nodes = count_nodes_0

let () =
  assert (count_nodes (T('a', [T('f',[]) ])) = 2);
;;

