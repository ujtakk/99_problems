(*
 * 87. Depth-first order graph traversal. (*medium*)
 *******************************************************************************
 *
 * Write a function that generates a [depth-first order graph
 * traversal](https://en.wikipedia.org/wiki/Depth-first_search) sequence.
 * The starting point should be specified, and the output should be a list
 * of nodes that are reachable from this starting point (in depth-first
 * order).
 *
 * Specifically, the graph will be provided by its [adjacency-list
 * representation](https://en.wikipedia.org/wiki/Adjacency_list) and you
 * must create a module `M` with the following signature:
 *
 * # module type GRAPH = sig
 *     type node = char
 *     type t
 *     val of_adjacency : (node * node list) list -> t
 *     val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a -> 'a
 *   end;;
 * module type GRAPH =
 *   sig
 *     type node = char
 *     type t
 *     val of_adjacency : (node * node list) list -> t
 *     val dfs_fold : t -> node -> ('a -> node -> 'a) -> 'a -> 'a
 *   end
 *
 * where `M.dfs_fold g n f a` applies `f` on the nodes of the graph `g` in
 * depth first order, starting with node `n`.
 *
 * # let g = M.of_adjacency
 *             ['u', ['v'; 'x'];
 *              'v',      ['y'];
 *              'w', ['z'; 'y'];
 *              'x',      ['v'];
 *              'y',      ['x'];
 *              'z',      ['z'];
 *             ];;
 * val g : M.t = <abstr>
 * # List.rev (M.dfs_fold g 'w' (fun acc c -> c :: acc) []);;
 * - : M.node list = ['w'; 'z'; 'y'; 'x'; 'v']
 *
 *******************************************************************************
 *)

let () =
;;

