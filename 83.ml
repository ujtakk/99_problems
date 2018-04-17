(*
 * 83. Construct all spanning trees. (*medium*)
 *******************************************************************************
 *
 * ![](../../img/spanning-tree-graph1.gif "Spanning tree graph")
 *
 * Write a function `s_tree g` to construct (by backtracking) all [spanning
 * trees](http://en.wikipedia.org/wiki/Spanning_tree) of a given graph `g`.
 * With this predicate, find out how many spanning trees there are for the
 * graph depicted to the left. The data of this example graph can be found
 * in the test below. When you have a correct solution for the `s_tree`
 * function, use it to define two other useful functions: `is_tree graph`
 * and `is_connected Graph`. Both are five-minutes tasks!
 *
 * # let g = { nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
 *             edges = [('a', 'b'); ('a', 'd'); ('b', 'c'); ('b', 'e');
 *                      ('c', 'e'); ('d', 'e'); ('d', 'f'); ('d', 'g');
 *                      ('e', 'h'); ('f', 'g'); ('g', 'h')] };;
 * val g : char graph_term =
 *   {nodes = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'];
 *    edges =
 *     [('a', 'b'); ('a', 'd'); ('b', 'c'); ('b', 'e'); ('c', 'e'); ('d', 'e');
 *      ('d', 'f'); ('d', 'g'); ('e', 'h'); ('f', 'g'); ('g', 'h')]}
 *
 *******************************************************************************
 *)

let () =
;;

