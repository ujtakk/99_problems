(*
 * 81. Path from one node to another one. (*medium*)
 *******************************************************************************
 *
 * Write a function `paths g a b` that returns all acyclic path `p` from
 * node `a` to node `b ≠ a` in the graph `g`. The function should return
 * the list of all paths via backtracking.
 *
 * # paths example_graph 'f' 'b';;
 * - : char list list = [['f'; 'c'; 'b']; ['f'; 'b']]
 *
 *******************************************************************************
 *)

type 'a graph_term = { nodes : 'a list;  edges : ('a * 'a) list }

let rec paths_0 graph st en =
  let rec iter acc st en = function
    | [] -> List.rev acc
    | (a, b) :: t ->
        iter (acc @ List.map (fun p -> a :: p) (paths_0 graph b en)) st en t
  in
  if List.mem st graph.nodes && List.mem en graph.nodes then
    if st = en then
      [[en]]
    else
      let cand = List.filter (fun t -> let a, b = t in a = st) graph.edges in
      iter [] st en cand
  else
    []
;;

let paths = paths_0

let () =
  let example_graph =
    { nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
      edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] }
  in
  assert (paths example_graph 'f' 'b' = [['f'; 'c'; 'b']; ['f'; 'b']]);
;;

