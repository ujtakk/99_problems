(*
 * 82. Cycle from a given node. (*easy*)
 *******************************************************************************
 *
 * Write a functions `cycle g a` that returns a closed path (cycle) `p`
 * starting at a given node `a` in the graph `g`. The predicate should
 * return the list of all cycles via backtracking.
 *
 * # cycles example_graph 'f';;
 * - : char list list =
 * [['f'; 'b'; 'c'; 'f']; ['f'; 'c'; 'f']; ['f'; 'c'; 'b'; 'f'];
 *  ['f'; 'b'; 'f']; ['f'; 'k'; 'f']]
 *
 *******************************************************************************
 *)

type 'a graph_term = { nodes : 'a list;  edges : ('a * 'a) list }

let rec paths graph base st en =
  let rec iter acc st en = function
    | [] -> List.rev acc
    | (a, b) :: t ->
        let rest = paths graph base b en in
        iter (acc @ (List.map (fun p -> a :: p) rest)) st en t
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

let cycles_0 graph p =
  let rec iter graph acc = function
    | [] -> acc
    | (a, b) :: t ->
        iter graph (acc @ (List.map (fun l -> a :: l) (paths graph a b p))) t
  in
  if List.mem p graph.nodes then
    let und_edges =
      graph.edges @ (List.map (fun t -> let a, b = t in (b, a)) graph.edges)
    in
    let cand = List.filter (fun t -> let a, b = t in a = p) und_edges in
    let und_graph = { nodes = graph.nodes; edges = und_edges } in
    iter und_graph [] cand
  else
    []
;;

let example_graph =
  { nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
    edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] }
;;

let und_edges = example_graph.edges
              @ (List.map (fun t -> let a, b = t in (b, a)) example_graph.edges)
;;

let und_graph = { nodes = example_graph.nodes; edges = und_edges };;

let cycles = cycles_0

let () =
  let example_graph =
    { nodes = ['b'; 'c'; 'd'; 'f'; 'g'; 'h'; 'k'];
      edges = ['h', 'g';  'k', 'f';  'f', 'b';  'f', 'c';  'c', 'b'] }
  in
  assert (
    cycles example_graph 'f'
    = [['f'; 'b'; 'c'; 'f']; ['f'; 'c'; 'f']; ['f'; 'c'; 'b'; 'f'];
       ['f'; 'b'; 'f']; ['f'; 'k'; 'f']]
  );
;;

