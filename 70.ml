(*
 * 70. Tree construction from a node string. (*medium*)
 *******************************************************************************
 *
 * ![](../../img/multiway-tree.gif "Multiway Tree")
 *
 * We suppose that the nodes of a multiway tree contain single characters.
 * In the depth-first order sequence of its nodes, a special character `^`
 * has been inserted whenever, during the tree traversal, the move is a
 * backtrack to the previous level.
 *
 * By this rule, the tree in the figure opposite is represented as:
 * `afg^^c^bd^e^^^`.
 *
 * Write functions `string_of_tree : char mult_tree -> string` to construct
 * the string representing the tree and `tree_of_string : string -> char
 * mult_tree` to construct the tree when the string is given.
 *
 * # let t = T('a', [T('f',[T('g',[])]); T('c',[]);
 *             T('b',[T('d',[]); T('e',[])])]);;
 * val t : char mult_tree =
 *   T ('a',
 *    [T ('f', [T ('g', [])]); T ('c', []); T ('b', [T ('d', []); T ('e', [])])])
 * # string_of_tree t;;
 * - : string = "afg^^c^bd^e^^^"
 * # tree_of_string "afg^^c^bd^e^^^";;
 * - : char mult_tree =
 * T ('a',
 *  [T ('f', [T ('g', [])]); T ('c', []); T ('b', [T ('d', []); T ('e', [])])])
 *
 *******************************************************************************
 *)

type 'a mult_tree = T of 'a * 'a mult_tree list

let rec string_of_tree_0 = function
  | T (id, trees) ->
      let rec iter acc = function
        | [] -> acc
        | h :: t -> iter (acc ^ string_of_tree_0 h) t
      in
      Char.escaped id ^ iter "" trees ^ "^"
;;

let tree_of_string_0 s =
  let rec parse str ptr =
    match str.[ptr] with
    | id ->
        let base_ptr = ptr + 1 in
        let rec iter acc ptr =
          match str.[ptr] with
          | '^' ->
              T(id, List.rev acc), ptr
          | id ->
              let tree, new_ptr = parse str ptr in
              iter (tree :: acc) (new_ptr+1)
        in
        iter [] base_ptr

  in
  match String.length s with
  | 0 | 1 -> raise (Invalid_argument "tree must be enclosed")
  | _ -> let tree, ptr = parse s 0 in tree
;;

let string_of_tree = string_of_tree_0
let tree_of_string = tree_of_string_0

let () =
  let t = T('a', [T('f',[T('g',[])]); T('c',[]);
            T('b',[T('d',[]); T('e',[])])])
  in
  assert (string_of_tree t = "afg^^c^bd^e^^^");
  assert (
    tree_of_string "afg^^c^bd^e^^^"
    = T ('a',
       [T ('f', [T ('g', [])]); T ('c', []); T ('b', [T ('d', []); T ('e', [])])])
  );
;;

