(*
 * 73. Lisp-like tree representation. (*medium*)
 *******************************************************************************
 *
 * There is a particular notation for multiway trees in Lisp. The picture
 * shows how multiway tree structures are represented in Lisp.
 *
 * ![](../../img/lisp-like-tree.png "Lisp representation of multiway
 * trees")
 *
 * Note that in the "lispy" notation a node with successors (children) in
 * the tree is always the first element in a list, followed by its
 * children. The "lispy" representation of a multiway tree is a sequence of
 * atoms and parentheses '(' and ')'. This is very close to the way trees
 * are represented in OCaml, except that no constructor `T` is used. Write
 * a function `lispy : char mult_tree -> string` that returns the lispy
 * notation of the tree.
 *
 * # lispy (T('a', []));;
 * - : string = "a"
 * # lispy (T('a', [T('b', [])]));;
 * - : string = "(a b)"
 * # lispy t;;
 * - : string = "(a (f g) c (b d e))"
 *
 *******************************************************************************
 *)

type 'a mult_tree = T of 'a * 'a mult_tree list

let rec lispy_0 = function
  | T (id, trees) -> begin
      match trees with
      | [] -> Char.escaped id
      | _ ->
        let sub_sexp = String.concat " " (List.map lispy_0 trees) in
        "("^(Char.escaped id)^" "^sub_sexp^")"
  end
;;

let lispy = lispy_0

let () =
  assert (lispy (T('a', [])) = "a");
  assert (lispy (T('a', [T('b', [])])) = "(a b)");

  let t = T('a', [T('f',[T('g',[])]); T('c',[]);
            T('b',[T('d',[]); T('e',[])])])
  in
  assert (lispy t = "(a (f g) c (b d e))");
;;

