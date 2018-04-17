(*
 * 7. Flatten a nested list structure. (*medium*)
 *******************************************************************************
 *
 * # (* There is no nested list type in OCaml, so we need to define one
 *      first. A node of a nested list is either an element, or a list of
 *      nodes. *)
 *   type 'a node =
 *     | One of 'a
 *     | Many of 'a node list;;
 * type 'a node = One of 'a | Many of 'a node list
 *
 * # flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ];;
 * - : string list = ["a"; "b"; "c"; "d"; "e"]
 *
 *******************************************************************************
 *)

type 'a node =
  | One of 'a
  | Many of 'a node list
;;

let flatten_0 =
  let rec iter acc = function
    | [] -> acc
    | One h :: t -> iter (acc @ [h]) t
    | Many h :: t -> iter (acc @ (iter [] h)) t
  in
  iter []
;;

let flatten = flatten_0

let () =
  let show_list l =
    let rec show = function
      | [] -> ""
      | [x] -> "\""^x^"\""
      | h :: t -> "\""^h^"\"; "^(show t)
    in
    "["^(show l)^"]"
  in

  flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]
  |> show_list
  |> print_endline;
  assert (flatten [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ] = ["a"; "b"; "c"; "d"; "e"]);
