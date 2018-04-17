(*
 * 5. Reverse a list. (*easy*)
 *******************************************************************************
 *
 * OCaml standard library has `List.rev` but we ask that you reimplement
 * it.
 *
 * # rev ["a" ; "b" ; "c"];;
 * - : string list = ["c"; "b"; "a"]
 *
 *******************************************************************************
 *)

let rec rev_0 = function
  | [] -> []
  | [x] -> [x]
  | h :: t -> (rev_0 t) @ [h]
;;

let rev_1 =
  let rec iter acc = function
    | [] -> acc
    | h :: t -> iter (h :: acc) t
  in
  iter []
;;

let rev = rev_1

let () =
  let show_list l =
    let rec show = function
      | [] -> ""
      | [x] -> "\""^x^"\""
      | h :: t -> "\""^h^"\"; "^(show t)
    in
    "["^(show l)^"]"
  in

  rev ["a" ; "b" ; "c"]
  |> show_list
  |> print_endline;
  assert (rev ["a" ; "b" ; "c"] = ["c"; "b"; "a"]);
;;
