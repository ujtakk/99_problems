(*
 * 3. Find the `k`'th element of a list. (*easy*)
 *******************************************************************************
 *
 * # at 3 [ "a" ; "b"; "c"; "d"; "e" ];;
 * - : string option = Some "c"
 * # at 3 [ "a" ];;
 * - : string option = None
 *
 * REMARK: OCaml has `List.nth` which numbers elements from `0` and raises
 * an exception if the index is out of bounds.
 *
 * # List.nth [ "a" ; "b"; "c"; "d"; "e" ] 2;;
 * - : string = "c"
 * # List.nth [ "a" ] 2;;
 * Exception: Failure "nth".
 *
 *******************************************************************************
 *)

let rec at_0 n = function
  | [] -> None
  | h :: t ->
      if n = 1 then
        Some h
      else
        at_0 (n-1) t
;;

let at_1 n l =
  try
    Some (List.nth l (n-1))
  with
    _ -> None
;;

let at = at_0

let () =
  let show_option = function
    | None -> "None"
    | Some s -> "Some " ^ s
  in

  at 3 [ "a" ; "b"; "c"; "d"; "e" ]
  |> show_option
  |> print_endline;
  assert (at 3 [ "a" ; "b"; "c"; "d"; "e" ] = Some "c");

  at 3 [ "a" ]
  |> show_option
  |> print_endline;
  assert (at 3 [ "a" ] = None);
;;
