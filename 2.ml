(*
 * 2. Find the last but one (last and penultimate) elements of a list. (*easy*)
 *******************************************************************************
 *
 * # last_two [ "a" ; "b" ; "c" ; "d" ];;
 * - : (string * string) option = Some ("c", "d")
 * # last_two [ "a" ];;
 * - : (string * string) option = None
 *
 *******************************************************************************
 *)

let rec last_two_0 = function
  | [] -> None
  | x :: [] -> None
  | x :: y :: [] -> Some (x, y)
  | _ :: t -> last_two_0 t
;;

let rec last_two_1 = function
  | []
  | [_] -> None
  | [x; y] -> Some (x, y)
  | _ :: t -> last_two_1 t
;;

let last_two = last_two_0

let () =
  let show_option = function
    | None -> "None"
    | Some (s, t) -> "Some (" ^ s ^ ", " ^ t ^ ")"
  in

  last_two [ "a" ; "b" ; "c" ; "d" ]
  |> show_option
  |> print_endline;
  assert (last_two [ "a" ; "b" ; "c" ; "d" ] = Some ("c", "d"));

  last_two [ "a" ]
  |> show_option
  |> print_endline;
  assert (last_two [ "a" ] = None);
;;
