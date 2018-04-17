(*
 * ## Working with lists
 *
 * 1. Write a function last : 'a list -> 'a option that returns the last element of a list. (*easy*)
 *******************************************************************************
 *
 * # last [ "a" ; "b" ; "c" ; "d" ];;
 * - : string option = Some "d"
 * # last [];;
 * - : 'a option = None
 *
 *******************************************************************************
 *)

let last_0 l =
  let len = List.length l in
  if len == 0 then
    None
  else
    Some (List.nth l (len-1))
;;

let rec last_1 = function
  | [] -> None
  | [x] -> Some x
  | _ :: t -> last_1 t
;;

let last : string list -> string option = last_1

let () =
  let show_option = function
    | None -> "None"
    | Some s -> "Some " ^ s
  in

  last [ "a" ; "b" ; "c" ; "d" ]
  |> show_option
  |> print_endline;
  (* Operator (==) and (!=) is for pointer, not for value. *)
  (* Use (=) and (<>) for value comparison.                *)
  assert (last [ "a" ; "b" ; "c" ; "d" ] = Some "d");

  last []
  |> show_option
  |> print_endline;
  assert (last [] = None);

  print_endline @@ show_option @@ last [];
;;

