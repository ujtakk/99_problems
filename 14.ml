(*
 * 14. Duplicate the elements of a list. (*easy*)
 *******************************************************************************
 *
 * # duplicate ["a";"b";"c";"c";"d"];;
 * - : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
 *
 *******************************************************************************
 *)

let rec duplicate_0 = function
  | [] -> []
  | [x] -> [x; x]
  | h :: t -> h :: h :: duplicate_0 t
;;

let duplicate_1 l =
  let rec iter acc = function
    | [] -> acc
    | h :: t -> iter (h :: h :: acc) t
  in
  List.rev @@ iter [] l
;;

let duplicate = duplicate_1

let () =
  assert (
    duplicate ["a";"b";"c";"c";"d"]
    = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"]
  );
;;
