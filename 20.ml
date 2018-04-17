(*
 * 20. Remove the K'th element from a list. (*easy*)
 *******************************************************************************
 *
 * The first element of the list is numbered 0, the second 1,...
 *
 * # remove_at 1 ["a";"b";"c";"d"];;
 * - : string list = ["a"; "c"; "d"]
 *
 *******************************************************************************
 *)

let remove_at_0 idx lst =
  let rec iter count acc = function
    | [] -> acc
    | h :: t -> if count = 0 then
                  List.rev acc @ t
                else
                  iter (count-1) (h :: acc) t
  in
  iter idx [] lst
;;

let remove_at = remove_at_0

let () =
  assert (
    remove_at 1 ["a";"b";"c";"d"]
    = ["a"; "c"; "d"]
  );
;;
