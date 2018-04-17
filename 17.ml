(*
 * 17. Split a list into two parts; the length of the first part is given. (*easy*)
 *******************************************************************************
 *
 * If the length of the first part is longer than the entire list, then the
 * first part is the list and the second part is empty.
 *
 * # split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
 * - : string list * string list =
 * (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
 * # split ["a";"b";"c";"d"] 5;;
 * - : string list * string list = (["a"; "b"; "c"; "d"], [])
 *
 *******************************************************************************
 *)

let split_0 lst pos =
  let rec iter count acc = function
    | [] -> acc, []
    | h :: t -> if count = 1 then
                  h :: acc, t
                else
                  iter (count-1) (h :: acc) t
  in
  let left, right = iter pos [] lst in
  List.rev left, right
;;

let split = split_0

let () =
  assert (
    split ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
    = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
  );

  assert (
    split ["a";"b";"c";"d"] 5
    = (["a"; "b"; "c"; "d"], [])
  );
;;
