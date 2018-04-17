(*
 * 18. Extract a slice from a list. (*medium*)
 *******************************************************************************
 *
 * Given two indices, `i` and `k`, the slice is the list containing the
 * elements between the `i`'th and `k`'th element of the original list
 * (both limits included). Start counting the elements with 0 (this is the
 * way the `List` module numbers elements).
 *
 * # slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6;;
 * - : string list = ["c"; "d"; "e"; "f"; "g"]
 *
 *******************************************************************************
 *)

let slice_0 lst f l =
  let rec iter f_count l_count acc = function
    | [] -> acc
    | h :: t -> match f_count, l_count with
                | 0, 0 -> h :: acc
                | 0, n -> iter 0 (n-1) (h :: acc) t
                | m, n -> iter (m-1) (n-1) acc t
  in
  List.rev @@ iter f l [] lst
;;

let slice = slice_0

let () =
  assert (
    slice ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 2 6
    = ["c"; "d"; "e"; "f"; "g"]
  );
;;
