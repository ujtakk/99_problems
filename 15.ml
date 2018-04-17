(*
 * 15. Replicate the elements of a list a given number of times. (*medium*)
 *******************************************************************************
 *
 * # replicate ["a";"b";"c"] 3;;
 * - : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
 *
 *******************************************************************************
 *)

let replicate_0 l n =
  let copy value length target =
    let rec iter_copy acc = function
      | 0 -> acc
      | i -> iter_copy (value :: acc) (i-1)
    in
    iter_copy target length
  in
  let rec iter acc = function
    | [] -> acc
    | h :: t -> iter (copy h n acc) t
  in
  iter [] (List.rev l)
;;

let replicate = replicate_0

let () =
  assert (
    replicate ["a";"b";"c"] 3
    = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"]
  );
;;
