(*
 * 4. Find the number of elements of a list. (*easy*)
 *******************************************************************************
 *
 * OCaml standard library has `List.length` but we ask that you reimplement
 * it. Bonus for a [tail recursive](http://en.wikipedia.org/wiki/Tail_call)
 * solution.
 *
 * # length [ "a" ; "b" ; "c"];;
 * - : int = 3
 * # length [];;
 * - : int = 0
 *
 *******************************************************************************
 *)

let rec length_0 = function
  | [] -> 0
  | h :: t -> 1 + (length_0 t)
;;

let length_1 =
  let rec iter accum = function
    | [] -> accum
    | h :: t -> iter (accum+1) t
  in
  iter 0
;;

let length = length_1

let () =
  length [ "a" ; "b" ; "c"]
  |> string_of_int
  |> print_endline;
  assert (length [ "a" ; "b" ; "c"] = 3);

  length []
  |> string_of_int
  |> print_endline;
  assert (length [] = 0);
;;
