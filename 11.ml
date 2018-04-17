(*
 * 11. Modified run-length encoding. (*easy*)
 *******************************************************************************
 *
 * Modify the result of the previous problem in such a way that if an
 * element has no duplicates it is simply copied into the result list. Only
 * elements with duplicates are transferred as (N E) lists.
 *
 * Since OCaml lists are homogeneous, one needs to define a type to hold
 * both single elements and sub-lists.
 *
 *     # type 'a rle =
 *         | One of 'a
 *         | Many of int * 'a;;
 *     type 'a rle = One of 'a | Many of int * 'a
 *
 *     # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 *     - : string rle list =
 *     [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 *      Many (4, "e")]
 *
 *******************************************************************************
 *)

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let encode_0 l =
  let entry c x = if c = 0 then One x else Many (c+1, x) in
  let rec iter count acc = function
    | [] -> []
    | [x] -> entry count x :: acc
    | a :: (b :: _ as t) -> if a = b then
                              iter (count+1) acc t
                            else
                              iter 0 ((entry count a) :: acc) t
  in
  List.rev @@ iter 0 [] l
;;

let encode = encode_0

let () =
  assert (
    encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
    = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
       Many (4, "e")]
  );
;;
