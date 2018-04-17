(*
 * 13. Run-length encoding of a list (direct solution). (*medium*)
 *******************************************************************************
 *
 * Implement the so-called run-length encoding data compression method
 * directly. I.e. don't explicitly create the sublists containing the
 * duplicates, as in problem "[Pack consecutive duplicates of list elements
 * into
 * sublists](#Packconsecutiveduplicatesoflistelementsintosublistsmedium)",
 * but only count them. As in problem "[Modified run-length
 * encoding](#Modifiedrunlengthencodingeasy)", simplify the result list by
 * replacing the singleton lists (1 X) by X.
 *
 * # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 * - : string rle list =
 * [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 *  Many (4, "e")]
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
