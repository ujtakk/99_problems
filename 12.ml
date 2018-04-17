(*
 * 12. Decode a run-length encoded list. (*medium*)
 *******************************************************************************
 *
 * Given a run-length code list generated as specified in the previous
 * problem, construct its uncompressed version.
 *
 * # decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")];;
 * - : string list =
 * ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
 *
 *******************************************************************************
 *)

type 'a rle =
  | One of 'a
  | Many of int * 'a
;;

let decode_0 l =
  let rec iter acc = function
    | [] -> acc
    | One s :: t -> iter (s :: acc) t
    | Many (i, s) :: t -> if i = 1 then
                            iter (s :: acc) t
                          else
                            iter (s :: acc) (Many (i-1, s) :: t)
  in
  List.rev @@ iter [] l
;;

let decode = decode_0

let () =
  assert (
    decode [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]
    = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
  );
;;
