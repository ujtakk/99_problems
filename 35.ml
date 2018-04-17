(*
 * 35. Determine the prime factors of a given positive integer. (*medium*)
 *******************************************************************************
 *
 * Construct a flat list containing the prime factors in ascending order.
 *
 * # factors 315;;
 * - : int list = [3; 3; 5; 7]
 *
 *******************************************************************************
 *)

let factors_0 = function
  | 1 -> []
  | num ->
      let rec div_iter acc base = function
        | i when base = i -> i :: acc
        | i when base mod i = 0 ->
          div_iter (i :: acc) (base/i) i
        | i ->
          div_iter acc base (i+1)
      in
      List.rev @@ div_iter [] num 2
;;

let factors = factors_0

let () =
  assert (factors 315 = [3; 3; 5; 7]);
;;

