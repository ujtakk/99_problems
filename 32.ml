(*
 * 32. Determine the greatest common divisor of two positive integer numbers. (*medium*)
 *******************************************************************************
 *
 * Use Euclid's algorithm.
 *
 * # gcd 13 27;;
 * - : int = 1
 * # gcd 20536 7826;;
 * - : int = 2
 *
 *******************************************************************************
 *)

let rec gcd_0 m n = match n with
  | 0 -> m
  | _ -> gcd_0 n (m mod n)
;;

let gcd = gcd_0

let () =
  assert (gcd 13 27 = 1);
  assert (gcd 20536 7826 = 2);
;;

