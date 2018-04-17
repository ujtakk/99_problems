(*
 * 33. Determine whether two positive integer numbers are coprime. (*easy*)
 *******************************************************************************
 *
 * Two numbers are coprime if their greatest common divisor equals 1.
 *
 * # coprime 13 27;;
 * - : bool = true
 * # not (coprime 20536 7826);;
 * - : bool = true
 *
 *******************************************************************************
 *)

let coprime_0 m n =
  let rec gcd m n =
    if n = 0 then m else gcd n (m mod n)
  in
  gcd m n = 1
;;

let coprime = coprime_0

let () =
  assert (coprime 13 27 = true);
  assert (not (coprime 20536 7826) = true);
;;

