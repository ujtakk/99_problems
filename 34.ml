(*
 * 34. Calculate Euler's totient function φ(m). (*medium*)
 *******************************************************************************
 *
 * Euler's so-called totient function φ(m) is defined as the number of
 * positive integers r (1 ≤ r < m) that are coprime to m. We let φ(1) = 1.
 *
 * Find out what the value of φ(m) is if m is a prime number. Euler's
 * totient function plays an important role in one of the most widely used
 * public key cryptography methods (RSA). In this exercise you should use
 * the most primitive method to calculate this function (there are smarter
 * ways that we shall discuss later).
 *
 * # phi 10;;
 * - : int = 4
 * # phi 13;;
 * - : int = 12
 *
 *******************************************************************************
 *)

let phi_0 = function
  | 1 -> 1
  | m ->
      let coprime m n =
        let rec gcd m n =
          if n = 0 then m else gcd n (m mod n)
        in
        gcd m n = 1
      in
      let rec iter acc = function
        | 0 -> acc
        | i when coprime m i -> iter (acc+1) (i-1)
        | i -> iter acc (i-1)
      in
      iter 0 m
;;

let phi = phi_0

let () =
  assert (phi 10 = 4);
  assert (phi 13 = 12);
;;

