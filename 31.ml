(*
 * ## Arithmetric
 *
 * 31. Determine whether a given integer number is prime. (*medium*)
 *******************************************************************************
 *
 * # not(is_prime 1);;
 * - : bool = true
 * # is_prime 7;;
 * - : bool = true
 * # not (is_prime 12);;
 * - : bool = true
 *
 *******************************************************************************
 *)

let is_prime_0 = function
  | 1 -> false
  | 2 -> true
  | n ->
      let range a b =
        let rec iter acc value = function
          | 0 -> acc
          | n -> iter (value :: acc) (value+1) (n-1)
        in
        List.rev @@ iter [] a (b-a)
      in
      let belows = range 2 n in
      let results = List.map (fun m -> n mod m <> 0) belows in
      List.fold_left (&&) true results
;;

let is_prime = is_prime_0

let () =
  assert (not(is_prime 1) = true);
  assert (is_prime 7 = true);
  assert (not (is_prime 12) = true);
;;
