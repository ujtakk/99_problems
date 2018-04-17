(*
 * 39. A list of prime numbers. (*easy*)
 *******************************************************************************
 *
 * Given a range of integers by its lower and upper limit, construct a list
 * of all prime numbers in that range.
 *
 * # List.length (all_primes 2 7920);;
 * - : int = 1000
 *
 *******************************************************************************
 *)

let all_primes_0 lower upper =
  let is_prime = function
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
  in
  let rec iter acc = function
    | i when i = upper -> if is_prime i then i :: acc else acc
    | i ->
        if is_prime i then
          iter (i :: acc) (i+1)
        else
          iter acc (i+1)
  in
  List.rev @@ iter [] lower
;;

let rec all_primes_1 a b =
  let is_prime n =
    let n = max n (-n) in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d+1))
    in
    is_not_divisor 2
  in
  if a > b then
    []
  else
    let rest = all_primes_1 (a + 1) b in
    if is_prime a then a :: rest else rest
;;

let all_primes = all_primes_1

let () =
  assert (List.length (all_primes 2 7920) = 1000);
;;

