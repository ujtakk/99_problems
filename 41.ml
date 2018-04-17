(*
 * 41. A list of Goldbach compositions. (*medium*)
 *******************************************************************************
 *
 * Given a range of integers by its lower and upper limit, print a list of
 * all even numbers and their Goldbach composition.
 *
 * In most cases, if an even number is written as the sum of two prime
 * numbers, one of them is very small. Very rarely, the primes are both
 * bigger than say 50. Try to find out how many such cases there are in the
 * range 2..3000.
 *
 * # goldbach_list 9 20;;
 * - : (int * (int * int)) list =
 * [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
 *  (20, (3, 17))]
 * # goldbach_limit 1 2000 50;;
 * - : (int * (int * int)) list =
 * [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
 *  (1928, (61, 1867))]
 *
 *******************************************************************************
 *)

let goldbach_list_0 lower upper =
  let range a b d =
    let rec iter acc value = function
      | n when n < 0 -> acc
      | n when n = 0 -> value :: acc
      | n -> iter (value :: acc) (value+d) (n-d)
    in
    List.rev @@ iter [] a (b-a)
  in
  let goldbach_w_orig x =
    let is_prime = function
      | 1 -> false
      | 2 -> true
      | n ->
          let belows = range 2 (n-1) 1 in
          let results = List.map (fun m -> n mod m <> 0) belows in
          List.fold_left (&&) true results
    in
    let rec iter m = function
      | 2 -> (m, 2)
      | n ->
          if (is_prime m) && (is_prime n) then
            m, n
          else
            iter (m+1) (n-1)
    in
    (x, iter 1 (x-1))
  in
  if lower mod 2 = 1 then
    List.map goldbach_w_orig (range (lower+1) upper 2)
  else
    List.map goldbach_w_orig (range lower upper 2)
;;

let goldbach_limit_0 min max thresh =
  let filter_lower n = function
    | (orig, (lower, upper)) -> lower > n && upper > n
  in
  let cand = goldbach_list_0 min max in
  List.filter (filter_lower thresh) cand
;;

let rec goldbach_list_1 a b =
   let is_prime n =
    let n = abs n in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d+1)) in
    n <> 1 && is_not_divisor 2
   in
  let goldbach n =
    let rec aux d =
      if is_prime d && is_prime (n - d) then (d, n-d)
      else aux (d+1) in
    aux 2
  in
  if a > b then [] else
    if a mod 2 = 1 then goldbach_list_1 (a+1) b
    else (a, goldbach a) :: goldbach_list_1 (a+2) b
;;

let goldbach_limit_1 a b lim =
  List.filter (fun (_,(a,b)) -> a > lim && b > lim) (goldbach_list_1 a b);;

let goldbach_list = goldbach_list_1
let goldbach_limit = goldbach_limit_1

let () =
  assert (
    goldbach_list 9 20
    = [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
       (20, (3, 17))]
  );

  assert (
    goldbach_limit 1 2000 50
    = [(992, (73, 919)); (1382, (61, 1321)); (1856, (67, 1789));
       (1928, (61, 1867))]
  );
;;

