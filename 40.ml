(*
 * 40. Goldbach's conjecture. (*medium*)
 *******************************************************************************
 *
 * Goldbach's conjecture says that every positive even number greater than
 * 2 is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of
 * the most famous facts in number theory that has not been proved to be
 * correct in the general case. It has been *numerically confirmed* up to
 * very large numbers. Write a function to find the two prime numbers that
 * sum up to a given even integer.
 *
 * # goldbach 28;;
 * - : int * int = (5, 23)
 *
 *******************************************************************************
 *)

let goldbach_0 x =
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
  let rec iter m = function
    | 2 -> (m, 2)
    | n ->
        if (is_prime m) && (is_prime n) then
          m, n
        else
          iter (m+1) (n-1)
  in
  iter 1 (x-1)
;;

let goldbach = goldbach_0

let () =
  assert (goldbach 28 = (5, 23));
;;

