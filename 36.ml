(*
 * 36. Determine the prime factors of a given positive integer (2). (*medium*)
 *******************************************************************************
 *
 * Construct a list containing the prime factors and their multiplicity.
 * *Hint:* The problem is similar to problem [Run-length encoding of a list
 * (direct solution)](#Runlengthencodingofalistdirectsolutionmedium).
 *
 * # factors 315;;
 * - : (int * int) list = [(3, 2); (5, 1); (7, 1)]
 *
 *******************************************************************************
 *)

let factors_0 : int -> (int * int) list = function
  | 1 -> []
  | num ->
      let rec div_iter acc base = function
        | i when base = i -> begin
              match acc with
              | (n, c) :: t when n = i ->
                  (n, c+1) :: t
              | _ ->
                  (i, 1) :: acc
            end
        | i when base mod i = 0 -> begin
              match acc with
              | (n, c) :: t when n = i ->
                  div_iter ((n, c+1) :: t) (base/i) i
              | _ ->
                  div_iter ((i, 1) :: acc) (base/i) i
            end
        | i ->
            div_iter acc base (i+1)
      in
      List.rev @@ div_iter [] num 2
;;

let factors = factors_0

let () =
  assert (factors 315 = [(3, 2); (5, 1); (7, 1)]);
;;

