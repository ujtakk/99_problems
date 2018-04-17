(*
 * 37. Calculate Euler's totient function φ(m) (improved). (*medium*)
 *******************************************************************************
 *
 * See problem "[Calculate Euler's totient function
 * φ(m)](#CalculateEuler39stotientfunctionmmedium)" for the definition of
 * Euler's totient function. If the list of the prime factors of a number m
 * is known in the form of the previous problem then the function phi(m)
 * can be efficiently calculated as follows: Let `[(p1, m1); (p2, m2);
 * (p3, m3); ...]` be the list of prime factors (and their multiplicities)
 * of a given number m. Then φ(m) can be calculated with the following
 * formula:
 *
 * φ(m) = (p1 - 1) × p1<sup>m1 - 1</sup> × (p2 - 1) × p2<sup>m2 - 1</sup> ×
 * (p3 - 1) × p3<sup>m3 - 1</sup> × ⋯
 *
 * # phi_improved 10;;
 * - : int = 4
 * # phi_improved 13;;
 * - : int = 12
 *
 *******************************************************************************
 *)

let phi_improved_0 m =
  let factors = function
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
  in
  let rec power a = function
    | 0 -> 1
    | k -> a * power a (k-1)
  in
  let rec iter acc = function
    | [] -> acc
    | (p, m) :: t -> iter ((p-1) * (power p (m-1))) t
  in
  iter 1 (factors m)
;;

let phi_improved = phi_improved_0

let () =
  assert (phi_improved 10 = 4);
  assert (phi_improved 13 = 12);
;;

