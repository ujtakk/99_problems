(*
 * 38. Compare the two methods of calculating Euler's totient function. (*easy*)
 *******************************************************************************
 *
 * Use the solutions of problems "[Calculate Euler's totient function
 * φ(m)](#CalculateEuler39stotientfunctionmmedium)" and "[Calculate
 * Euler's totient function φ(m)
 * (improved)](#CalculateEuler39stotientfunctionmimprovedmedium)" to
 * compare the algorithms. Take the number of logical inferences as a
 * measure for efficiency. Try to calculate φ(10090) as an example.
 *
 * # timeit phi 10090;;
 * - : float = 0.00458407402038574219
 * # timeit phi_improved 10090;;
 * - : float = 4.00543212890625e-05
 *
 *******************************************************************************
 *)

let phi = function
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

let phi_improved m =
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

let timeit_0 func arg =
  let t0 = Sys.time () in
  ignore(func arg);
  let t1 = Sys.time () in
  t1 -. t0
;;

(* #require "unix" *)

let timeit_1 f a =
  let t0 = Unix.gettimeofday () in
  ignore(f a);
  let t1 = Unix.gettimeofday () in
  t1 -. t0
;;

let timeit = timeit_1

let () =
  timeit phi 10090
  |> string_of_float
  |> print_endline;

  timeit phi_improved 10090
  |> string_of_float
  |> print_endline;
;;

