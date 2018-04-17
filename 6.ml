(*
 * 6. Find out whether a list is a palindrome. (*easy*)
 *******************************************************************************
 *
 * HINT: a palindrome is its own reverse.
 *
 * # is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ];;
 * - : bool = true
 * # not (is_palindrome [ "a" ; "b" ]);;
 * - : bool = true
 *
 *******************************************************************************
 *)

let is_palindrome_0 l =
  let l_rev = List.rev l in
  let rec iter acc x y = match x, y with
    | [], [] -> acc
    | a :: b, c :: d -> iter (a = c && acc) b d
    | _ -> false
  in
  iter true l l_rev
;;

let is_palindrome_1 l =
  l = List.rev l
;;

let is_palindrome = is_palindrome_1

let () =
  is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ]
  |> string_of_bool
  |> print_endline;
  assert (is_palindrome [ "x" ; "a" ; "m" ; "a" ; "x" ] = true);

  not (is_palindrome [ "a" ; "b" ])
  |> string_of_bool
  |> print_endline;
  assert (not (is_palindrome [ "a" ; "b" ]) = true);
;;

