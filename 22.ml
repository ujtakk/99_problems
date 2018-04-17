(*
 * 22. Create a list containing all integers within a given range. (*easy*)
 *******************************************************************************
 *
 * If first argument is greater than second, produce a list in decreasing * order.
 *
 * # range 4 9;;
 * - : int list = [4; 5; 6; 7; 8; 9]
 * # range 9 4;;
 * - : int list = [9; 8; 7; 6; 5; 4]
 *
 *******************************************************************************
 *)

let rec range_0 a b = match a, b with
  | x, y when x = y -> [x]
  | x, y when x < y -> x :: range_0 (x+1) y
  | x, y when x > y -> x :: range_0 (x-1) y
  | _ -> []
;;

let range = range_0

let () =
  assert (
    range 4 9
    = [4; 5; 6; 7; 8; 9]
  );

  assert (
    range 9 4
    = [9; 8; 7; 6; 5; 4]
  );
;;

