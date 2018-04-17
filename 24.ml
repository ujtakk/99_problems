(*
 * 24. Lotto: Draw N different random numbers from the set 1..M. (*easy*)
 *******************************************************************************
 *
 * The selected numbers shall be returned in a list.
 *
 * # lotto_select 6 49;;
 * - : int list = [10; 20; 44; 22; 41; 2]
 *
 *******************************************************************************
 *)

let rec lotto_select_0 length range =
  if length = 0 then
    []
  else
    (Random.int range) + 1 :: lotto_select_0 (length-1) range
;;

let lotto_select = lotto_select_0

let () =
  let show_list l =
    let show_int i = string_of_int i in
    let rec show_l = function
      | [] -> ""
      | [x] -> show_int x
      | h :: t -> (show_int h)^"; "^(show_l t)
    in
    "["^(show_l l)^"]"
  in

  lotto_select 6 49
  |> show_list
  |> print_endline;
  assert true;
;;
