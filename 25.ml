(*
 * 25. Generate a random permutation of the elements of a list. (*easy*)
 *******************************************************************************
 *
 * # permutation ["a"; "b"; "c"; "d"; "e"; "f"];;
 * - : string list = ["a"; "e"; "f"; "b"; "d"; "c"]
 *
 *******************************************************************************
 *)

let permutation_0 lst =
  let pop_rand len target =
    let rec iter_pop i acc = function
      | [] -> raise Not_found
      | h :: t -> if i = 0 then
                    (h, acc @ t)
                  else
                    iter_pop (i-1) (h :: acc) t
    in
    iter_pop (Random.int len) [] target
  in
  let rec iter count acc = function
    | [] -> acc
    | l ->
        let popped, rest_lst = pop_rand count l in
        iter (count-1) (popped :: acc) rest_lst
  in
  iter (List.length lst) [] lst
;;

let rec permutation_1 list =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
  in
  let extract_rand list len =
    extract [] (Random.int len) list
  in
  let rec aux acc list len =
    if len = 0 then acc else
      let picked, rest = extract_rand list len in
      aux (picked :: acc) rest (len-1)
  in
  aux [] list (List.length list)
;;

let permutation = permutation_0

let () =
  let show_list l =
    let show_string s = "\""^s^"\"" in
    let rec show_l = function
      | [] -> ""
      | [x] -> show_string x
      | h :: t -> (show_string h)^"; "^(show_l t)
    in
    "["^(show_l l)^"]"
  in

  permutation ["a"; "b"; "c"; "d"; "e"; "f"]
  |> show_list
  |> print_endline;
  assert true;
;;
