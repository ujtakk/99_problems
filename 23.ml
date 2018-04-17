(*
 * 23. Extract a given number of randomly selected elements from a list. (*medium*)
 *******************************************************************************
 *
 * The selected items shall be returned in a list. We use the `Random`
 * module but do not initialize it with `Random.self_init` for
 * reproducibility.
 *
 * # rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3;;
 * - : string list = ["g"; "d"; "a"]
 *
 *******************************************************************************
 *)

let rec rand_select_0 lst size =
  if size = 0 then
    []
  else
    let len = List.length lst in
    List.nth lst (Random.int len) :: rand_select_0 lst (size-1)
;;

let rand_select_1 list n =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
  in
  let extract_rand list len =
    extract [] (Random.int len) list
  in
  let rec aux n acc list len =
    if n = 0 then acc else
      let picked, rest = extract_rand list len in
      aux (n-1) (picked :: acc) rest (len-1)
  in
  let len = List.length list in
  aux (min n len) [] list len
;;

let rand_select = rand_select_0

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

  rand_select ["a";"b";"c";"d";"e";"f";"g";"h"] 3
  |> show_list
  |> print_endline;
  assert true;
;;
