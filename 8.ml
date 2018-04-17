(*
 * 8. Eliminate consecutive duplicates of list elements. (*medium*)
 *******************************************************************************
 *
 * # compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 * - : string list = ["a"; "b"; "c"; "a"; "d"; "e"]
 *
 *******************************************************************************
 *)

let compress_0 l =
  let rec iter acc seek = function
    | [] -> acc
    | h :: t ->
        if h = seek then
          iter acc seek t
        else
          iter (h :: acc) h t
  in
  List.rev @@ iter [] "" l
;;

let compress = compress_0

let () =
  let show_list l =
    let rec show = function
      | [] -> ""
      | [x] -> "\""^x^"\""
      | h :: t -> "\""^h^"\"; "^(show t)
    in
    "["^(show l)^"]"
  in

  compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  |> show_list
  |> print_endline;
  assert (
    compress ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
    = ["a"; "b"; "c"; "a"; "d"; "e"]
  );
;;
