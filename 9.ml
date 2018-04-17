(*
 * 9. Pack consecutive duplicates of list elements into sublists. (*medium*)
 *******************************************************************************
 *
 * # pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"];;
 * - : string list list =
 * [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
 *  ["e"; "e"; "e"; "e"]]
 *
 *******************************************************************************
 *)

let pack_0 l =
  let rec iter acc seek = function
    | [] -> seek :: acc
    | h :: t -> match seek with
                | [] -> iter acc [h] t
                | a :: b -> if a = h then
                              iter acc (h :: seek) t
                            else
                              iter (seek :: acc) [h] t
  in
  List.rev @@ iter [] [] l
;;

let pack = pack_0

let show_list_list ll =
  let show_list l =
    let rec show = function
      | [] -> ""
      | [x] -> "\""^x^"\""
      | h :: t -> "\""^h^"\"; "^(show t)
    in
    "["^(show l)^"]"
  in
  let rec show_ll = function
    | [] -> ""
    | [l] -> show_list l
    | h :: t -> (show_list h)^"; "^(show_ll t)
  in
  "["^(show_ll ll)^"]"
;;

let () =
  let show_list_list ll =
    let show_list l =
      let rec show = function
        | [] -> ""
        | [x] -> "\""^x^"\""
        | h :: t -> "\""^h^"\"; "^(show t)
      in
      "["^(show l)^"]"
    in
    let rec show_ll = function
      | [] -> ""
      | [l] -> show_list l
      | h :: t -> (show_list h)^"; "^(show_ll t)
    in
    "["^(show_ll ll)^"]"
  in

  pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
  |> show_list_list
  |> print_endline;
  assert (
    pack ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"d";"e";"e";"e";"e"]
    = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
       ["e"; "e"; "e"; "e"]]
  );
;;
