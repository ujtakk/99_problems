(*
 * 10. Run-length encoding of a list. (*easy*)
 *******************************************************************************
 *
 * If you need so, refresh your memory about [run-length
 * encoding](http://en.wikipedia.org/wiki/Run-length_encoding).
 *
 * Here is an example:
 *
 * # encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"];;
 * - : (int * string) list =
 * [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
 *
 *******************************************************************************
 *)

let encode_0 l =
  let rec iter acc count seek = function
    | [] -> (count, seek) :: acc
    | h :: t -> if h = seek then
                  iter acc (count+1) seek t
                else
                  iter ((count, seek) :: acc) 1 h t
  in
  match l with
  | [] -> []
  | h :: t -> List.rev @@ iter [] 1 h t
;;

let encode_1 list =
  let rec aux count acc = function
    | [] -> [] (* Can only be reached if original list is empty *)
    | [x] -> (count+1, x) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                            else aux 0 ((count+1,a) :: acc) t in
  List.rev (aux 0 [] list)
;;

(* This solution may be deduced from pack function in 9.ml *)
let encode_2 list =
  let pack l =
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
  in
  List.map (fun l -> (List.length l, List.hd l)) (pack list)
;;

let encode = encode_1

let () =
  let show_tuple_list tl =
    let show_tuple (i, s) = "("^(string_of_int i)^", \""^s^"\")" in
    let rec show_tl = function
      | [] -> ""
      | [t] -> show_tuple t
      | h :: t -> (show_tuple h)^"; "^(show_tl t)
    in
    "["^(show_tl tl)^"]"
  in

  encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
  |> show_tuple_list
  |> print_endline;
  assert (
    encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
    = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
  );
;;
