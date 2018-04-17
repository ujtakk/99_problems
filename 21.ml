(*
 * 21. Insert an element at a given position into a list. (*easy*)
 *******************************************************************************
 *
 * Start counting list elements with 0. If the position is larger or equal
 * to the length of the list, insert the element at the end. (The behavior
 * is unspecified if the position is negative.)
 *
 * # insert_at "alfa" 1 ["a";"b";"c";"d"];;
 * - : string list = ["a"; "alfa"; "b"; "c"; "d"]
 * # insert_at "alfa" 3 ["a";"b";"c";"d"];;
 * - : string list = ["a"; "b"; "c"; "alfa"; "d"]
 * # insert_at "alfa" 4 ["a";"b";"c";"d"];;
 * - : string list = ["a"; "b"; "c"; "d"; "alfa"]
 *
 *******************************************************************************
 *)

let insert_at_0 str idx lst =
  let rec iter count acc = function
    | [] -> if count >= 0 then
              (str :: acc)
            else
              acc
    | h :: t -> if count = 0 then
                  (List.rev t) @ (h :: str :: acc)
                else
                  iter (count-1) (h :: acc) t
  in
  List.rev @@ iter idx [] lst
;;

let insert_at_1 str idx lst =
  let rec iter count acc = function
    | [] -> if count >= 0 then
              (str :: acc)
            else
              acc
    | h :: t -> if count = 0 then
                  iter (count-1) (h :: str :: acc) t
                else
                  iter (count-1) (h :: acc) t
  in
  List.rev @@ iter idx [] lst
;;

let insert_at = insert_at_1

let () =
  assert (
    insert_at "alfa" 1 ["a";"b";"c";"d"]
    = ["a"; "alfa"; "b"; "c"; "d"]
  );

  assert (
    insert_at "alfa" 3 ["a";"b";"c";"d"]
    = ["a"; "b"; "c"; "alfa"; "d"]
  );

  assert (
    insert_at "alfa" 4 ["a";"b";"c";"d"]
    = ["a"; "b"; "c"; "d"; "alfa"]
  );
;;
