(*
 * 28. Sorting a list of lists according to length of sublists. (*medium*)
 *******************************************************************************
 *
 * 1.  We suppose that a list contains elements that are lists themselves.
 *     The objective is to sort the elements of this list according to
 *     their length. E.g. short lists first, longer lists later, or vice
 *     versa.
 *
 * 2.  Again, we suppose that a list contains elements that are lists
 *     themselves. But this time the objective is to sort the elements of
 *     this list according to their **length frequency**; i.e., in the
 *     default, where sorting is done ascendingly, lists with rare lengths
 *     are placed first, others with a more frequent length come later.
 *
 * # length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
 *                 ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
 * - : string list list =
 * [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
 *  ["i"; "j"; "k"; "l"]]
 * # frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
 *                    ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ];;
 * - : string list list =
 * [["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
 *  ["d"; "e"]; ["m"; "n"]]
 *
 *******************************************************************************
 *)

let bubble_sort meas target =
  let insert value target =
    let rec insert_iter value stack = function
      | [] -> List.rev (value :: stack)
      | h :: t ->
          if meas value < meas h then
            List.rev (h :: value :: stack) @ t
          else
            insert_iter value (h :: stack) t
    in
    insert_iter value [] target
  in
  let rec iter acc = function
    | [] -> acc
    | h :: t -> iter (insert h acc) t
  in
  iter [] target
;;

let length_sort_0 = bubble_sort List.length

let length_sort = length_sort_0

let frequency_sort_0 lst =
  let aggregate target =
    let cand = bubble_sort (fun x -> x) target in
    let rec iter acc = function
      | [] -> acc
      | h :: t -> begin
            match acc with
            | [] -> iter ((h, 1) :: acc) t
            | (n, c) :: r -> if n = h then
                               iter ((n, c+1) :: r) t
                             else
                               iter ((h, 1) :: acc) t
          end
    in
    List.rev @@ iter [] cand
  in
  let lens = List.map List.length lst in
  let occurs = aggregate lens in
  let frequency = fun l -> List.assoc (List.length l) occurs in
  bubble_sort frequency lst
;;

let frequency_sort = frequency_sort_0

let () =
  assert (
    length_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                  ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ]
    = [["o"]; ["d"; "e"]; ["d"; "e"]; ["m"; "n"]; ["a"; "b"; "c"]; ["f"; "g"; "h"];
       ["i"; "j"; "k"; "l"]]
  );

  assert (
    frequency_sort [ ["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                     ["i";"j";"k";"l"]; ["m";"n"]; ["o"] ]
    = [["i"; "j"; "k"; "l"]; ["o"]; ["a"; "b"; "c"]; ["f"; "g"; "h"]; ["d"; "e"];
       ["d"; "e"]; ["m"; "n"]]
  );
;;
