(*
 * 26. Generate the combinations of K distinct objects chosen from the N elements of a list. (*medium*)
 *******************************************************************************
 *
 * In how many ways can a committee of 3 be chosen from a group of 12
 * people? We all know that there are C(12,3) = 220 possibilities (C(N,K)
 * denotes the well-known binomial coefficients). For pure mathematicians,
 * this result may be great. But we want to really generate all the
 * possibilities in a list.
 *
 * # extract 2 ["a";"b";"c";"d"];;
 * - : string list list =
 * [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
 *
 *******************************************************************************
 *)

let rec extract_0 size = function
  | [] -> []
  | h :: t as l -> match size with
      | 0 -> []
      | 1 -> List.map (fun x -> [x]) l
      | _ ->
          let fst = List.map (fun l -> h :: l) (extract_0 (size-1) t) in
          let snd = extract_0 size t in
          fst @ snd
;;

let rec extract_1 k list =
    if k <= 0 then
      [ [] ]
    else
      match list with
       | [] -> []
       | h :: tl ->
          let with_h = List.map (fun l -> h :: l) (extract_1 (k-1) tl) in
          let without_h = extract_1 k tl in
          with_h @ without_h
;;

let extract = extract_0

let () =
  assert (
    extract 2 ["a";"b";"c";"d"]
    = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]]
  );
;;
