(*
 * 27. Group the elements of a set into disjoint subsets. (*medium*)
 *******************************************************************************
 *
 * 1.  In how many ways can a group of 9 people work in 3 disjoint
 *     subgroups of 2, 3 and 4 persons? Write a function that generates all
 *     the possibilities and returns them in a list.
 * 2.  Generalize the above function in a way that we can specify a list of
 *     group sizes and the function will return a list of groups.
 *
 * # group ["a";"b";"c";"d"] [2;1];;
 * - : string list list list =
 * [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
 *  [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
 *  [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
 *  [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
 *
 *******************************************************************************
 *)

let append l1 l2 =
  let rec iter acc x y =
    match x, y with
    | [], [] -> List.rev acc
    | [], h :: t -> iter (h :: acc) [] t
    | h :: t, l -> iter (h :: acc) t l
  in
  iter [] l1 l2
;;

let rec extract target = function
  | 0 -> [[]]
  | n -> begin
    match target with
    | [] -> []
    | h :: t ->
        let with_h = List.map (fun l -> h :: l) (extract t (n-1)) in
        let without_h = extract t n in
        append with_h without_h
  end

let cand = extract ["a"; "b"; "c"; "d"] 3

let rec remove base = function
  | [] -> base
  | h :: t ->
    let rec iter acc = function
      | [] -> List.rev acc
      | h' :: t' ->
          if h' = h then
            iter acc t'
          else
            iter (h' :: acc) t'
    in
    let base_wo_h = iter [] base in
    remove base_wo_h t

let combine l1 l2 =
  let rec iter acc x y =
    match x, y with
    | [], [] -> List.rev acc
    | x :: xs, y :: ys -> iter ((x, y) :: acc) xs ys
    | l, [] | [], l -> raise Exit
  in
  iter [] l1 l2

let choose target size =
  let chosen = extract target size in
  let not_chosen = List.map (remove target) chosen in
  combine chosen not_chosen

let group_0 target sizes =
  let total_size = List.fold_left (+) 0 sizes in
  let candidate = extract target total_size in
  let rec iter_choose stack = function
    | [] -> [[]]
    | h :: t ->
        let rec iter_process acc = function
          | [] -> acc
          | (c, nc) :: rest ->
              let nc_pat = iter_choose nc t in
              let entry = List.map (fun l -> c :: l) nc_pat in
              iter_process (append acc entry) rest
        in
        let whole_pat = choose stack h in
        iter_process [] whole_pat
  in
  let rec iter acc = function
    | [] -> acc
    | h :: t ->
        (* expects all possible combination for given sizes *)
        let result = iter_choose h sizes in
        iter (append acc result) t
  in
  iter [] candidate

let grps =
  let rec iter acc = function
    | [] -> List.rev acc
    | h :: t ->
        let chosen = extract h 2 in
        let not_chosen = List.map (remove h) chosen in
        iter ((combine chosen not_chosen) :: acc) t
  in
  iter [] cand

let group = group_0

let () =
  assert (
    group ["a";"b";"c";"d"] [2;1]
    = [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
       [["a"; "b"]; ["d"]]; [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]];
       [["a"; "c"]; ["d"]]; [["a"; "d"]; ["c"]]; [["c"; "d"]; ["a"]];
       [["b"; "c"]; ["d"]]; [["b"; "d"]; ["c"]]; [["c"; "d"]; ["b"]]]
   (*
    = [[["a"; "b"]; ["c"]]; [["a"; "c"]; ["b"]]; [["b"; "c"]; ["a"]];
       [["a"; "b"]; ["d"]]; [["a"; "c"]; ["d"]]; [["b"; "c"]; ["d"]];
       [["a"; "d"]; ["b"]]; [["b"; "d"]; ["a"]]; [["a"; "d"]; ["c"]];
       [["b"; "d"]; ["c"]]; [["c"; "d"]; ["a"]]; [["c"; "d"]; ["b"]]]
    *)
  );
;;

