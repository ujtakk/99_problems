(*
 * ## Miscellaneous Problems
 *
 * 91. Eight queens problem. (*medium*)
 *******************************************************************************
 *
 * This is a classical problem in computer science. The objective is to
 * place eight queens on a chessboard so that no two queens are attacking
 * each other; i.e., no two queens are in the same row, the same column, or
 * on the same diagonal.
 *
 * Hint: Represent the positions of the queens as a list of numbers 1..N.
 * Example: `[4;2;7;3;6;8;5;1]` means that the queen in the first column is
 * in row 4, the queen in the second column is in row 2, etc. Use the
 * generate-and-test paradigm.
 *
 * # queens_positions 4;;
 * - : int list list = [[3; 1; 4; 2]; [2; 4; 1; 3]]
 * # List.length (queens_positions 8);;
 * - : int = 92
 *
 *******************************************************************************
 *)

let range lower upper =
  let rec iter acc value = function
    | 0 -> value :: acc
    | n -> iter (value :: acc) (value-1) (n-1)
  in
  iter [] upper (upper-lower)
;;

let append l1 l2 =
  let rec iter acc x y =
    match x, y with
    | [], [] -> List.rev acc
    | [], h :: t -> iter (h :: acc) [] t
    | h :: t, l -> iter (h :: acc) t l
  in
  iter [] l1 l2
;;

let factor_map lst_a lst_b =
  List.map2 (-) lst_b lst_a
;;

let has_duplicate tup_list =
  let rec duplicate base elem = function
    | [] -> base
    | h :: t -> duplicate (elem = h || base) elem t
  in
  let rec iter acc = function
    | [] -> acc
    | h :: t -> iter ((duplicate false h t) || acc) t
  in
  iter false tup_list
;;

let diag_dup combi_rows =
  let combi_max = List.length combi_rows in
  let combi_cols = range 1 combi_max in
  let combi_invs = List.map (fun r -> combi_max - r + 1) combi_rows in
  let to_lower_right = factor_map combi_cols combi_rows in
  let to_upper_right = factor_map combi_cols combi_invs in
  has_duplicate to_lower_right || has_duplicate to_upper_right

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
;;

let rec variation = function
  | [] -> [[]]
  | target ->
  let remains = List.map (fun l -> remove target [l]) target in
  let join base rest = List.map (fun x -> base :: x) (variation rest) in
  let rec iter acc = function
    | [] -> acc
    | (a, b) :: t -> iter (append acc (join a b)) t
  in
  List.rev @@ iter [] (List.combine target remains)
;;

let queens_positions_0 n =
  let cand = variation (range 1 n) in
  List.filter (fun v -> not @@ diag_dup v) cand
;;

let queens_positions = queens_positions_0

let () =
  assert (queens_positions 4 = [[3; 1; 4; 2]; [2; 4; 1; 3]]);
  assert (List.length (queens_positions 8) = 92);
;;

