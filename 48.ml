(*
 * 48. Truth tables for logical expressions. (*medium*)
 *******************************************************************************
 *
 * Generalize the previous problem in such a way that the logical
 * expression may contain any number of logical variables. Define `table`
 * in a way that `table variables expr` returns the truth table for the
 * expression `expr`, which contains the logical variables enumerated in
 * `variables`.
 *
 * # table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")));;
 * - : ((string * bool) list * bool) list =
 * [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
 *  ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
 * # let a = Var "a" and b = Var "b" and c = Var "c" in
 *   table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))));;
 * - : ((string * bool) list * bool) list =
 * [([("a", true); ("b", true); ("c", true)], true);
 *  ([("a", true); ("b", true); ("c", false)], true);
 *  ([("a", true); ("b", false); ("c", true)], true);
 *  ([("a", true); ("b", false); ("c", false)], false);
 *  ([("a", false); ("b", true); ("c", true)], false);
 *  ([("a", false); ("b", true); ("c", false)], false);
 *  ([("a", false); ("b", false); ("c", true)], false);
 *  ([("a", false); ("b", false); ("c", false)], false)]
 *
 *******************************************************************************
 *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
;;

let table_0 vars expr =
  let rec gen_logic = function
    | 0 -> []
    | 1 -> [[true]; [false]]
    | n ->
        let rest = gen_logic (n-1) in
        let true_part = List.map (fun t -> true :: t) rest in
        let false_part = List.map (fun t -> false :: t) rest in
        true_part @ false_part
  in
  let logic_cases = gen_logic (List.length vars) in

  let concat_var_logic var lut =
    let rec iter_concat acc = function
      | [] -> acc
      | combi :: rest ->
          let entry = List.map2 (fun v l -> (v, l)) var combi in
          iter_concat (entry :: acc) rest
    in
    List.rev @@ iter_concat [] lut
  in
  let logic_lut = concat_var_logic vars logic_cases in

  let rec eval lut = function
    | Var a -> List.assoc a lut
    | Not a -> not (eval lut a)
    | And (a, b) -> (eval lut a) && (eval lut b)
    | Or (a, b) -> (eval lut a) || (eval lut b)
  in
  let rec iter_eval acc = function
    | [] -> acc
    | var_case :: rest ->
        iter_eval ((var_case, eval var_case expr) :: acc) rest
  in
  List.rev @@ iter_eval [] logic_lut
;;

let table = table_0

let () =
  assert (
    table ["a"; "b"] (And(Var "a", Or(Var "a", Var "b")))
    = [([("a", true); ("b", true)], true); ([("a", true); ("b", false)], true);
       ([("a", false); ("b", true)], false); ([("a", false); ("b", false)], false)]
  );

  assert (
    let a = Var "a" and b = Var "b" and c = Var "c" in
    table ["a"; "b"; "c"] (Or(And(a, Or(b,c)), Or(And(a,b), And(a,c))))
    = [([("a", true); ("b", true); ("c", true)], true);
       ([("a", true); ("b", true); ("c", false)], true);
       ([("a", true); ("b", false); ("c", true)], true);
       ([("a", true); ("b", false); ("c", false)], false);
       ([("a", false); ("b", true); ("c", true)], false);
       ([("a", false); ("b", true); ("c", false)], false);
       ([("a", false); ("b", false); ("c", true)], false);
       ([("a", false); ("b", false); ("c", false)], false)]
  );
;;

