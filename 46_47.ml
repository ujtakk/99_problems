(*
 * ## Logic and Codes
 *
 * Let us define a small "language" for boolean expressions containing
 * variables:
 *
 * # type bool_expr =
 *     | Var of string
 *     | Not of bool_expr
 *     | And of bool_expr * bool_expr
 *     | Or of bool_expr * bool_expr;;
 * type bool_expr =
 *     Var of string
 *   | Not of bool_expr
 *   | And of bool_expr * bool_expr
 *   | Or of bool_expr * bool_expr
 *
 * A logical expression in two variables can then be written in prefix
 * notation. For example, `(a ∨ b) ∧ (a ∧ b)` is written:
 *
 * # And(Or(Var "a", Var "b"), And(Var "a", Var "b"));;
 * - : bool_expr = And (Or (Var "a", Var "b"), And (Var "a", Var "b"))
 *
 * 46 & 47. Truth tables for logical expressions (2 variables). (*medium*)
 *******************************************************************************
 *
 * Define a function, `table2` which returns the truth table of a given
 * logical expression in two variables (specified as arguments). The return
 * value must be a list of triples containing `(value_of_a, balue_of_b,
 * value_of_expr)`.
 *
 * # table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")));;
 * - : (bool * bool * bool) list =
 * [(true, true, true); (true, false, true); (false, true, false);
 * (false, false, false)]
 *
 *******************************************************************************
 *)

type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr
;;

let table2_0 x y expr =
  let rec eval lut = function
    | Var a -> List.assoc a lut
    | Not a -> not (eval lut a)
    | And (a, b) -> (eval lut a) && (eval lut b)
    | Or (a, b) -> (eval lut a) || (eval lut b)
  in
  let logic = [(true, true); (true, false); (false, true); (false, false)] in
  let rec iter acc = function
    | [] -> acc
    | (a, b) :: t -> iter ((a, b, eval [(x, a); (y, b)] expr) :: acc) t
  in
  List.rev @@ iter [] logic
;;

let table2 = table2_0

let () =
  assert (
    table2 "a" "b" (And(Var "a", Or(Var "a", Var "b")))
    = [(true, true, true); (true, false, true); (false, true, false);
      (false, false, false)]
  );
;;

