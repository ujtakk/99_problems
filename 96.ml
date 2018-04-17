(*
 * 96. Syntax checker. (*medium*)
 *******************************************************************************
 *
 * ![](../../img/syntax-graph.gif "Syntax graph")
 *
 * In a certain programming language (Ada) identifiers are defined by the
 * syntax diagram (railroad chart) opposite. Transform the syntax diagram
 * into a system of syntax diagrams which do not contain loops; i.e. which
 * are purely recursive. Using these modified diagrams, write a function
 * `identifier : string -> bool` that can check whether or not a given
 * string is a legal identifier.
 *
 * # identifier "this-is-a-long-identifier";;
 * - : bool = true
 * # identifier "this-ends-in-";;
 * - : bool = false
 * # identifier "two--hyphens";;
 * - : bool = false
 * # identifier "-dash-first";;
 * - : bool = false
 *
 *******************************************************************************
 *)

#require "str"

let identifier_0 s =
  let letter = function
    | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'g' | 'h' | 'i' | 'j' | 'k' | 'l' | 'm'
    | 'n' | 'o' | 'p' | 'q' | 'r' | 's' | 't' | 'u' | 'v' | 'w' | 'x' | 'y' | 'z'
        -> true
    | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' | 'H' | 'I' | 'J' | 'K' | 'L' | 'M'
    | 'N' | 'O' | 'P' | 'Q' | 'R' | 'S' | 'T' | 'U' | 'V' | 'W' | 'X' | 'Y' | 'Z'
        -> true
    | _ -> false
  in
  let hyphen = function
    | '-' -> true
    | _ -> false
  in
  let digit = function
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'
        -> true
    | _ -> false
  in
  let letter_or_digit c = letter c || digit c in
  let rec iter str max acc = function
    | i when i = max-1 ->
        acc && letter_or_digit str.[i]
    | i when hyphen str.[i] ->
        iter str max (acc && letter_or_digit str.[i+1]) (i+2)
    | i ->
        iter str max (acc && letter_or_digit str.[i]) (i+1)
  in
  iter s (String.length s) (letter s.[0]) 1
;;

let identifier_1 s =
  let id = Str.regexp "^[a-zA-Z]\\(-?[a-zA-Z0-9]\\)*$" in
  Str.string_match id s 0
;;

let identifier = identifier_0

let () =
  assert (identifier "this-is-a-long-identifier" = true);
  assert (identifier "this-ends-in-" = false);
  assert (identifier "two--hyphens" = false);
  assert (identifier "-dash-first" = false);
;;

