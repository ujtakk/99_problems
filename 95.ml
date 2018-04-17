(*
 * 95. English number words. (*medium*)
 *******************************************************************************
 *
 * On financial documents, like cheques, numbers must sometimes be written
 * in full words. Example: 175 must be written as one-seven-five. Write a
 * function `full_words` to print (non-negative) integer numbers in full
 * words.
 *
 * # full_words 175;;
 * - : string = "one-seven-five"
 * # full_words 23485;;
 * - : string = "two-three-four-height-five"
 * # full_words 0;;
 * - : string = "zero"
 *
 *******************************************************************************
 *)

let full_words_0 n =
  let lut_words =
    [(0, "zero"); (1, "one"); (2, "two"); (3, "three"); (4, "four");
     (5, "five"); (6, "six"); (7, "seven"); (8, "eight"); (9, "nine")]
  in
  let rec iter acc = function
    | i when i < 10 -> begin
        match acc with
        | "" -> List.assoc i lut_words
        | s -> List.assoc i lut_words ^"-"^ s
      end
    | i -> begin
        match acc with
        | "" -> iter (List.assoc (i mod 10) lut_words) (i / 10)
        | s -> iter (List.assoc (i mod 10) lut_words ^"-"^ s) (i / 10)
      end
  in
  iter "" n
;;

let full_words = full_words_0

let () =
  assert (full_words 175 = "one-seven-five");
  assert (full_words 23485 = "two-three-four-eight-five");
  assert (full_words 0 = "zero");
;;

