(*
 * 97. Sudoku. (*medium*)
 *******************************************************************************
 *
 * Sudoku puzzles go like this:
 *
 *    Problem statement                 Solution
 *
 *     .  .  4 | 8  .  . | .  1  7      9  3  4 | 8  2  5 | 6  1  7
 *             |         |                      |         |
 *     6  7  . | 9  .  . | .  .  .      6  7  2 | 9  1  4 | 8  5  3
 *             |         |                      |         |
 *     5  .  8 | .  3  . | .  .  4      5  1  8 | 6  3  7 | 9  2  4
 *     --------+---------+--------      --------+---------+--------
 *     3  .  . | 7  4  . | 1  .  .      3  2  5 | 7  4  8 | 1  6  9
 *             |         |                      |         |
 *     .  6  9 | .  .  . | 7  8  .      4  6  9 | 1  5  3 | 7  8  2
 *             |         |                      |         |
 *     .  .  1 | .  6  9 | .  .  5      7  8  1 | 2  6  9 | 4  3  5
 *     --------+---------+--------      --------+---------+--------
 *     1  .  . | .  8  . | 3  .  6      1  9  7 | 5  8  2 | 3  4  6
 *             |         |                      |         |
 *     .  .  . | .  .  6 | .  9  1      8  5  3 | 4  7  6 | 2  9  1
 *             |         |                      |         |
 *     2  4  . | .  .  1 | 5  .  .      2  4  6 | 3  9  1 | 5  7  8
 *
 * Every spot in the puzzle belongs to a (horizontal) row and a (vertical)
 * column, as well as to one single 3x3 square (which we call "square" for
 * short). At the beginning, some of the spots carry a single-digit number
 * between 1 and 9. The problem is to fill the missing spots with digits in
 * such a way that every number between 1 and 9 appears exactly once in
 * each row, in each column, and in each square.
 *
 * # (* The board representation is not imposed.  Here "0" stands for "." *)
 *   let initial_board =
 *     Board.of_list [[0; 0; 4;  8; 0; 0;  0; 1; 7];
 *                    [6; 7; 0;  9; 0; 0;  0; 0; 0];
 *                    [5; 0; 8;  0; 3; 0;  0; 0; 4];
 *                    [3; 0; 0;  7; 4; 0;  1; 0; 0];
 *                    [0; 6; 9;  0; 0; 0;  7; 8; 0];
 *                    [0; 0; 1;  0; 6; 9;  0; 0; 5];
 *                    [1; 0; 0;  0; 8; 0;  3; 0; 6];
 *                    [0; 0; 0;  0; 0; 6;  0; 9; 1];
 *                    [2; 4; 0;  0; 0; 1;  5; 0; 0]];;
 * val initial_board : Board.t =
 *   [|0; 0; 4; 8; 0; 0; 0; 1; 7; 6; 7; 0; 9; 0; 0; 0; 0; 0; 5; 0; 8; 0; 3; 0;
 *     0; 0; 4; 3; 0; 0; 7; 4; 0; 1; 0; 0; 0; 6; 9; 0; 0; 0; 7; 8; 0; 0; 0; 1;
 *     0; 6; 9; 0; 0; 5; 1; 0; 0; 0; 8; 0; 3; 0; 6; 0; 0; 0; 0; 0; 6; 0; 9; 1;
 *     2; 4; 0; 0; 0; 1; 5; 0; 0|]
 * # Board.print (sudoku initial_board);;
 * 9  3  4 | 8  2  5 | 6  1  7
 *         |         |
 * 6  7  2 | 9  1  4 | 8  5  3
 *         |         |
 * 5  1  8 | 6  3  7 | 9  2  4
 * --------+---------+--------
 * 3  2  5 | 7  4  8 | 1  6  9
 *         |         |
 * 4  6  9 | 1  5  3 | 7  8  2
 *         |         |
 * 7  8  1 | 2  6  9 | 4  3  5
 * --------+---------+--------
 * 1  9  7 | 5  8  2 | 3  4  6
 *         |         |
 * 8  5  3 | 4  7  6 | 2  9  1
 *         |         |
 * 2  4  6 | 3  9  1 | 5  7  8
 * - : unit = ()
 *
 *******************************************************************************
 *)

module Board : sig
  type t
  val of_list : int list list -> t
  val print : t -> unit
end = struct
  let of_list = function
    | [[p11; p12; p13;  p14; p15; p16;  p17; p18; p19];
      [[p21; p22; p23;  p24; p25; p26;  p27; p28; p29];
      [[p31; p32; p33;  p34; p35; p36;  p37; p38; p39];
      [[p41; p42; p43;  p44; p45; p46;  p47; p48; p49];
      [[p51; p52; p53;  p54; p55; p56;  p57; p58; p59];
      [[p61; p62; p63;  p64; p65; p66;  p67; p68; p69];
      [[p71; p72; p73;  p74; p75; p76;  p77; p78; p79];
      [[p81; p82; p83;  p84; p85; p86;  p87; p88; p89];
      [[p91; p92; p93;  p94; p95; p96;  p97; p98; p99]]
    -> 1
  let print s = print_endline s
end

let sudoku_0 s = "NOT IMPLEMENTED YET"
;;

let sudoku : Board.t -> Board.t = sudoku_0

let () =
  let initial_board =
    Board.of_list [[0; 0; 4;  8; 0; 0;  0; 1; 7];
                   [6; 7; 0;  9; 0; 0;  0; 0; 0];
                   [5; 0; 8;  0; 3; 0;  0; 0; 4];
                   [3; 0; 0;  7; 4; 0;  1; 0; 0];
                   [0; 6; 9;  0; 0; 0;  7; 8; 0];
                   [0; 0; 1;  0; 6; 9;  0; 0; 5];
                   [1; 0; 0;  0; 8; 0;  3; 0; 6];
                   [0; 0; 0;  0; 0; 6;  0; 9; 1];
                   [2; 4; 0;  0; 0; 1;  5; 0; 0]]
  in
  Board.print (sudoku initial_board)
;;

