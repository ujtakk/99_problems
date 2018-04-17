(*
 * 49. Gray code. (*medium*)
 *******************************************************************************
 *
 * An n-bit Gray code is a sequence of n-bit strings constructed according
 * to certain rules. For example,
 *
 * n = 1: C(1) = ['0','1'].
 * n = 2: C(2) = ['00','01','11','10'].
 * n = 3: C(3) = ['000','001','011','010',´110´,´111´,´101´,´100´].
 *
 * Find out the construction rules and write a function with the following
 * specification: `gray n` returns the `n`-bit Gray code.
 *
 * # gray 1;;
 * - : bytes list = ["0"; "1"]
 * # gray 2;;
 * - : bytes list = ["00"; "01"; "11"; "10"]
 * # gray 3;;
 * - : bytes list = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]
 *
 *******************************************************************************
 *)

let rec gray_0 = function
  | 0 -> []
  | 1 -> ["0"; "1"]
  | n ->
      let rest = gray_0 (n-1) in
      let false_part = List.map (fun t -> "0" ^ t) rest in
      let true_part = List.map (fun t -> "1" ^ t) (List.rev rest) in
      false_part @ true_part
;;

let gray = gray_0

let () =
  assert (gray 1 = ["0"; "1"]);
  assert (gray 2 = ["00"; "01"; "11"; "10"]);
  assert (gray 3 = ["000"; "001"; "011"; "010"; "110"; "111"; "101"; "100"]);
;;

