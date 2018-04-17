(*
 * 50. Huffman code (*hard*)
 *******************************************************************************
 *
 * First of all, consult a good book on discrete mathematics or algorithms
 * for a detailed description of Huffman codes (you can start with the
 * [Wikipedia page](http://en.wikipedia.org/wiki/Huffman_coding))\!
 *
 * We consider a set of symbols with their frequencies. For example, if the
 * alphabet is `"a"`,..., `"f"` (represented as the positions 0,...5) and
 * respective frequencies are 45, 13, 12, 16, 9, 5:
 *
 * # let fs = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16);
 *              ("e", 9); ("f", 5) ];;
 * val fs : (string * int) list =
 *   [("a", 45); ("b", 13); ("c", 12); ("d", 16); ("e", 9); ("f", 5)]
 *
 * Our objective is to construct the Huffman code `c` word for all symbols
 * `s`. In our example, the result could be `hs = [("a", "0"); ("b",
 * "101"); ("c", "100"); ("d", "111"); ("e", "1101"); ("f", "1100")]` (or
 * `hs = [ ("a", "1");...]`). The task shall be performed by the function
 * `huffman` defined as follows: `huffman(fs)` returns the Huffman code
 * table for the frequency table `fs`
 *
 * # huffman fs;;
 * - : (string * string) list =
 * [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101");
 *  ("d", "111")]
 * # huffman ["a", 10;  "b", 15;  "c", 30;  "d", 16;  "e", 29];;
 * - : (string * string) list =
 * [("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")]
 *
 *******************************************************************************
 *)

(*********************

   d  e f   b c  a
   |  | |   | |  |
   | 1+-+0 1+-+0 |
   |   |     |   |
  1+---+0    |   |
     |       |   |
    1+-------+0  |
         |       |
        1+-------+0
             |

*********************)

type 'a binary_tree =
  | Empty
  | Node of 'a * 'a binary_tree * 'a binary_tree
;;

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

let huffman_0 series =
  let sort =
    let metric = function
      | Empty -> 0
      | Node ((tag, freq), a, b) -> freq
    in
    bubble_sort metric
  in

  let rec iter_leaf acc = function
    | [] -> List.rev acc
    | h :: t -> iter_leaf (Node (h, Empty, Empty) :: acc) t
  in

  let concat_tree a b =
    match a, b with
    | Empty, Empty -> Empty
    | _, Empty -> a
    | Empty, _ -> b
    | Node ((a_tag, a_freq), _, _), Node ((b_tag, b_freq), _, _) ->
      Node ((a_tag^b_tag, a_freq+b_freq), a, b)
  in

  let annotate tree =
    let append l1 l2 =
      let rec iter acc x y =
        match x, y with
        | [], [] -> List.rev acc
        | [], h :: t -> iter (h :: acc) [] t
        | h :: t, l -> iter (h :: acc) t l
      in
      iter [] l1 l2
    in
    let rec bind prefix = function
      | Empty -> []
      | Node ((tag, freq), Empty, Empty) -> [(tag, prefix)]
      | Node ((tag, freq), a, b) ->
          append (bind (prefix^"0") a) (bind (prefix^"1") b)
    in
    bind "" tree
  in

  let rec iter tree = function
    | [] -> tree
    | last :: [] ->
        iter last []
    | fst :: snd :: rest ->
        let new_tree = concat_tree fst snd in
        iter new_tree (sort (new_tree :: rest))
  in

  annotate @@ iter Empty (sort @@ iter_leaf [] series)
;;

let huffman = huffman_0

let () =
  let fs = [ ("a", 45); ("b", 13); ("c", 12); ("d", 16);
             ("e", 9); ("f", 5) ]
  in
  assert (
    huffman fs
    = [("a", "0"); ("c", "100"); ("b", "101"); ("f", "1100"); ("e", "1101");
       ("d", "111")]
  );

  assert (
    huffman ["a", 10;  "b", 15;  "c", 30;  "d", 16;  "e", 29]
    = [("d", "00"); ("a", "010"); ("b", "011"); ("e", "10"); ("c", "11")]
  );
;;

