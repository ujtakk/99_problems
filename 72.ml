(*
 * 72. Construct the bottom-up order sequence of the tree nodes. (*easy*)
 *******************************************************************************
 *
 * Write a function `bottom_up t` which constructs the bottom-up sequence
 * of the nodes of the multiway tree `t`.
 *
 * # bottom_up (T('a', [T('b', [])]));;
 * - : char list = ['b'; 'a']
 * # bottom_up t;;
 * - : char list = ['g'; 'f'; 'c'; 'd'; 'e'; 'b'; 'a']
 *
 *******************************************************************************
 *)

type 'a mult_tree = T of 'a * 'a mult_tree list

let append l1 l2 =
  let rec iter acc x y =
    match x, y with
    | [], [] -> List.rev acc
    | [], h :: t -> iter (h :: acc) [] t
    | h :: t, l -> iter (h :: acc) t l
  in
  iter [] l1 l2
;;

let rec bottom_up_0 = function
  | T (id, trees) ->
      let rec iter acc = function
        | [] -> append acc [id]
        | h :: t -> iter (append acc (bottom_up_0 h)) t
      in
      iter [] trees
;;

let bottom_up = bottom_up_0

let () =
  assert (bottom_up (T('a', [T('b', [])])) = ['b'; 'a']);

  let t = T('a', [T('f',[T('g',[])]); T('c',[]);
            T('b',[T('d',[]); T('e',[])])])
  in
  assert (bottom_up t = ['g'; 'f'; 'c'; 'd'; 'e'; 'b'; 'a']);
;;

