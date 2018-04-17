(*
 * 68. Preorder and inorder sequences of binary trees. (*medium*)
 *******************************************************************************
 *
 * We consider binary trees with nodes that are identified by single
 * lower-case letters, as in the example of the previous problem.
 *
 * 1.  Write functions `preorder` and `inorder` that construct the
 *     [preorder](https://en.wikipedia.org/wiki/Tree_traversal#Pre-order)
 *     and
 *     [inorder](https://en.wikipedia.org/wiki/Tree_traversal#In-order_.28symmetric.29)
 *     sequence of a given binary tree, respectively. The results should be
 *     atoms, e.g. 'abdecfg' for the preorder sequence of the example in
 *     the previous problem.
 * 2.  Can you use `preorder` from problem part 1 in the reverse direction;
 *     i.e. given a preorder sequence, construct a corresponding tree? If
 *     not, make the necessary arrangements.
 * 3.  If both the preorder sequence and the inorder sequence of the nodes
 *     of a binary tree are given, then the tree is determined
 *     unambiguously. Write a function `pre_in_tree` that does the job.
 * 4.  Solve problems 1 to 3 using [difference
 *     lists](https://en.wikipedia.org/wiki/Difference_list). Cool\! Use
 *     the function `timeit` (defined in problem “[Compare the two methods
 *     of calculating Euler's totient
 *     function.](#ComparethetwomethodsofcalculatingEuler39stotientfunctioneasy)”)
 *     to compare the solutions.
 *
 * What happens if the same character appears in more than one node. Try
 * for instance `pre_in_tree "aba" "baa"`.
 *
 * # preorder (Node (1, Node (2, Empty, Empty), Empty));;
 * - : int list = [1; 2]
 * # preorder (Node (1, Empty, Node (2, Empty, Empty)));;
 * - : int list = [1; 2]
 * # let p = preorder example_tree;;
 * val p : char list = ['a'; 'b'; 'd'; 'e'; 'c'; 'f'; 'g']
 * # let i = inorder example_tree;;
 * val i : char list = ['d'; 'b'; 'e'; 'a'; 'c'; 'g'; 'f']
 * # pre_in_tree p i = example_tree;;
 * - : bool = true
 *
 *******************************************************************************
 *)

let preorder_0 =
;;

let inorder_0 =
;;

let preorder = preorder_0
let inorder = inorder_0

let () =
  assert (preorder (Node (1, Node (2, Empty, Empty), Empty)) = [1; 2]);
  assert (preorder (Node (1, Empty, Node (2, Empty, Empty))) = [1; 2]);

  let p = preorder example_tree in
  assert (p = ['a'; 'b'; 'd'; 'e'; 'c'; 'f'; 'g']);

  let i = inorder example_tree in
  assert (i = ['d'; 'b'; 'e'; 'a'; 'c'; 'g'; 'f']);

  assert (pre_in_tree p i = example_tree);
;;

