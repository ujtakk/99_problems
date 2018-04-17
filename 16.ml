(*
 * 16. Drop every N'th element from a list. (*medium*)
 *******************************************************************************
 *
 * # drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3;;
 * - : string list = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
 *
 *******************************************************************************
 *)

let drop_0 l n =
  let rec iter max count acc = function
    | [] -> acc
    | h :: t -> if count = 1 then
                  iter max max acc t
                else
                  iter max (count-1) (h :: acc) t
  in
  List.rev @@ iter n n [] l
;;

let drop = drop_0

let () =
  assert (
    drop ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"] 3
    = ["a"; "b"; "d"; "e"; "g"; "h"; "j"]
  );
;;
