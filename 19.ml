(*
 * 19. Rotate a list N places to the left. (*medium*)
 *******************************************************************************
 *
 * # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
 * - : string list = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
 * # rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2);;
 * - : string list = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
 *
 *******************************************************************************
 *)

let rec rotate_0 lst = function
  | i when i > 0 -> begin
      match lst with
      | [] -> []
      | h :: t -> rotate_0 (t @ [h]) (i-1)
    end
  | i when i < 0 -> begin
      match (List.rev lst) with
      | [] -> []
      | h :: t -> rotate_0 (h :: (List.rev t)) (i+1)
    end
  | _ -> lst
;;

let rotate = rotate_0

let () =
  assert (
    rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3
    = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"]
  );

  assert (
    rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] (-2)
    = ["g"; "h"; "a"; "b"; "c"; "d"; "e"; "f"]
  );
;;
