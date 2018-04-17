let fact n =
  let rec iter acc = function
    | 0 -> acc
    | n -> iter (n*acc) (n-1)
  in
  iter 1 n
;;

let combi n r = (fact n) / (fact (n-r) * fact r)

let remove_at target index =
  let rec iter acc count = function
    | [] -> List.rev acc
    | h :: t -> if count = index then
                  iter acc (count+1) t
                else
                  iter (h :: acc) (count+1) t
  in
  iter [] 0 target
;;

let rec range a b =
  match a, b with
  | x, y when x = y -> []
  | x, y when x < y -> x :: range (x+1) y
  | x, y when x > y -> x :: range (x-1) y
  | _ -> []
;;

let rec order = function
  | [] -> [[]]
  | lst ->
    let len = List.length lst in
    let generate idx =
      let item = List.nth lst idx in
      let cand = remove_at lst idx in
      List.map (fun l -> item :: l) (order cand)
    in
    let target = List.map generate (range 0 len) in
    List.fold_left (@) [] target

let rec gen_logic = function
  | 0 -> []
  | 1 -> [[true]; [false]]
  | n ->
      let rest = gen_logic (n-1) in
      let true_part = List.map (fun t -> true :: t) rest in
      let false_part = List.map (fun t -> false :: t) rest in
      true_part @ false_part

let append l1 l2 =
  let rec loop acc x y =
    match x, y with
    | [], [] -> List.rev acc
    | [], h :: t -> loop (h :: acc) [] t
    | h :: t, l -> loop (h :: acc) t l
  in
  loop [] l1 l2

