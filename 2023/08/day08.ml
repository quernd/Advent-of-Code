open Util

let parse input =
  let open Angstrom in
  let direction = char 'R' *> return `Right <|> char 'L' *> return `Left in
  let directions = many1 direction in
  let is_alpha = function '0' .. '9' | 'A' .. 'Z' -> true | _ -> false in
  let label = take_while is_alpha in
  let node =
    lift3
      (fun label left right -> (label, (left, right)))
      (label <* string " = (")
      (label <* string ", ")
      (label <* string ")")
  in
  let p =
    directions <* string "\n\n" >>= fun directions ->
    sep_by1 (char '\n') node >>= fun nodes -> return (directions, nodes)
  in
  parse_string ~consume:Prefix p input |> Result.get_ok

let part1 input =
  let directions, nodes = parse input in
  let directions = Seq.cycle (List.to_seq directions) in
  let rec step acc directions = function
    | "ZZZ" -> acc
    | node ->
        let direction, directions =
          match directions () with
          | Seq.Cons (hd, tl) -> (hd, tl)
          | Nil -> assert false
        in
        let left, right = List.assoc node nodes in
        let next_node =
          match direction with `Left -> left | `Right -> right
        in
        step (acc + 1) directions next_node
  in
  step 0 directions "AAA"

(* let part2 input =
   let directions, all_nodes = parse input in
   let directions = Seq.cycle (List.to_seq directions) in
   let rec full_step acc directions = function
     | nodes when List.for_all (fun label -> label.[2] = 'Z') nodes -> acc
     | nodes ->
         let direction, directions =
           match directions () with
           | Seq.Cons (hd, tl) -> (hd, tl)
           | Nil -> assert false
         in
         let single_step node =
           let left, right = List.assoc node all_nodes in
           match direction with `Left -> left | `Right -> right
         in
         let next_nodes = List.map single_step nodes in
         full_step (acc + 1) directions next_nodes
   in
   full_step 0 directions
     (List.filter_map
        (fun (label, _) -> if label.[2] = 'A' then Some label else None)
        all_nodes) *)

let part2 input =
  let directions_list, all_nodes = parse input in
  let simulate_node node =
    let directions = Seq.cycle (List.to_seq directions_list) in
    let rec step acc directions ?(start = true) node =
      if start && node.[2] = 'Z' then (acc, directions, node)
      else
        let direction, directions =
          match directions () with
          | Seq.Cons (hd, tl) -> (hd, tl)
          | Nil -> assert false
        in
        let left, right = List.assoc node all_nodes in
        let next_node =
          match direction with `Left -> left | `Right -> right
        in
        step (acc + 1) directions next_node
    in
    let cycle_length, _, _ = step 0 directions node in
    cycle_length / List.length directions_list
    (* let cycle_offset, directions, end_node = step 0 directions node in
       let cycle_length, directions, end_node2 =
         step ~start:false 0 directions end_node
       in
       let cycle_length2, _, end_node3 = step ~start:false 0 directions end_node in
       Format.printf "\n%s %d %s %d %s %d %s\n" node cycle_offset end_node
         cycle_length end_node2 cycle_length2 end_node3 *)
  in
  let start_nodes =
    List.filter_map
      (fun (label, _) -> if label.[2] = 'A' then Some label else None)
      all_nodes
  in
  let cycle_lengths = List.map simulate_node start_nodes in
  product cycle_lengths * List.length directions_list

let input = get_input_all ()

let example =
  {|RL

AAA = (BBB, CCC)
BBB = (DDD, EEE)
CCC = (ZZZ, GGG)
DDD = (DDD, DDD)
EEE = (EEE, EEE)
GGG = (GGG, GGG)
ZZZ = (ZZZ, ZZZ)|}

let example' = {|LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)|}

let example2 =
  {|LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)|}

let%expect_test "part1" =
  part1 example |> print_int;
  [%expect {| 2 |}];
  part1 example' |> print_int;
  [%expect {| 6 |}];
  part1 input |> print_int;
  [%expect {| 12169 |}]

let%expect_test "part2" =
  part2 example2 |> print_int;
  [%expect {| 2 |}];
  part2 input |> print_int;
  [%expect {| 12030780859469 |}]
