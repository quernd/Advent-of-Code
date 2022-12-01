let grouper (groups, group) = function
  | "" -> (group :: groups, [])
  | calories -> (groups, calories :: group)

let group_calories channel =
  match Util.fold_channel grouper ([], []) channel with
  | groups, group -> group :: groups

let group_calories_list lines =
  match List.fold_left grouper ([], []) lines with
  | groups, group -> group :: groups

let sum_calories items = List.(map int_of_string items |> fold_left ( + ) 0)
let part1 elves = List.(map sum_calories elves |> sort Int.compare |> rev |> hd)

let part2 elves =
  match List.(map sum_calories elves |> sort Int.compare |> rev) with
  | a :: b :: c :: _ -> a + b + c
  | _ -> failwith "List too short"

let example =
  {|1000
2000
3000

4000

5000
6000

7000
8000
9000

10000|}
  |> String.split_on_char '\n'

let%expect_test "part1" =
  print_int (group_calories_list example |> part1);
  [%expect {|24000|}];
  let input = Util.get_input () in
  print_int (group_calories input |> part1);
  [%expect {|69912|}]

let%expect_test "part2" =
  print_int (group_calories_list example |> part2);
  [%expect {|45000|}];
  let input = Util.get_input () in
  print_int (group_calories input |> part2);
  [%expect {|208180|}]
