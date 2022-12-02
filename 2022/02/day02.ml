type hand = Rock | Paper | Scissors
type strategy = X | Y | Z
type outcome = Lose | Draw | Win
type play = { yours : hand; mine : strategy }

let hand_of_char = function
  | 'A' -> Rock
  | 'B' -> Paper
  | 'C' -> Scissors
  | c -> failwith (Format.sprintf "%c is not a valid hand." c)

let strategy_of_char = function
  | 'X' -> X
  | 'Y' -> Y
  | 'Z' -> Z
  | c -> failwith (Format.sprintf "%c is not a valid strategy." c)

let round_of_string s =
  Scanf.sscanf s "%c %c" (fun yours mine ->
      { yours = hand_of_char yours; mine = strategy_of_char mine })

let hand_points = function Rock -> 1 | Paper -> 2 | Scissors -> 3

let win_points yours mine =
  match (yours, mine) with
  | Rock, Paper | Paper, Scissors | Scissors, Rock -> 6
  | yours, mine when yours = mine -> 3
  | _ -> 0

let strategy1 { yours; mine } =
  let mine = match mine with X -> Rock | Y -> Paper | Z -> Scissors in
  win_points yours mine + hand_points mine

let strategy2 { yours; mine } =
  let strategy = match mine with X -> Lose | Y -> Draw | Z -> Win in
  let mine =
    match (yours, strategy) with
    | Rock, Draw | Paper, Lose | Scissors, Win -> Rock
    | Paper, Draw | Scissors, Lose | Rock, Win -> Paper
    | Scissors, Draw | Rock, Lose | Paper, Win -> Scissors
  in
  win_points yours mine + hand_points mine

let points_of_round strategy lines =
  List.(map round_of_string lines |> map strategy |> fold_left ( + ) 0)

let part1 = points_of_round strategy1
let part2 = points_of_round strategy2

let example = {|A Y
B X
C Z|} |> String.split_on_char '\n'

let%expect_test "part1" =
  print_int (part1 example);
  [%expect {| 15 |}];
  let input = Util.get_input () in
  print_int (part1 (Util.lines_of_channel input));
  [%expect {| 12740 |}]

let%expect_test "part2" =
  print_int (part2 example);
  [%expect {| 12 |}];
  let input = Util.get_input () in
  print_int (part2 (Util.lines_of_channel input));
  [%expect {| 11980 |}]
