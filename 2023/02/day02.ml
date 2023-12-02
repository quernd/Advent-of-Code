type color = Red | Green | Blue [@@deriving show]
type draw = (int * color) list [@@deriving show]
type game = { id : int; draws : draw list } [@@deriving show]

let is_digit = function '0' .. '9' -> true | _ -> false

let parse_game input =
  let open Angstrom in
  let number = take_while1 is_digit >>| int_of_string in
  let game_info = string "Game " *> number in
  let color' =
    string "red" *> return Red
    <|> string "green" *> return Green
    <|> string "blue" *> return Blue
  in
  let color = lift3 (fun n _ c -> (n, c)) number (string " ") color' in
  let draw = sep_by1 (string ", ") color in
  let draws = sep_by1 (string "; ") draw in
  let p =
    lift3 (fun id _ draws -> { id; draws }) game_info (string ": ") draws
  in
  parse_string ~consume:All p input |> Result.get_ok

let parse_part1 = List.map parse_game

let part1 spec input =
  let possible { draws; _ } =
    List.flatten draws
    |> List.for_all (fun (n, color) ->
           List.for_all (fun (n', color') -> color <> color' || n <= n') spec)
  in
  parse_part1 input |> List.filter possible
  |> List.fold_left (fun acc { id; _ } -> acc + id) 0

let part2 input =
  let update mins (n, color) =
    let min = List.assoc color mins in
    if min < n then (color, n) :: List.remove_assoc color mins else mins
  in
  let power { draws; _ } =
    List.flatten draws
    |> List.fold_left update [ (Red, 0); (Green, 0); (Blue, 0) ]
    |> List.fold_left (fun acc (_, n) -> acc * n) 1
  in
  parse_part1 input |> List.map power
  |> List.fold_left (fun acc power -> acc + power) 0

let example =
  {|Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green|}
  |> String.split_on_char '\n'

let spec =
  (* 12 red cubes, 13 green cubes, and 14 blue cubes *)
  [ (12, Red); (13, Green); (14, Blue) ]

let input = Util.get_input_lines ()

let%expect_test "part1" =
  part1 spec example |> print_int;
  [%expect {| 8 |}];
  part1 spec input |> print_int;
  [%expect {| 2164 |}];
  part2 example |> print_int;
  [%expect {| 2286 |}];
  part2 input |> print_int;
  [%expect {| 69929 |}]
