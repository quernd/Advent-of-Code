open Util

let parse_line line =
  let open Angstrom in
  let number = take_while1 P.is_digit >>| fun number -> `Number number in
  let nothing = char '.' *> return `Nothing in
  let gear = char '*' *> return `Gear in
  let symbol = any_char *> return `Symbol in
  Angstrom.parse_string ~consume:All
    (number <|> nothing <|> gear <|> symbol |> many)
    line
  |> Result.get_ok

let parse input =
  let tokens = List.map parse_line input in
  let symbols = Util.Cmap.empty in
  let numbers = Util.Cmap.empty in
  let _row, symbols, numbers =
    List.fold_left
      (fun (row, symbols, numbers) tokens ->
        let _col, symbols, numbers =
          List.fold_left
            (fun (col, symbols, numbers) -> function
              | `Nothing -> (col + 1, symbols, numbers)
              | (`Gear | `Symbol) as symbol ->
                  let symbols = Cmap.add (row, col) symbol symbols in
                  (col + 1, symbols, numbers)
              | `Number number ->
                  let numbers = Cmap.add (row, col) number numbers in
                  (col + String.length number, symbols, numbers))
            (0, symbols, numbers) tokens
        in
        (row + 1, symbols, numbers))
      (0, symbols, numbers) tokens
  in
  (symbols, numbers)

let adj symbols =
  Cmap.fold
    (fun coord _ adj -> Cset.add_seq (List.to_seq (Coordinate.adj' coord)) adj)
    symbols Cset.empty

let part1 input =
  let symbols, numbers = parse input in
  let adj = adj symbols in
  let part_numbers =
    Cmap.filter_map
      (fun coord number ->
        let coords =
          List.init (String.length number) (fun i ->
              Coordinate.(coord + (0, i)))
        in
        if List.exists (fun coord -> Cset.mem coord adj) coords then
          Some (int_of_string number)
        else None)
      numbers
    |> Cmap.bindings |> List.map snd
  in
  Util.sum part_numbers

let part2 input =
  let symbols, numbers = parse input in
  let gears =
    Cmap.filter_map (fun _ -> function `Gear -> Some [] | _ -> None) symbols
  in
  let gears =
    Cmap.fold
      (fun coord number gears ->
        let coords =
          List.init (String.length number) (fun i ->
              Coordinate.(coord + (0, i)))
        in
        let number = int_of_string number in
        let adj_gears =
          List.fold_left
            (fun adj_gears coord ->
              let adj_coords = Coordinate.adj' coord in
              List.fold_left
                (fun acc coord -> Cset.add coord acc)
                adj_gears adj_coords)
            Cset.empty coords
        in
        Cset.fold
          (fun gear gears ->
            Cmap.update gear
              (function Some list -> Some (number :: list) | _ -> None)
              gears)
          adj_gears gears)
      numbers gears
  in
  Cmap.filter_map
    (fun _ -> function [ one; two ] -> Some (one * two) | _ -> None)
    gears
  |> Cmap.bindings |> List.map snd |> Util.sum

let example =
  {|467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598..|}
  |> String.split_on_char '\n'

let input = get_input_lines ()

let%expect_test "part1" =
  part1 example |> print_int;
  [%expect {| 4361 |}];
  part1 input |> print_int;
  [%expect {| 517021 |}];
  part2 example |> print_int;
  [%expect {| 467835 |}];
  part2 input |> print_int;
  [%expect {| 81296995 |}]
