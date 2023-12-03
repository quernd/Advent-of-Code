open Util

let parse line =
  let open Angstrom in
  let number = take_while1 P.is_digit >>| fun number -> `Number number in
  let nothing = char '.' *> return `Nothing in
  let gear = char '*' *> return `Gear in
  let symbol = any_char *> return `Symbol in
  Angstrom.parse_string ~consume:All
    (number <|> nothing <|> gear <|> symbol |> many)
    line
  |> Result.get_ok

let part1 input =
  let symbols = Util.Cset.empty in
  let numbers = Util.Cmap.empty in
  let tokens = List.map parse input in
  let _row, symbols, numbers =
    List.fold_left
      (fun (row, symbols, numbers) tokens ->
        let _col, symbols, numbers =
          List.fold_left
            (fun (col, symbols, numbers) -> function
              | `Nothing -> (col + 1, symbols, numbers)
              | `Gear | `Symbol ->
                  let symbols = Cset.add (row, col) symbols in
                  (col + 1, symbols, numbers)
              | `Number number ->
                  let numbers = Cmap.add (row, col) number numbers in
                  (col + String.length number, symbols, numbers))
            (0, symbols, numbers) tokens
        in
        (row + 1, symbols, numbers))
      (0, symbols, numbers) tokens
  in
  let adj =
    Cset.fold
      (fun coord adj -> Cset.add_seq (List.to_seq (Coordinate.adj' coord)) adj)
      symbols Cset.empty
  in
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
  [%expect {| 517021 |}]
