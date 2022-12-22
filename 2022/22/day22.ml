module Coordinate = struct
  type t = int * int

  let compare = compare
end

module Board = struct
  type facing = Left | Right | Up | Down
  type tile = Void | Open | Solid

  let tile_of_char = function '.' -> Open | '#' -> Solid | _ -> Void

  module Cmap = Map.Make (Coordinate)

  type t = { board : tile Cmap.t; width : int; height : int }

  let of_char_list list =
    let enumerate list = List.mapi (fun i line -> (i, line)) list in
    let height = List.length list in
    let width =
      List.fold_left (fun acc line -> max acc (List.length line)) 0 list
    in
    let board =
      List.fold_left
        (fun board (row, line) ->
          List.fold_left
            (fun board (col, tile) ->
              Cmap.add (row, col) (tile_of_char tile) board)
            board (enumerate line))
        Cmap.empty (enumerate list)
    in
    { board; width; height }

  let start board =
    let rec aux i =
      match Cmap.find (0, i) board with Open -> (0, i) | _ -> aux (i + 1)
    in
    aux 0

  let trace_path { board; height; width } instructions =
    let rec move origin (row, col) facing n =
      (* origin is needed because we possibly have to go back to before wrap
         when we encounter a wall *)
      if n = 0 then (row, col)
      else
        let row', col' =
          match facing with
          | Left -> (row, col - 1)
          | Right -> (row, col + 1)
          | Up -> (row - 1, col)
          | Down -> (row + 1, col)
        in
        let row' =
          if row' = height then 0 else if row' = -1 then height - 1 else row'
        in
        let col' =
          if col' = width then 0 else if col' = -1 then width - 1 else col'
        in
        let next = (row', col') in
        match Cmap.find_opt next board with
        | Some Open -> move next next facing (n - 1)
        | Some Solid -> origin
        | _ -> move origin next facing n
    in

    let start = start board in
    List.fold_left
      (fun ((row, col), facing) instruction ->
        match instruction with
        | `Turn_left -> (
            ( (row, col),
              match facing with
              | Up -> Left
              | Left -> Down
              | Down -> Right
              | Right -> Up ))
        | `Turn_right -> (
            ( (row, col),
              match facing with
              | Up -> Right
              | Right -> Down
              | Down -> Left
              | Left -> Up ))
        | `Move number -> (move (row, col) (row, col) facing number, facing))
      (start, Right) instructions
end

module Parser = struct
  open Angstrom

  let line = many1 (satisfy (function ' ' | '.' | '#' -> true | _ -> false))
  let board = many1 (line <* end_of_line)
  let is_digit = function '0' .. '9' -> true | _ -> false

  let number =
    take_while1 is_digit >>| fun number -> `Move (int_of_string number)

  let left = char 'L' *> return `Turn_left
  let right = char 'R' *> return `Turn_right
  let path = many1 (number <|> left <|> right) <* end_of_line

  let parse input =
    parse_string ~consume:All
      (map3 board end_of_line path ~f:(fun board _ path ->
           (Board.of_char_list board, path)))
      input
    |> Result.get_ok
end

let part1 input =
  let board, instructions = Parser.parse input in
  let (row, col), facing = Board.trace_path board instructions in
  (1000 * (row + 1))
  + (4 * (col + 1))
  + match facing with Right -> 0 | Down -> 1 | Left -> 2 | Up -> 3

let example =
  {|        ...#
  .#..
  #...
  ....
...#.......#
........#...
..#....#....
..........#.
  ...#....
  .....#..
  .#......
  ......#.

10R5L5R10L4R5L5
|}

let%expect_test "part1" =
  print_int (part1 example);
  [%expect {| 6032 |}];
  print_int (part1 (Util.get_input_all ()));
  [%expect {| 162186 |}]