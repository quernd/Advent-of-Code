module Coordinate = struct
  type t = int * int
  type move = U | D | L | R | NOOP

  let move (x, y) = function
    | U -> (x, y - 1)
    | D -> (x, y + 1)
    | L -> (x - 1, y)
    | R -> (x + 1, y)
    | NOOP -> (x, y)

  let ( - ) (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
  let compare = compare
end

module Rope = struct
  type t = Coordinate.t list

  let tail_moves head tail =
    let open Coordinate in
    let move_of_x = function
      | 1 | 2 -> R
      | -1 | -2 -> L
      | 0 -> NOOP
      | _ -> assert false
    in
    let move_of_y = function
      | 1 | 2 -> D
      | -1 | -2 -> U
      | 0 -> NOOP
      | _ -> assert false
    in
    let moves =
      let x, y = head - tail in
      if abs x = 2 || abs y = 2 then [ move_of_x x; move_of_y y ] else []
    in
    moves

  let move dir rope =
    let rec follow_head = function
      | first :: second :: rest ->
          let second' =
            List.fold_left Coordinate.move second (tail_moves first second)
          in
          let tail, rest' = follow_head (second' :: rest) in
          (tail, first :: rest')
      | [ tail ] -> (tail, [ tail ])
      | _ -> assert false
    in
    let head = Coordinate.move (List.hd rope) dir in
    follow_head (head :: List.tl rope)
end

module Cmap = Map.Make (Coordinate)

let move ?(rope = [ (0, 0); (0, 0) ]) moves =
  let _rope, visited =
    List.fold_left
      (fun (rope, visited) move ->
        let rec make_move rope visited move =
          match move with
          | _, 0 -> (rope, visited)
          | dir, distance ->
              let last, rope' = Rope.move dir rope in
              let visited' = Cmap.add last true visited in
              make_move rope' visited' (dir, distance - 1)
        in
        make_move rope visited move)
      (rope, Cmap.empty |> Cmap.add (0, 0) true)
      moves
  in
  Cmap.cardinal visited

let parse input =
  List.map
    (fun line ->
      Scanf.sscanf line "%c %i" (fun c i ->
          match c with
          | 'U' -> (Coordinate.U, i)
          | 'D' -> (D, i)
          | 'L' -> (L, i)
          | 'R' -> (R, i)
          | _ -> failwith (Format.sprintf "%c is not a valid direction!" c)))
    input

let%expect_test "part1" =
  let example1 =
    {|R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2|} |> String.split_on_char '\n'
  in
  print_int (parse example1 |> move);
  [%expect {| 13 |}];
  print_int (Util.(get_input () |> lines_of_channel) |> parse |> move);
  [%expect {| 5858 |}]

let%expect_test "part2" =
  let example2 =
    {|R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20|} |> String.split_on_char '\n'
  in
  let rope = List.init 10 (fun _ -> (0, 0)) in
  print_int (parse example2 |> move ~rope);
  [%expect {| 36 |}];
  print_int (Util.(get_input () |> lines_of_channel) |> parse |> move ~rope);
  [%expect {| 2602 |}]
