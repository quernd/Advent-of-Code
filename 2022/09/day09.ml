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
    let x, y = head - tail in
    if abs x = 2 || abs y = 2 then [ move_of_x x; move_of_y y ] else []

  let move dir rope =
    let rec follow_head acc head = function
      | second :: rest ->
          let second' =
            List.fold_left Coordinate.move second (tail_moves head second)
          in
          follow_head
            (fun tail rest' -> acc tail (second' :: rest'))
            second' rest
      | [] -> acc head []
    in
    let head = Coordinate.move (List.hd rope) dir in
    follow_head (fun tail rope' -> (tail, head :: rope')) head (List.tl rope)
end

module Cset = Set.Make (Coordinate)

let move ?(rope = [ (0, 0); (0, 0) ]) moves =
  let _rope, visited =
    List.fold_left
      (fun (rope, visited) move ->
        let rec make_move rope visited move =
          match move with
          | _, 0 -> (rope, visited)
          | dir, distance ->
              let last, rope' = Rope.move dir rope in
              let visited' = Cset.add last visited in
              make_move rope' visited' (dir, distance - 1)
        in
        make_move rope visited move)
      (rope, Cset.singleton (0, 0))
      moves
  in
  Cset.cardinal visited

let parse =
  List.map (fun line ->
      Scanf.sscanf line "%c %i" (fun c i ->
          match c with
          | 'U' -> (Coordinate.U, i)
          | 'D' -> (D, i)
          | 'L' -> (L, i)
          | 'R' -> (R, i)
          | _ -> failwith (Format.sprintf "%c is not a valid direction!" c)))

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
