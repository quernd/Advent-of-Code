module Coordinate3 = struct
  type t = int * int * int [@@deriving show]

  let neighbors (x, y, z) =
    [
      (x + 1, y, z);
      (x - 1, y, z);
      (x, y + 1, z);
      (x, y - 1, z);
      (x, y, z + 1);
      (x, y, z - 1);
    ]

  let ( + ) (x, y, z) (x', y', z') = (x + x', y + y', z + z')
  let compare = compare
end

module Cset3 = struct
  include Set.Make (Coordinate3)

  let of_list = List.fold_left (fun cubes cube -> add cube cubes) empty

  let bounds set =
    fold
      (fun (x, y, z) ((min_x, min_y, min_z), (max_x, max_y, max_z)) ->
        ( (min min_x x, min min_y y, min min_z z),
          (max max_x x, max max_y y, max max_z z) ))
      set
      ((max_int, max_int, max_int), (min_int, min_int, min_int))
end

let parse_input =
  List.map (fun line -> Scanf.sscanf line "%i,%i,%i" (fun x y z -> (x, y, z)))

let example =
  {|2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5|}
  |> String.split_on_char '\n'

let part1 cubes =
  let lava = Cset3.of_list cubes in
  List.fold_left
    (fun acc cube ->
      let surface =
        List.filter
          (fun neighbor -> not (Cset3.mem neighbor lava))
          (Coordinate3.neighbors cube)
        |> List.length
      in
      surface + acc)
    0 cubes

let part2 cubes =
  let lava = Cset3.of_list cubes in
  let min_bound, max_bound = Cset3.bounds lava in
  let min_x, min_y, min_z = Coordinate3.(min_bound + (-1, -1, -1)) in
  let max_x, max_y, max_z = Coordinate3.(max_bound + (1, 1, 1)) in
  let out_of_bounds (x, y, z) =
    x < min_x || y < min_y || z < min_z || x > max_x || y > max_y || z > max_z
  in
  let rec aux visited = function
    | [] -> 0
    | hd :: tl ->
        if Cset3.mem hd visited then aux visited tl
        else
          let visited = Cset3.add hd visited in
          let lava_neighbors, air_neighbors =
            List.partition
              (fun cube -> Cset3.mem cube lava)
              (Coordinate3.neighbors hd)
          in
          let surface = List.length lava_neighbors in
          let to_visit =
            List.filter
              (fun coord ->
                (not (out_of_bounds coord)) && not (Cset3.mem coord visited))
              air_neighbors
          in
          surface + aux visited (to_visit @ tl)
  in
  aux Cset3.empty [ (min_x, min_y, min_z) ]

let%expect_test "part1" =
  parse_input example |> part1 |> print_int;
  [%expect {| 64 |}];
  Util.get_input_lines () |> parse_input |> part1 |> print_int;
  [%expect {| 4322 |}]

let%expect_test "part2" =
  parse_input example |> part2 |> print_int;
  [%expect {| 58 |}];
  Util.get_input_lines () |> parse_input |> part2 |> print_int;
  [%expect {| 2516 |}]
