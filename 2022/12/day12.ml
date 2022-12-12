module Coordinate = struct
  type t = int * int [@@deriving show]

  let neighbors (x, y) = [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1) ]
  let compare = compare
end

module Cmap = Map.Make (Coordinate)
module Pqueue = Psq.Make (Coordinate) (Int)

let shortest_paths ?(part2 = false) start_coord end_coord coord_map =
  let possible_step current neighbor =
    let current_elevation = Cmap.find current coord_map in
    let neighbor_elevation = Cmap.find neighbor coord_map in
    (current = start_coord && neighbor_elevation = 'a')
    || (current_elevation = 'z' && neighbor = end_coord)
    || current <> start_coord && neighbor <> end_coord
       && Char.code neighbor_elevation - Char.code current_elevation <= 1
  in
  let initial_distance coord elevation =
    if coord = start_coord || (part2 && elevation = 'a') then 0 else Int.max_int
  in
  let tentative =
    Cmap.bindings coord_map
    |> List.map (fun (coord, elevation) ->
           (coord, initial_distance coord elevation))
    |> List.to_seq |> Cmap.of_seq
  in
  let queue =
    Cmap.bindings tentative
    |> List.filter (fun (_coord, distance) -> distance = 0)
    |> List.to_seq |> Pqueue.of_seq
  in

  let rec go queue tentative =
    match Pqueue.pop queue with
    | None -> tentative
    | Some ((current, current_distance), queue) ->
        let tentative, queue =
          List.fold_left
            (fun (tentative, queue) neighbor ->
              match Cmap.find_opt neighbor tentative with
              | Some distance
                when current_distance + 1 < distance
                     && possible_step current neighbor ->
                  ( Cmap.add neighbor (current_distance + 1) tentative,
                    Pqueue.add neighbor (current_distance + 1) queue )
              | Some _ | None -> (tentative, queue))
            (tentative, queue)
            (Coordinate.neighbors current)
        in
        go queue tentative
  in
  go queue tentative

let parse input =
  match
    List.fold_left
      (fun (y, start_coord, end_coord, coord_map) line ->
        let _, start_coord', end_coord', coord_map' =
          String.fold_left
            (fun (x, start_coord, end_coord, coord_map) c ->
              let start_coord =
                match c with 'S' -> Some (x, y) | _ -> start_coord
              in
              let end_coord =
                match c with 'E' -> Some (x, y) | _ -> end_coord
              in
              let coord_map = Cmap.add (x, y) c coord_map in
              (x + 1, start_coord, end_coord, coord_map))
            (0, start_coord, end_coord, coord_map)
            line
        in
        (y + 1, start_coord', end_coord', coord_map'))
      (0, None, None, Cmap.empty)
      input
  with
  | _, Some start_coord, Some end_coord, coord_map ->
      (start_coord, end_coord, coord_map)
  | _ -> failwith "Start or end coordinate missing"

let solve ?(part2 = false) input =
  let start_coord, end_coord, coord_map = parse input in
  shortest_paths ~part2 start_coord end_coord coord_map |> Cmap.find end_coord

let example =
  {|Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi|} |> String.split_on_char '\n'

let%expect_test "part1" =
  print_int (solve example);
  [%expect {| 31 |}];
  print_int (Util.get_input_lines () |> solve);
  [%expect {| 380 |}]

let%expect_test "part2" =
  print_int (solve ~part2:true example);
  [%expect {| 29 |}];
  print_int (Util.get_input_lines () |> solve ~part2:true);
  [%expect {| 375 |}]
