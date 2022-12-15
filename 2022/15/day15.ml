module Coordinate = struct
  type t = int * int

  let compare = compare
  let manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
end

module Cset = Set.Make (Coordinate)

module Range = struct
  type t = int * int

  let merge (a, b) (a', b') =
    if a <= b' && b >= a' then Some (min a a', max b b') else None
end

let parse_input =
  List.map (fun line ->
      Scanf.sscanf line "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
        (fun sx sy bx by -> ((sx, sy), (bx, by))))

let ranges y pairs =
  let sorted_ranges =
    let range_at_y y ((sx, sy) as sensor) beacon =
      let distance = Coordinate.manhattan sensor beacon in
      let half_range_size = distance - abs (y - sy) in
      if half_range_size >= 0 then
        Some (sx - half_range_size, sx + half_range_size)
      else None
    in
    List.fold_left
      (fun ranges (sensor, beacon) ->
        match range_at_y y sensor beacon with
        | Some range -> range :: ranges
        | None -> ranges)
      [] pairs
    |> List.sort (fun (a, _) (a', _) -> compare a a')
  in
  let rec merge_heads = function
    | range1 :: range2 :: tail -> (
        match Range.merge range1 range2 with
        | Some range -> merge_heads (range :: tail)
        | None -> range1 :: merge_heads (range2 :: tail))
    | ranges -> ranges
  in
  merge_heads sorted_ranges

let part1 ?(y = 10) pairs =
  let set_of_range (a, b) =
    let rec aux set i =
      if i > b then set else aux (Cset.add (i, y) set) (i + 1)
    in
    aux Cset.empty a
  in
  let no_beacons =
    List.fold_left
      (fun set range -> Cset.union set (set_of_range range))
      Cset.empty (ranges y pairs)
  in
  let beacons = List.map snd pairs |> Cset.of_list in
  Cset.diff no_beacons beacons |> Cset.cardinal

let part2 ?(limit = 4_000_000) pairs =
  let rec aux y =
    if y > limit then failwith "No solution found!"
    else
      let no_beacons =
        List.map (fun (a, b) -> (max 0 a, min limit b)) (ranges y pairs)
      in
      let sum =
        no_beacons |> List.fold_left (fun acc (a, b) -> acc + (b - a)) 0
      in
      if sum < limit then
        let x =
          match no_beacons with
          | (0, a) :: _ -> a + 1
          | (a, b) :: _ -> if b = limit then a - 1 else b + 1
          | _ -> assert false
        in
        (x * 4_000_000) + y
      else aux (y + 1)
  in
  aux 0

let example =
  {|Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3|}
  |> String.split_on_char '\n'

let%expect_test "part1" =
  print_int (parse_input example |> part1);
  [%expect {| 26 |}];
  print_int (Util.get_input_lines () |> parse_input |> part1);
  [%expect {| 5587312 |}]

let%expect_test "part2" =
  print_int (parse_input example |> part2);
  [%expect {| 108000000 |}];
  print_int (Util.get_input_lines () |> parse_input |> part2);
  [%expect {| 12691026767556 |}]
