module Coordinate = struct
  type t = int * int [@@deriving show]

  let compare = compare

  let range (x1, y1) (x2, y2) =
    let xs = List.init (abs (x1 - x2) + 1) (fun x -> min x1 x2 + x) in
    let ys = List.init (abs (y1 - y2) + 1) (fun y -> min y1 y2 + y) in
    match (xs, ys) with
    | [ x ], ys -> List.map (fun y -> (x, y)) ys
    | xs, [ y ] -> List.map (fun x -> (x, y)) xs
    | _ ->
        failwith
          (Format.asprintf "No straight line between %a and %a!" pp (x1, y1) pp
             (x2, y2))
end

module Cave = struct
  module Cmap = Map.Make (Coordinate)
  include Cmap

  type solid_matter = Rock | Sand

  let explore_cave input =
    let rec explore_line cave = function
      | start_coord :: end_coord :: tl ->
          let cave' =
            List.fold_left
              (fun cave coord -> Cmap.add coord Rock cave)
              cave
              (Coordinate.range start_coord end_coord)
          in
          explore_line cave' (end_coord :: tl)
      | _ -> cave
    in
    List.fold_left (fun cave line -> explore_line cave line) Cmap.empty input

  let pour_sand ?(origin = (500, 0)) ~floor ~abyss cave =
    let rec fill cave acc =
      let rec fall = function
        | coord when abyss coord -> coord
        | coord -> (
            let try_step f =
              Either.fold
                ~left:(fun (x, y) ->
                  let coord' = f x y in
                  match (Cmap.find_opt coord' cave, floor coord') with
                  | None, false -> Either.Right coord'
                  | _ -> Left (x, y))
                ~right:Either.right
            in
            match
              Either.left coord
              |> try_step (fun x y -> (x, y + 1))
              |> try_step (fun x y -> (x - 1, y + 1))
              |> try_step (fun x y -> (x + 1, y + 1))
            with
            | Left coord -> coord
            | Right coord -> fall coord)
      in
      match fall origin with
      | coord when abyss coord -> (cave, `Abyss acc)
      | coord when coord = origin -> (cave, `Origin (acc + 1))
      | coord -> fill (Cmap.add coord Sand cave) (acc + 1)
    in
    fill cave 0

  let lowest_rock cave =
    Cmap.bindings cave |> List.map fst |> List.map snd |> List.fold_left max 0
end

module Parser = struct
  open Angstrom

  let is_digit = function '0' .. '9' -> true | _ -> false
  let integer = take_while1 is_digit >>| int_of_string
  let comma = char ','

  let coordinate =
    map3 integer comma integer ~f:(fun start_coord _ end_coord ->
        (start_coord, end_coord))

  let line = sep_by (string " -> ") coordinate <* end_of_line

  let parse_input input =
    parse_string ~consume:All (many line) input |> Result.get_ok
end

let example = {|498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9
|}

let part1 cave =
  let lowest_rock = Cave.lowest_rock cave in
  match
    Cave.pour_sand
      ~abyss:(fun (_, y) -> y >= lowest_rock)
      ~floor:(fun _ -> false)
      cave
  with
  | _cave, `Abyss units -> units
  | _ -> failwith "Abyss not found!"

let part2 cave =
  let floor = Cave.lowest_rock cave + 2 in
  match
    Cave.pour_sand ~abyss:(fun _ -> false) ~floor:(fun (_, y) -> y = floor) cave
  with
  | _cave, `Origin units -> units
  | _ -> failwith "Origin not reached!"

let%expect_test "part1" =
  let cave = Parser.parse_input example |> Cave.explore_cave in
  print_int (part1 cave);
  [%expect {| 24 |}];
  let cave = Parser.parse_input (Util.get_input_all ()) |> Cave.explore_cave in
  print_int (part1 cave);
  [%expect {| 715 |}]

let%expect_test "part2" =
  let cave = Parser.parse_input example |> Cave.explore_cave in
  print_int (part2 cave);
  [%expect {| 93 |}];
  let cave = Parser.parse_input (Util.get_input_all ()) |> Cave.explore_cave in
  print_int (part2 cave);
  [%expect {| 25248 |}]
