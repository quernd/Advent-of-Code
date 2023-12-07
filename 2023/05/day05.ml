open Util

type map = { dest : int; source : int; range : int }

let parse input =
  let open Angstrom in
  let number = take_while1 P.is_digit >>| int_of_string in
  let numbers = sep_by1 (char ' ') number in
  let seeds = string "seeds: " *> numbers <* string "\n\n" in
  let header = take_while1 (( <> ) '\n') *> char '\n' in
  let map_line =
    lift3
      (fun dest source range -> { dest; source; range })
      (number <* char ' ')
      (number <* char ' ')
      number
  in
  let map' = header *> sep_by1 (char '\n') map_line in
  let maps = sep_by1 (string "\n\n") map' in
  let p = lift2 (fun seeds maps -> (seeds, maps)) seeds maps in
  parse_string ~consume:All p input |> Result.get_ok

let map' { dest; source; range } x =
  if x >= source && x < source + range then Some (x + (dest - source)) else None

let map_seed maps seed =
  List.fold_left
    (fun acc map_lines ->
      let rec aux acc map_lines =
        match map_lines with
        | [] -> acc
        | hd :: tl -> (
            match map' hd acc with Some x -> x | None -> aux acc tl)
      in
      aux acc map_lines)
    seed maps

let part1 input =
  let seeds, maps = parse input in
  let locations = List.map (map_seed maps) seeds in
  List.(fold_left min (hd locations) (tl locations))

let part2 input =
  let seeds, maps = parse input in
  let min' = ref max_int in
  let rec aux = function
    | start :: range :: tail ->
        for i = 0 to range - 1 do
          min' := min !min' (map_seed maps (start + i))
        done;
        aux tail
    | [] -> !min'
    | _ -> assert false
  in
  aux seeds

let example =
  {|seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
|}

let input = get_input_all ()

let%expect_test "part1" =
  part1 example |> print_int;
  [%expect {| 35 |}];
  part1 input |> print_int;
  [%expect {| 993500720 |}];
  part2 example |> print_int;
  [%expect {| 46 |}];
  part2 input |> print_int;
  [%expect {| 4917124 |}]
