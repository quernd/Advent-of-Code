open Util

let parse input =
  let open Angstrom in
  let value =
    take_while1 (function '0' .. '9' | '-' -> true | _ -> false)
    >>| int_of_string
  in
  let history = sep_by1 (char ' ') value in
  parse_string ~consume:Prefix (sep_by1 (char '\n') history) input
  |> Result.get_ok

let oasis ~part2 history =
  let differences ~part2 sequence =
    let last, differences =
      List.fold_left_map
        (fun prev value ->
          match (prev, value) with
          | None, value -> (Some value, None)
          | Some prev, value ->
              let diff = if part2 then prev - value else value - prev in
              (Some value, Some diff))
        None sequence
    in
    (last |> Option.get, differences |> List.filter_map Fun.id)
  in
  let rec aux ~part2 sequence =
    if List.for_all (( = ) 0) sequence then 0
    else
      let last, differences = differences ~part2 sequence in
      if part2 then last - aux ~part2 differences
      else last + aux ~part2 differences
  in
  if part2 then aux ~part2:true (List.rev history) else aux ~part2:false history

let part1 input = List.map (oasis ~part2:false) (parse input) |> sum
let part2 input = List.map (oasis ~part2:true) (parse input) |> sum

let example = {|0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45|}

let input = get_input_all ()

let%expect_test "part1" =
  part1 example |> print_int;
  [%expect {| 114 |}];
  part1 input |> print_int;
  [%expect {| 1708206096 |}]

let%expect_test "part2" =
  part2 "10 13 16 21 30 45" |> print_int;
  [%expect {| 5 |}];
  part2 input |> print_int;
  [%expect {| 1050 |}]
