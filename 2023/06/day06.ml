open Util

let parse ?(part2 = false) input =
  let open Angstrom in
  let whitespace = many1 (char ' ') in
  let number = take_while1 P.is_digit in
  let numbers = sep_by1 whitespace number in
  let numbers' =
    if part2 then
      numbers >>| fun numbers ->
      let number = String.concat "" numbers in
      [ int_of_string number ]
    else numbers >>| List.map int_of_string
  in
  let time = string "Time:" *> whitespace *> numbers' <* char '\n' in
  let distance = string "Distance:" *> whitespace *> numbers' <* char '\n' in
  let p = lift2 (fun time distance -> (time, distance)) time distance in
  let times, distances = parse_string ~consume:All p input |> Result.get_ok in
  zip [ times; distances ]

let solve ?(part2 = false) input =
  let ways_to_beat_record time record =
    let rec aux winners pressed =
      if pressed > time then winners
      else
        let distance = pressed * (time - pressed) in
        let winners = if distance > record then winners + 1 else winners in
        aux winners (pressed + 1)
    in
    aux 0 0
  in
  let races = parse ~part2 input in
  List.fold_left
    (fun acc -> function
      | [ time; record ] -> acc * ways_to_beat_record time record
      | _ -> assert false)
    1 races

let example = {|Time:      7  15   30
Distance:  9  40  200
|}

let input = get_input_all ()

let%expect_test "part1" =
  solve example |> print_int;
  [%expect {| 288 |}];
  solve input |> print_int;
  [%expect {| 781200 |}];
  solve ~part2:true example |> print_int;
  [%expect {| 71503 |}];
  solve ~part2:true input |> print_int;
  [%expect {| 49240091 |}]
