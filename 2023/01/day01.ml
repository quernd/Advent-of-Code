let is_digit = function '0' .. '9' -> true | _ -> false
let first_digit = List.find is_digit
let last_digit chars = List.rev chars |> first_digit

let part1 input =
  let aux line =
    let chars = String.to_seq line |> List.of_seq in
    Format.sprintf "%c%c" (first_digit chars) (last_digit chars)
    |> int_of_string
  in
  input |> List.fold_left (fun acc line -> acc + aux line) 0

let reverse_string s =
  String.to_seq s |> List.of_seq |> List.rev |> List.to_seq |> String.of_seq

let part2 input =
  let aux line =
    let open Angstrom in
    let p word value =
      ( (string (string_of_int value) <|> string word) *> return (Some value),
        (string (string_of_int value) <|> string (reverse_string word))
        *> return (Some value) )
    in

    let one, one' = p "one" 1 in
    let two, two' = p "two" 2 in
    let three, three' = p "three" 3 in
    let four, four' = p "four" 4 in
    let five, five' = p "five" 5 in
    let six, six' = p "six" 6 in
    let seven, seven' = p "seven" 7 in
    let eight, eight' = p "eight" 8 in
    let nine, nine' = p "nine" 9 in
    let zero = string "0" *> return (Some 0) in
    let other = any_char *> return None in
    let forward =
      many
        (choice
           [ one; two; three; four; five; six; seven; eight; nine; zero; other ])
    in
    let backward =
      many
        (choice
           [
             one';
             two';
             three';
             four';
             five';
             six';
             seven';
             eight';
             nine';
             zero;
             other;
           ])
    in
    let first =
      parse_string ~consume:All forward line
      |> Result.get_ok |> List.filter_map Fun.id |> List.hd
    in
    let last =
      parse_string ~consume:All backward (reverse_string line)
      |> Result.get_ok |> List.filter_map Fun.id |> List.hd
    in
    Format.sprintf "%d%d" first last |> int_of_string
  in
  input |> List.fold_left (fun acc line -> acc + aux line) 0

let input = Util.get_input_lines ()

let%expect_test "part1" =
  let example =
    {|1abc2
pqr3stu8vwx
a1b2c3d4e5f
treb7uchet|} |> String.split_on_char '\n'
  in
  part1 example |> print_int;
  [%expect {| 142 |}];
  part1 input |> print_int;
  [%expect {| 54708 |}]

let%expect_test "part2" =
  let example =
    {|two1nine
eightwothree
abcone2threexyz
xtwone3four
4nineeightseven2
zoneight234
7pqrstsixteen|}
    |> String.split_on_char '\n'
  in
  part2 example |> print_int;
  [%expect {| 281 |}];
  part2 input |> print_int;
  [%expect {| 54087 |}]
