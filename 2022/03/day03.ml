module Char_set = Set.Make (Char)

let compartments_of_rucksack rucksack =
  let half_len = String.length rucksack / 2 in
  (String.sub rucksack 0 half_len, String.sub rucksack half_len half_len)

let priority_of_item item =
  match item with
  | 'a' .. 'z' -> Char.code item - 96
  | 'A' .. 'Z' -> Char.code item - 64 + 26
  | _ -> failwith (Format.sprintf "Character %c is not a letter" item)

let char_set_of_string =
  String.fold_left (fun set item -> Char_set.add item set) Char_set.empty

let priority_of_rucksack rucksack =
  let compartment1, compartment2 = compartments_of_rucksack rucksack in
  let common =
    Char_set.inter
      (char_set_of_string compartment1)
      (char_set_of_string compartment2)
  in
  priority_of_item (Char_set.choose common)

let group_into_threes rucksacks =
  List.fold_left
    (fun groups rucksack ->
      match groups with
      | [] :: tl -> [ rucksack ] :: tl
      | [ hd ] :: tl -> [ hd; rucksack ] :: tl
      | [ hd; hd' ] :: tl -> [ hd; hd'; rucksack ] :: tl
      | group :: tl -> [ rucksack ] :: group :: tl
      | [] -> assert false)
    [ [] ] rucksacks

let badge rucksacks =
  let sets = List.map char_set_of_string rucksacks in
  match sets with
  | [ elf1; elf2; elf3 ] -> Char_set.inter elf1 (Char_set.inter elf2 elf3)
  | _ -> failwith "Not a group of three elves"

let part1 rucksacks =
  let open List in
  map priority_of_rucksack rucksacks |> fold_left ( + ) 0

let part2 rucksacks =
  let groups = group_into_threes rucksacks in
  let open List in
  map badge groups |> map Char_set.choose |> map priority_of_item
  |> fold_left ( + ) 0

let example =
  {|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw|}
  |> String.split_on_char '\n'

let%expect_test "part1" =
  print_int (part1 example);
  [%expect {| 157 |}];
  print_int (part1 Util.(get_input () |> lines_of_channel));
  [%expect {| 7742 |}]

let%expect_test "part2" =
  print_int (part2 example);
  [%expect {| 70 |}];
  print_int (part2 Util.(get_input () |> lines_of_channel));
  [%expect {| 2276 |}]