open Util
module Int_set = Set.Make (Int)
module Int_map = Map.Make (Int)

let parse_card input =
  let open Angstrom in
  let integer = take_while1 P.is_digit >>| int_of_string in
  let space = take_while1 (( = ) ' ') in
  let header = string "Card" *> space *> integer <* string ":" <* space in
  let separator = space *> string "|" <* space in
  let p =
    lift4
      (fun id winning _ have ->
        (id, Int_set.of_list winning, Int_set.of_list have))
      header (sep_by1 space integer) separator (sep_by1 space integer)
  in
  parse_string ~consume:All p input |> Result.get_ok

let part1 input =
  let worth (_, winning, have) =
    match Int_set.(cardinal (inter winning have)) with
    | 1 -> 1
    | n when n >= 2 -> 2 lsl (n - 2)
    | _ -> 0
  in
  let cards = List.map parse_card input in
  List.map worth cards |> sum

let part2 input =
  let cards = List.map parse_card input in
  let copies =
    List.fold_left
      (fun acc (id, winning, have) ->
        let acc =
          Int_map.update id
            (function Some num -> Some (num + 1) | None -> Some 1)
            acc
        in
        let n_won = Int_set.(cardinal (inter winning have)) in
        let won = List.init n_won (fun i -> i + 1 + id) in
        List.fold_left
          (fun acc n ->
            let x =
              match Int_map.find_opt id acc with Some x -> x | None -> 0
            in
            Int_map.update n
              (function Some num -> Some (num + x) | None -> Some x)
              acc)
          acc won)
      Int_map.empty cards
  in
  List.fold_left
    (fun acc (id, _, _) ->
      match Int_map.find_opt id copies with Some n -> acc + n | _ -> acc)
    0 cards

let example =
  {|Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11|}
  |> String.split_on_char '\n'

let input = get_input_lines ()

let%expect_test "part1" =
  part1 example |> print_int;
  [%expect {| 13 |}];
  part1 input |> print_int;
  [%expect {| 25183 |}];
  part2 example |> print_int;
  [%expect {| 30 |}];
  part2 input |> print_int;
  [%expect {| 5667240 |}]
