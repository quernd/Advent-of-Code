let example =
  {|[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]
|}

module Packet = struct
  type data = Int of int | List of data list
  [@@deriving show { with_path = false }]

  type t = data list [@@deriving show { with_path = false }]

  let rec compare left right =
    match (left, right) with
    | Int left :: left_tl, Int right :: right_tl -> (
        match Int.compare left right with
        | -1 -> -1
        | 1 -> 1
        | 0 -> compare left_tl right_tl
        | _ -> assert false)
    | List left :: left_tl, List right :: right_tl -> (
        match compare left right with
        | -1 -> -1
        | 1 -> 1
        | 0 -> compare left_tl right_tl
        | _ -> assert false)
    | Int left :: left_tl, right -> compare (List [ Int left ] :: left_tl) right
    | left, Int right :: right_tl ->
        compare left (List [ Int right ] :: right_tl)
    | [], _hd :: _tl -> -1
    | _hd :: _tl, [] -> 1
    | [], [] -> 0
end

type pair = Packet.t * Packet.t [@@deriving show { with_path = false }]
type signal = pair list [@@deriving show { with_path = false }]

module Parser = struct
  open Angstrom

  let packet =
    let open Packet in
    let open_bracket = char '[' in
    let close_bracket = char ']' in
    let comma = char ',' in
    let is_digit = function '0' .. '9' -> true | _ -> false in
    let integer = take_while1 is_digit >>| int_of_string in
    let list =
      fix (fun list ->
          open_bracket
          *> sep_by comma
               (integer
               >>| (fun integer -> Int integer)
               <|> (list >>| fun list -> List list))
          <* close_bracket)
    in
    list

  let%expect_test "packet" =
    Format.printf "%a" Packet.pp
      (parse_string ~consume:All packet "[[1],[2,3,4]]" |> Result.get_ok);
    [%expect {| [(List [(Int 1)]); (List [(Int 2); (Int 3); (Int 4)])] |}]

  let pair =
    packet <* end_of_line >>= fun first ->
    packet <* end_of_line >>| fun second -> (first, second)

  let%expect_test "pair" =
    Format.printf "%a" pp_pair
      (parse_string ~consume:All pair "[[4,4],4,4]\n[[4,4],4,4,4]\n"
      |> Result.get_ok);
    [%expect
      {|
        ([(List [(Int 4); (Int 4)]); (Int 4); (Int 4)],
         [(List [(Int 4); (Int 4)]); (Int 4); (Int 4); (Int 4)]) |}]

  let signal = sep_by end_of_line pair

  let%expect_test "signal" =
    Format.printf "%a" pp_signal
      (parse_string ~consume:All signal example |> Result.get_ok);
    [%expect
      {|
        [([(Int 1); (Int 1); (Int 3); (Int 1); (Int 1)],
          [(Int 1); (Int 1); (Int 5); (Int 1); (Int 1)]);
          ([(List [(Int 1)]); (List [(Int 2); (Int 3); (Int 4)])],
           [(List [(Int 1)]); (Int 4)]);
          ([(Int 9)], [(List [(Int 8); (Int 7); (Int 6)])]);
          ([(List [(Int 4); (Int 4)]); (Int 4); (Int 4)],
           [(List [(Int 4); (Int 4)]); (Int 4); (Int 4); (Int 4)]);
          ([(Int 7); (Int 7); (Int 7); (Int 7)], [(Int 7); (Int 7); (Int 7)]);
          ([], [(Int 3)]); ([(List [(List [])])], [(List [])]);
          ([(Int 1);
             (List
                [(Int 2);
                  (List
                     [(Int 3); (List [(Int 4); (List [(Int 5); (Int 6); (Int 7)])])])
                  ]);
             (Int 8); (Int 9)],
           [(Int 1);
             (List
                [(Int 2);
                  (List
                     [(Int 3); (List [(Int 4); (List [(Int 5); (Int 6); (Int 0)])])])
                  ]);
             (Int 8); (Int 9)])
          ] |}]

  let parse_packet input =
    parse_string ~consume:All packet input |> Result.get_ok

  let parse_signal input =
    parse_string ~consume:All signal input |> Result.get_ok
end

let part1 input =
  Parser.parse_signal input
  |> List.mapi (fun index (left, right) ->
         (index + 1, Packet.compare left right))
  |> List.filter_map (function index, -1 -> Some index | _ -> None)
  |> List.fold_left ( + ) 0

let part2 input =
  let divider1 = Parser.parse_packet "[[2]]" in
  let divider2 = Parser.parse_packet "[[6]]" in
  let all_packets =
    String.split_on_char '\n' input
    |> List.filter (( <> ) "")
    |> List.map Parser.parse_packet
  in
  all_packets @ [ divider1; divider2 ]
  |> List.sort Packet.compare
  |> List.mapi (fun index item -> (index + 1, item))
  |> List.filter (function _, item -> item = divider1 || item = divider2)
  |> List.map fst |> List.fold_left ( * ) 1

let%expect_test "part1" =
  print_int (part1 example);
  [%expect {| 13 |}];
  print_int (Util.get_input () |> In_channel.input_all |> part1);
  [%expect {| 5938 |}]

let%expect_test "part2" =
  print_int (part2 example);
  [%expect {| 140 |}];
  print_int (Util.get_input () |> In_channel.input_all |> part2);
  [%expect {| 29025 |}]