module Char_set = Set.Make (Char)

let find_marker ?(length = 4) str =
  let all_distinct str =
    let set =
      String.fold_left
        (fun set item -> Char_set.add item set)
        Char_set.empty str
    in
    Char_set.cardinal set = String.length str
  in
  let rec go offset =
    if all_distinct (String.sub str offset length) then offset + length
    else go (offset + 1)
  in
  go 0

let examples =
  [
    "mjqjpqmgbljsphdztnvjfqwrcgsmlb";
    "bvwbjplbgvbhsrlpgdmjqwftvncz";
    "nppdvjthqldpwncqszvftbrmjlhg";
    "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg";
    "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw";
  ]

let%expect_test "part1" =
  print_endline
    (Util.show_int_list
       (List.map (fun example -> find_marker example) examples));
  [%expect {|
    [7; 5; 6; 10; 11] |}];
  print_int (Util.get_input () |> In_channel.input_all |> find_marker);
  [%expect {| 1361 |}]

let%expect_test "part2" =
  print_endline
    (Util.show_int_list
       (List.map (fun example -> find_marker ~length:14 example) examples));
  [%expect {|
    [19; 23; 23; 29; 26] |}];
  print_int (Util.get_input () |> In_channel.input_all |> find_marker ~length:14);
  [%expect {| 3263 |}]
