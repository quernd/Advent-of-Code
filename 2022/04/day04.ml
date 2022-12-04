module Assignment = struct
  type t = { first : int; last : int }

  let scan buf = Scanf.bscanf buf "%i-%i" (fun first last -> { first; last })
  let contains s1 s2 = s1.first <= s2.first && s1.last >= s2.last
  let overlap s1 s2 = s1.first <= s2.last && s2.first <= s1.last
end

let solve ~criterion assignments =
  let assignments_of_string s =
    Scanf.sscanf s "%r,%r" Assignment.scan Assignment.scan (fun s1 s2 ->
        (s1, s2))
  in
  let open List in
  map assignments_of_string assignments |> filter criterion |> length

let part1 =
  solve ~criterion:(fun (a1, a2) ->
      Assignment.contains a1 a2 || Assignment.contains a2 a1)

let part2 = solve ~criterion:(fun (a1, a2) -> Assignment.overlap a1 a2)

let example =
  {|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|}
  |> String.split_on_char '\n'

let%expect_test "part1" =
  print_int (part1 example);
  [%expect {| 2 |}];
  print_int (Util.(get_input () |> lines_of_channel) |> part1);
  [%expect {| 571 |}]

let%expect_test "part2" =
  print_int (part2 example);
  [%expect {| 4 |}];
  print_int (Util.(get_input () |> lines_of_channel) |> part2);
  [%expect {| 917 |}]
