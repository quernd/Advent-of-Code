type instruction = Noop | Addx of int

module Parser = struct
  open Angstrom

  let noop = string "noop" *> return Noop

  let addx =
    string "addx "
    *> take_while1 (function '0' .. '9' | '-' -> true | _ -> false)
    >>| fun increment -> Addx (int_of_string increment)

  let instruction = noop <|> addx <* char '\n'

  let parse input =
    parse_string ~consume:All (many instruction) input |> Result.get_ok
end

let run instructions =
  List.fold_left
    (fun registers instruction ->
      match (registers, instruction) with
      | hd :: tl, Noop -> hd :: hd :: tl
      | hd :: tl, Addx increment -> (hd + increment) :: hd :: hd :: tl
      | _ -> assert false)
    [ 1 ] instructions
  |> List.tl (* discard the last cycle because it's after, not during *)
  |> List.rev

let part1 instructions =
  let registers = run instructions in
  let _cycles, sum =
    List.fold_left
      (fun (cycle, sum) register ->
        ( cycle + 1,
          if (cycle - 20) mod 40 = 0 then sum + (cycle * register) else sum ))
      (1, 0) registers
  in
  sum

let part2 ?(width = 40) ?(height = 6) instructions =
  let buffer = Buffer.create (height * width) in
  let registers = run instructions in
  let () =
    List.iteri
      (fun index register ->
        let position = index mod width in
        let visible = abs (position - register) <= 1 in
        let () = Buffer.add_char buffer (if visible then '#' else '.') in
        if position = width - 1 then Buffer.add_char buffer '\n')
      registers
  in
  Buffer.contents buffer

let example =
  {|addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop
|}

let%expect_test "part1" =
  print_int (part1 (Parser.parse example));
  [%expect {| 13140 |}];
  print_int (Util.get_input () |> In_channel.input_all |> Parser.parse |> part1);
  [%expect {| 12740 |}]

let%expect_test "part2" =
  print_endline (part2 (Parser.parse example));
  [%expect
    {|
    ##..##..##..##..##..##..##..##..##..##..
    ###...###...###...###...###...###...###.
    ####....####....####....####....####....
    #####.....#####.....#####.....#####.....
    ######......######......######......####
    #######.......#######.......#######..... |}];
  print_endline
    (Util.get_input () |> In_channel.input_all |> Parser.parse |> part2);
  [%expect
    {|
    ###..###..###...##..###...##...##..####.
    #..#.#..#.#..#.#..#.#..#.#..#.#..#.#....
    #..#.###..#..#.#..#.#..#.#..#.#....###..
    ###..#..#.###..####.###..####.#.##.#....
    #.#..#..#.#....#..#.#.#..#..#.#..#.#....
    #..#.###..#....#..#.#..#.#..#..###.#.... |}]
