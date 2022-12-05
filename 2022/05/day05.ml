module Stacks = Map.Make (Char)

let stacks_of_ascii_art lines =
  let open List in
  map Util.explode lines |> Util.zip |> map rev
  |> filter (function
       | hd :: _ when hd >= '1' && hd <= '9' -> true
       | _ -> false)
  |> map (function
       | hd :: tl -> (hd, rev tl |> filter (fun c -> c <> ' '))
       | _ -> assert false)
  |> fold_left
       (fun stacks (index, stack) -> Stacks.add index stack stacks)
       Stacks.empty

let top_of_stacks stacks =
  Stacks.fold (fun _index stack acc -> List.hd stack :: acc) stacks []
  |> List.rev |> List.to_seq |> String.of_seq

let stacks_and_instructions seq =
  let get_stacks seq =
    let rec take_while_nonempty seq acc =
      match seq () with
      | Seq.Cons (line, seq') when line <> "" ->
          take_while_nonempty seq' (line :: acc)
      | Seq.Cons (_, seq') -> (acc, seq')
      | _ -> assert false
    in
    take_while_nonempty seq []
  in
  let stacks, instructions = get_stacks seq in
  let stacks = stacks_of_ascii_art (List.rev stacks) in
  let instructions = Seq.filter (fun line -> line <> "") instructions in
  (stacks, instructions)

let parse_instruction instruction =
  Scanf.sscanf instruction "move %i from %c to %c"
    (fun how_many from_index to_index -> (how_many, from_index, to_index))

let fold_instructions ?(crane = `CrateMover_9000) stacks seq =
  Seq.fold_left
    (fun stacks instruction ->
      match parse_instruction instruction with
      | how_many, from_index, to_index ->
          let old_from = Stacks.find from_index stacks in
          let crates_to_move, new_from =
            let rec first_n n stack =
              match (n, stack) with
              | 0, stack -> ([], stack)
              | n, hd :: tl ->
                  let crates, rest = first_n (n - 1) tl in
                  (hd :: crates, rest)
              | _ -> failwith "Stack empty"
            in
            first_n how_many old_from
          in
          let old_to = Stacks.find to_index stacks in
          let new_to =
            List.append
              (match crane with
              | `CrateMover_9000 -> List.rev crates_to_move
              | `CrateMover_9001 -> crates_to_move)
              old_to
          in
          Stacks.add from_index new_from stacks |> Stacks.add to_index new_to)
    stacks seq

let example =
  {|    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2
|}
  |> String.split_on_char '\n' |> List.to_seq

let%expect_test "part1" =
  let stacks, instructions = stacks_and_instructions example in
  let stacks = fold_instructions stacks instructions in
  print_endline (top_of_stacks stacks);
  [%expect {| CMZ |}];
  let stacks, instructions =
    stacks_and_instructions Util.(get_input () |> seq_of_channel)
  in
  let stacks = fold_instructions stacks instructions in
  print_endline (top_of_stacks stacks);
  [%expect {| RTGWZTHLD |}]

let%expect_test "part2" =
  let stacks, instructions = stacks_and_instructions example in
  let stacks = fold_instructions ~crane:`CrateMover_9001 stacks instructions in
  print_endline (top_of_stacks stacks);
  [%expect {| MCD |}];
  let stacks, instructions =
    stacks_and_instructions Util.(get_input () |> seq_of_channel)
  in
  let stacks = fold_instructions ~crane:`CrateMover_9001 stacks instructions in
  print_endline (top_of_stacks stacks);
  [%expect {| STHGRZZFR |}]