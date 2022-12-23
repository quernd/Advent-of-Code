module Coordinate = struct
  type t = int * int

  let north (x, y) = ([ (x - 1, y - 1); (x, y - 1); (x + 1, y - 1) ], (x, y - 1))
  let south (x, y) = ([ (x - 1, y + 1); (x, y + 1); (x + 1, y + 1) ], (x, y + 1))
  let west (x, y) = ([ (x - 1, y - 1); (x - 1, y); (x - 1, y + 1) ], (x - 1, y))
  let east (x, y) = ([ (x + 1, y - 1); (x + 1, y); (x + 1, y + 1) ], (x + 1, y))
  let compare = compare
  let pp ppf (a, b) = Format.fprintf ppf "(%i,%i)" a b
end

module Cmap = Map.Make (Coordinate)
module Cset = Set.Make (Coordinate)

let parse input =
  let enumerate list = List.mapi (fun i line -> (i, line)) list in
  List.fold_left
    (fun board (row, line) ->
      String.fold_left
        (fun (col, board) tile ->
          (col + 1, if tile = '#' then Cset.add (col, row) board else board))
        (0, board) line
      |> snd)
    Cset.empty (enumerate input)

type move = Zero | One of Coordinate.t | Too_many

let bounds set =
  Cset.fold
    (fun (x, y) ((min_x, min_y), (max_x, max_y)) ->
      ((min min_x x, min min_y y), (max max_x x, max max_y y)))
    set
    ((max_int, max_int), (min_int, min_int))

let print_elfs elfs' =
  let (min_x, min_y), (max_x, max_y) = bounds elfs' in
  for i = min_y to max_y do
    for ii = min_x to max_x do
      if Cset.mem (ii, i) elfs' then print_char '#' else print_char '.'
    done;
    print_endline ""
  done;
  print_endline ""

let part1 ?(rounds = 10) elfs =
  let rec step n elfs directions =
    if n = 0 then elfs
    else
      (* let _ = print_elfs elfs in *)
      (* first half *)
      let proposed =
        Cset.fold
          (fun elf acc ->
            if
              List.for_all
                (fun neighbor -> not (Cset.mem neighbor elfs))
                (List.concat
                   (List.map (fun direction -> direction elf |> fst) directions))
            then
              (* Format.printf "Elf %a chooses not to move\n" Coordinate.pp elf; *)
              acc
            else
              List.fold_left
                (fun (acc, has_proposed) direction ->
                  let neighbors, move_to = direction elf in

                  let propose_move =
                    if
                      (not has_proposed)
                      && List.for_all
                           (fun neighbor -> not (Cset.mem neighbor elfs))
                           neighbors
                    then Some move_to
                    else None
                  in
                  match (propose_move, Cmap.find_opt move_to acc) with
                  | Some move_to, None | Some move_to, Some Zero ->
                      (* Format.printf "Elf %a proposes move to %a\n" Coordinate.pp
                         elf Coordinate.pp move_to; *)
                      (Cmap.add move_to (One elf) acc, true)
                  | Some move_to, Some (One _) ->
                      (* Format.printf "Elf %a also proposes move to %a\n"
                         Coordinate.pp elf Coordinate.pp move_to; *)
                      (Cmap.add move_to Too_many acc, true)
                  | _ -> (acc, has_proposed))
                (acc, false) directions
              |> fst)
          elfs Cmap.empty
      in
      (* second half *)
      let elfs' =
        Cmap.fold
          (fun move_to move_from acc ->
            match move_from with
            | One move_from ->
                (* Format.printf "Elf %a moves to %a\n" Coordinate.pp move_from
                   Coordinate.pp move_to; *)
                Cset.remove move_from acc |> Cset.add move_to
            | _ -> acc)
          proposed elfs
      in
      (* print_int @@ Cset.cardinal elfs'; *)
      let directions' = List.tl directions @ [ List.hd directions ] in
      step (n - 1) elfs' directions'
  in
  let elfs' = step rounds elfs Coordinate.[ north; south; west; east ] in

  (* let _ = print_elfs elfs' in *)
  let (min_x, min_y), (max_x, max_y) = bounds elfs' in
  let size = (max_x - min_x + 1) * (max_y - min_y + 1) in

  size - Cset.cardinal elfs'

let example =
  {|.....
..##.
..#..
.....
..##.
.....|} |> String.split_on_char '\n'

let example2 =
  {|....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..|}
  |> String.split_on_char '\n'

let input = Util.get_input_lines ()

let%expect_test "part1" =
  parse example2 |> part1 |> print_int;
  [%expect {| 110 |}];
  parse input |> part1 |> print_int
