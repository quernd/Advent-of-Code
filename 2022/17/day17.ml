module Coordinate = struct
  type t = int * int

  let compare = compare

  (* Using y/x coordinates to make some things a bit easier down the road *)
  let ( + ) (y1, x1) (y2, x2) = (y1 + y2, x1 + x2)
end

module Cset = Set.Make (Coordinate)

module Rock = struct
  type t = Coordinate.t List.t

  let move (y, x) = List.map (Coordinate.( + ) (y, x))
end

let rocks =
  Array.of_list
    [
      (* #### *)
      [ (3, 2); (3, 3); (3, 4); (3, 5) ];
      (* .#.
         ###
         .#. *)
      [ (3, 3); (4, 2); (4, 3); (4, 4); (5, 3) ];
      (* ..#
         ..#
         ### *)
      [ (3, 2); (3, 3); (3, 4); (4, 4); (5, 4) ];
      (* #
         #
         #
         # *)
      [ (3, 2); (4, 2); (5, 2); (6, 2) ];
      (* ##
         ## *)
      [ (3, 2); (3, 3); (4, 2); (4, 3) ];
    ]

let simulate ?(max_rocks = 2022) jets =
  let impossible rock chamber =
    List.exists
      (fun (y, x) -> x < 0 || x >= 7 || y < 0 || Cset.mem (y, x) chamber)
      rock
  in
  let jet_move jet rock chamber =
    let rock' =
      match jet with
      | `Left -> Rock.move (0, -1) rock
      | `Right -> Rock.move (0, 1) rock
    in
    if impossible rock' chamber then rock else rock'
  in
  let jets = Array.of_list jets in
  let num_jets = Array.length jets in
  let num_rocks = Array.length rocks in
  let hashtbl = Hashtbl.create 100 in
  let rec simulate_rocks ~rocks_counter ~jets_counter ~height ~chamber =
    let rocks_counter, chamber =
      if jets_counter >= num_jets * num_rocks then (
        let hash = (rocks_counter mod num_rocks, jets_counter mod num_jets) in
        match Hashtbl.find_opt hashtbl hash with
        | Some (height', rocks_counter') ->
            let cycle_length = rocks_counter - rocks_counter' in
            let cycle_height = height - height' in
            let cycles = (max_rocks - rocks_counter) / cycle_length in
            Hashtbl.clear hashtbl;
            ( rocks_counter + (cycles * cycle_length),
              Cset.map (Coordinate.( + ) (cycles * cycle_height, 0)) chamber )
        | None ->
            Hashtbl.add hashtbl hash (height, rocks_counter);
            (rocks_counter, chamber))
      else (rocks_counter, chamber)
    in
    if rocks_counter >= max_rocks then chamber
    else
      let rec simulate_steps ~jets_counter ~rock ~chamber =
        let jets_counter, rock =
          ( jets_counter + 1,
            jet_move jets.(jets_counter mod num_jets) rock chamber )
        in
        let rock' = Rock.move (-1, 0) rock in
        if impossible rock' chamber then (jets_counter, rock)
        else simulate_steps ~jets_counter ~rock:rock' ~chamber
      in
      let rock = rocks.(rocks_counter mod num_rocks) in
      let height =
        match Cset.max_elt_opt chamber with Some (y, _) -> y + 1 | None -> 0
      in
      let rock = Rock.move (height, 0) rock in
      let jets_counter, rock = simulate_steps ~jets_counter ~rock ~chamber in
      let chamber =
        List.fold_left
          (fun chamber coord -> Cset.add coord chamber)
          chamber rock
      in
      simulate_rocks ~rocks_counter:(rocks_counter + 1) ~jets_counter ~height
        ~chamber
  in
  match
    simulate_rocks ~rocks_counter:0 ~jets_counter:0 ~height:0
      ~chamber:Cset.empty
    |> Cset.max_elt_opt
  with
  | Some (y, _) -> y + 1
  | None -> failwith "Chamber is still empty!"

let parse input =
  Util.explode input
  |> List.map (function
       | '<' -> `Left
       | '>' -> `Right
       | c -> failwith (Format.sprintf "Not a valid jet direction: %c!" c))

let example = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

let%expect_test "part1" =
  print_int (simulate (parse example));
  [%expect {| 3068 |}];
  print_int (simulate (Util.get_input_all () |> parse));
  [%expect {| 3197 |}]

let%expect_test "part2" =
  print_int (simulate ~max_rocks:1_000_000_000_000 (parse example));
  [%expect {| 1514285714288 |}];
  print_int
    (simulate ~max_rocks:1_000_000_000_000 (Util.get_input_all () |> parse));
  [%expect {| 1568513119571 |}]
