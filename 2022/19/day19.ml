type robot = Ore | Clay | Obsidian | Geode

module State = struct
  type t = { ore : int; clay : int; obsidian : int; geode : int }
  type inventory = t
  type robots = t

  let make ?(ore = 0) ?(clay = 0) ?(obsidian = 0) ?(geode = 0) () =
    { ore; clay; obsidian; geode }

  let add_robot { ore; clay; obsidian; geode } = function
    | Ore -> { ore = ore + 1; clay; obsidian; geode }
    | Clay -> { ore; clay = clay + 1; obsidian; geode }
    | Obsidian -> { ore; clay; obsidian = obsidian + 1; geode }
    | Geode -> { ore; clay; obsidian; geode = geode + 1 }

  let ( ++ ) inv1 inv2 =
    {
      ore = inv1.ore + inv2.ore;
      clay = inv1.clay + inv2.clay;
      obsidian = inv1.obsidian + inv2.obsidian;
      geode = inv1.geode + inv2.geode;
    }

  let ( -- ) inv1 inv2 =
    {
      ore = inv1.ore - inv2.ore;
      clay = inv1.clay - inv2.clay;
      obsidian = inv1.obsidian - inv2.obsidian;
      geode = inv1.geode - inv2.geode;
    }

  type state = { robots : robots; inventory : inventory }

  let initial =
    {
      robots = { ore = 1; clay = 0; obsidian = 0; geode = 0 };
      inventory = { ore = 0; clay = 0; obsidian = 0; geode = 0 };
    }

  let build_robot ~robot_costs inventory =
    match inventory -- robot_costs with
    | { ore; clay; obsidian; geode }
      when ore < 0 || clay < 0 || obsidian < 0 || geode < 0 ->
        None
    | inventory -> Some inventory
end

module Blueprint = struct
  type t = {
    ore_robot : State.t;
    clay_robot : State.t;
    obsidian_robot : State.t;
    geode_robot : State.t;
  }

  let cost_of_robot { ore_robot; clay_robot; obsidian_robot; geode_robot } =
    function
    | Ore -> ore_robot
    | Clay -> clay_robot
    | Obsidian -> obsidian_robot
    | Geode -> geode_robot

  let of_string line =
    Scanf.sscanf line
      "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d \
       ore. Each obsidian robot costs %d ore and %d clay. Each geode robot \
       costs %d ore and %d obsidian."
      (fun
        id
        ore_ore
        clay_ore
        obsidian_ore
        obsidian_clay
        geode_ore
        geode_obsidian
      ->
        ( id,
          {
            ore_robot = State.make ~ore:ore_ore ();
            clay_robot = State.make ~ore:clay_ore ();
            obsidian_robot = State.make ~ore:obsidian_ore ~clay:obsidian_clay ();
            geode_robot = State.make ~ore:geode_ore ~obsidian:geode_obsidian ();
          } ))
end

let geodes ?(max_time = 24) blueprint =
  let next_robot ~blueprint ~robot (time, state) =
    let robot_costs = Blueprint.cost_of_robot blueprint robot in
    let rec aux (time, state) =
      if time >= max_time then Some (time, state)
      else
        let try_robot = State.(build_robot ~robot_costs state.inventory) in
        match try_robot with
        | Some inventory ->
            Some
              ( time + 1,
                State.
                  {
                    inventory = inventory ++ state.robots;
                    robots = add_robot state.robots robot;
                  } )
        | None ->
            aux
              ( time + 1,
                {
                  state with
                  inventory = State.(state.inventory ++ state.robots);
                } )
    in
    aux (time, state)
  in
  let initial = (0, State.initial) in

  let rec aux acc =
    let open State in
    function
    | [] -> acc
    | (time, state) :: tl ->
        let new_states =
          if time >= max_time then []
          else
            List.filter_map
              (fun robot ->
                match next_robot ~blueprint ~robot (time, state) with
                | None -> None
                | Some (time, state) ->
                    if
                      let remaining_time = max_time - time in
                      state.inventory.geode
                      + ((remaining_time * state.robots.geode)
                        (* existing geode robots keep cracking *)
                        + (remaining_time * remaining_time / 2)
                          (* maximum one new geode robot per time unit *))
                      < acc (* if it's under the current maximum, discard *)
                    then None
                    else Some (time, state))
              [ Geode; Obsidian; Clay; Ore ]
        in
        aux (max state.inventory.geode acc) (new_states @ tl)
  in
  aux 0 [ initial ]

let part1 ~max_time ~blueprints =
  List.fold_left
    (fun acc (id, blueprint) -> acc + (id * geodes ~max_time blueprint))
    0 blueprints

let part2 ~max_time ~blueprints =
  match blueprints with
  | (_, first) :: (_, second) :: (_, third) :: _ ->
      geodes ~max_time first * geodes ~max_time second * geodes ~max_time third
  | _ -> failwith "Elephants ate too many blueprints!"

let example =
  [
    "Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. \
     Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore \
     and 7 obsidian.";
    "Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. \
     Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore \
     and 12 obsidian.";
  ]
  |> List.map Blueprint.of_string

let blueprints = Util.get_input_lines () |> List.map Blueprint.of_string

let%expect_test "part1" =
  print_int (part1 ~max_time:24 ~blueprints:example);
  [%expect {| 33 |}];
  print_int (part1 ~max_time:24 ~blueprints);
  [%expect {| 960 |}]

let%expect_test "part2" =
  print_int (geodes ~max_time:32 (List.nth example 0 |> snd));
  [%expect {| 56 |}];
  print_int (geodes ~max_time:32 (List.nth example 1 |> snd));
  [%expect {| 62 |}];
  print_int (part2 ~max_time:32 ~blueprints);
  [%expect {| 2040 |}]
