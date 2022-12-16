module Vmap = Map.Make (String)
module Vset = Set.Make (String)

module State = struct
  type t = { me : string; open_valves : Vset.t }

  let compare = compare
end

type valve = { flow_rate : int; tunnels : string list }

module Parser = struct
  open Angstrom

  let letter = satisfy (function 'A' .. 'Z' -> true | _ -> false)
  let valve = map2 letter letter ~f:(fun c1 c2 -> Format.sprintf "%c%c" c1 c2)
  let is_digit = function '0' .. '9' -> true | _ -> false
  let flow_rate = take_while1 is_digit >>| int_of_string

  let line =
    string "Valve " *> valve >>= fun valve_id ->
    string " has flow rate=" *> flow_rate >>= fun flow_rate ->
    (string "; tunnels lead to valves " <|> string "; tunnel leads to valve ")
    *> sep_by (string ", ") valve
    <* end_of_line
    >>| fun tunnels -> (valve_id, { flow_rate; tunnels })

  let parse_all = many line >>| fun lines -> Vmap.of_seq (List.to_seq lines)
  let parse input = parse_string ~consume:All parse_all input |> Result.get_ok
end

module State_map = Map.Make (State)
module Vset_map = Map.Make (Vset)

let max_flow acc = function
  | Some flow when flow > acc -> Some flow
  | _ -> Some acc

let release_pressure ~time valves =
  let open State in
  let flow_sum open_valves =
    Vmap.fold
      (fun valve_id { flow_rate; _ } flow ->
        match Vset.mem valve_id open_valves with
        | true -> flow + flow_rate
        | false -> flow)
      valves 0
  in
  let next_states { me; open_valves } =
    let { flow_rate; tunnels } = Vmap.find me valves in
    let neighbors = List.map (fun dst -> { me = dst; open_valves }) tunnels in
    if flow_rate > 0 then
      { me; open_valves = Vset.add me open_valves } :: neighbors
    else neighbors
  in
  let rec aux minute states =
    if minute = time then states
    else
      List.fold_left
        (fun queue (state, flow) ->
          let flow' = flow_sum state.open_valves in
          let new_states =
            List.map (fun state -> (flow + flow', state)) (next_states state)
          in
          List.fold_left
            (fun queue (acc, state) ->
              State_map.update state (max_flow acc) queue)
            queue new_states)
        State_map.empty
        (State_map.bindings states)
      |> aux (minute + 1)
  in
  aux 0 (State_map.singleton { me = "AA"; open_valves = Vset.empty } 0)

let part1 valves =
  let states = release_pressure ~time:30 valves in
  State_map.fold (fun _ flow acc -> max flow acc) states 0

let part2 valves =
  let states = release_pressure ~time:26 valves in
  (* We assume elephant and I walked separately, opening disjoint sets of valves *)
  let best_for_valveset =
    State_map.fold
      (fun state acc queue ->
        Vset_map.update state.open_valves (max_flow acc) queue)
      states Vset_map.empty
  in
  Vset_map.fold
    (fun valves1 best1 acc ->
      Vset_map.fold
        (fun valves2 best2 acc ->
          if best1 + best2 > acc && Vset.disjoint valves1 valves2 then
            best1 + best2
          else acc)
        best_for_valveset acc)
    best_for_valveset 0

let example =
  {|Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II
|}

let%expect_test "part1" =
  print_int (Parser.parse example |> part1);
  [%expect {| 1651 |}];
  print_int (Parser.parse (Util.get_input_all ()) |> part1);
  [%expect {| 2320 |}]

let%expect_test "part2" =
  print_int (Parser.parse example |> part2);
  [%expect {| 1707 |}];
  print_int (Parser.parse (Util.get_input_all ()) |> part2);
  [%expect {| 2967 |}]
