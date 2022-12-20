module Doubly_linked_ring = struct
  type 'a node = {
    mutable prev : 'a node;
    content : 'a;
    mutable next : 'a node;
  }

  let rec move_forward node = function
    | 0 -> ()
    | n ->
        let next_node = node.next in
        node.next <- next_node.next;
        next_node.prev <- node.prev;
        node.prev.next <- next_node;
        node.prev <- next_node;
        next_node.next.prev <- node;
        next_node.next <- node;
        move_forward node (n - 1)

  let rec move_backward node = function
    | 0 -> ()
    | n ->
        let prev_node = node.prev in
        node.prev <- prev_node.prev;
        prev_node.next <- node.next;
        node.next.prev <- prev_node;
        node.next <- prev_node;
        prev_node.prev.next <- node;
        prev_node.prev <- node;
        move_backward node (n - 1)

  let of_list list =
    let rec aux = function
      | [] -> ([], None)
      | hd :: tl -> (
          let rec node = { prev = node; content = hd; next = node } in
          match aux tl with
          | [], _ -> ([ node ], Some node)
          | next_node :: tl', last ->
              next_node.prev <- node;
              node.next <- next_node;
              (node :: next_node :: tl', last))
    in
    match aux list with
    | (first :: _ as ring), Some last ->
        first.prev <- last;
        last.next <- first;
        ring
    | _ -> []
end

let mix ?(times = 1) list =
  let length = List.length list - 1 in
  let open Doubly_linked_ring in
  let ring = of_list list in
  for _ = 1 to times do
    List.iter
      (fun item ->
        if item.content > 0 then move_forward item (item.content mod length)
        else if item.content < 0 then
          move_backward item (-item.content mod length))
      ring
  done;
  let rec find_zero node =
    match node.content with 0 -> node | _ -> find_zero node.next
  in
  let rec after n node = if n = 0 then node else after (n - 1) node.next in
  let one = after 1_000 (find_zero (List.hd ring)) in
  let two = after 1_000 one in
  let three = after 1_000 two in
  one.content + two.content + three.content

let part2 list = mix ~times:10 (List.map (( * ) 811589153) list)

let example =
  {|1
2
-3
3
-2
0
4|} |> String.split_on_char '\n' |> List.map int_of_string

let input = Util.get_input_lines () |> List.map int_of_string

let%expect_test "part1" =
  print_int (mix example);
  [%expect {| 3 |}];
  print_int (mix input);
  [%expect {| 7395 |}]

let%expect_test "part2" =
  print_int (part2 example);
  [%expect {| 1623178306 |}];
  print_int (part2 input);
  [%expect {| 1640221678213 |}]