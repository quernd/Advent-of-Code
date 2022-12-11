type operand = Old | Int of int [@@deriving show]

type operation = Mul of (operand * operand) | Add of (operand * operand)
[@@deriving show]

module Monkey = struct
  type t = {
    inspect_count : int;
    items : int list;
    operation : operation;
    test : int;
    if_true : int;
    if_false : int;
  }
  [@@deriving show]

  let inspect operation worry_level =
    let value = function Old -> worry_level | Int i -> i in
    match operation with
    | Mul (a, b) -> value a * value b
    | Add (a, b) -> value a + value b

  let turn ~manage_worry monkey =
    let { items; operation; test; if_true; if_false; inspect_count } = monkey in
    let inspect_count = inspect_count + List.length items in
    let throws =
      List.map
        (fun worry_level ->
          let worry_level' = manage_worry (inspect operation worry_level) in
          let throw_to_monkey =
            if worry_level' mod test = 0 then if_true else if_false
          in
          (throw_to_monkey, worry_level'))
        items
    in
    (throws, { monkey with items = []; inspect_count })
end

type monkey_list = (int * Monkey.t) list [@@deriving show]

module Parser = struct
  open Angstrom

  let number = take_while (function '0' .. '9' -> true | _ -> false)
  let space = take_while (( = ) ' ')
  let monkey_number = string "Monkey " *> number <* string ":" >>| int_of_string

  let starting_items =
    space *> string "Starting items: " *> sep_by1 (string ", ") number
    >>| List.map int_of_string

  let operand =
    string "old" *> return Old
    <|> (number >>| fun number -> Int (int_of_string number))

  let operator =
    let mul = string " * " *> return (fun op1 op2 -> Mul (op1, op2)) in
    let add = string " + " *> return (fun op1 op2 -> Add (op1, op2)) in
    mul <|> add

  let operation =
    space *> string "Operation: new = " *> operand >>= fun op1 ->
    operator >>= fun operator ->
    operand >>| fun op2 -> operator op1 op2

  let test = space *> string "Test: divisible by " *> number >>| int_of_string

  let if_true =
    space *> string "If true: throw to monkey " *> number >>| int_of_string

  let if_false =
    space *> string "If false: throw to monkey " *> number >>| int_of_string

  let monkey =
    monkey_number <* end_of_line >>= fun number ->
    starting_items <* end_of_line >>= fun items ->
    operation <* end_of_line >>= fun operation ->
    test <* end_of_line >>= fun test ->
    if_true <* end_of_line >>= fun if_true ->
    if_false <* end_of_line >>= fun if_false ->
    return
      ( number,
        Monkey.{ items; operation; test; if_true; if_false; inspect_count = 0 }
      )

  let parse input =
    parse_string ~consume:All (sep_by1 end_of_line monkey) input
    |> Result.get_ok
end

module Int_map = Map.Make (Int)

let monkey_business ?(rounds = 20)
    ?(manage_worry = fun worry_level -> worry_level / 3) monkeys =
  let round monkeys =
    let numbers = Int_map.bindings monkeys |> List.map fst in
    List.fold_left
      (fun monkeys number ->
        let monkey = Int_map.find number monkeys in
        let throw_to, monkey = Monkey.turn ~manage_worry monkey in
        let monkeys =
          List.fold_left
            (fun monkeys (to_monkey, item) ->
              Int_map.update to_monkey
                (function
                  | Some monkey ->
                      Some Monkey.{ monkey with items = item :: monkey.items }
                  | _ -> assert false)
                monkeys)
            (Int_map.add number monkey monkeys)
            throw_to
        in
        monkeys)
      monkeys numbers
  in
  let rec go n monkeys =
    match n with 0 -> monkeys | n -> go (n - 1) (round monkeys)
  in
  let inspect_counts =
    let monkeys = Int_map.of_seq (List.to_seq monkeys) in
    Int_map.bindings (go rounds monkeys)
    |> List.map (fun (_, monkey) -> monkey.Monkey.inspect_count)
  in
  match List.sort (fun a b -> -compare a b) inspect_counts with
  | fst :: snd :: _ -> fst * snd
  | _ -> assert false

let example =
  {|Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1
|}

let%expect_test "part1" =
  let monkeys = Parser.parse example in
  print_int (monkey_business monkeys);
  [%expect {| 10605 |}];
  let monkeys = Parser.parse (Util.get_input () |> In_channel.input_all) in
  print_int (monkey_business monkeys);
  [%expect {| 110220 |}]

let%expect_test "part2" =
  let common_multiple =
    List.fold_left (fun acc (_, monkey) -> acc * monkey.Monkey.test) 1
  in
  let monkeys = Parser.parse example in
  print_int
    (monkey_business ~rounds:10_000
       ~manage_worry:(fun worry_level ->
         worry_level mod common_multiple monkeys)
       monkeys);
  [%expect {| 2713310158 |}];
  let monkeys = Parser.parse (Util.get_input () |> In_channel.input_all) in
  print_int
    (monkey_business ~rounds:10_000
       ~manage_worry:(fun worry_level ->
         worry_level mod common_multiple monkeys)
       monkeys);
  [%expect {| 19457438264 |}]
