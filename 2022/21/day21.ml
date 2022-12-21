module Rational = struct
  type t = int * int

  let pp ppf (a, b) = Format.fprintf ppf "%i/%i" a b
  let compare (a, b) (c, d) = compare (a * d) (b * c)
  let of_int a = (a, 1)
  let to_int (a, b) = a / b

  let reduce (a, b) =
    let rec gcd n m = if n = 0 then m else gcd (m - (m / n * n)) n in
    let gcd = gcd (abs a) (abs b) in
    (a / gcd, b / gcd)

  let add (a, b) (c, d) = ((a * d) + (b * c), b * d) |> reduce
  let sub (a, b) (c, d) = ((a * d) - (b * c), b * d) |> reduce
  let mul (a, b) (c, d) = (a * c, b * d) |> reduce

  let div (a, b) (c, d) =
    if c = 0 then raise Division_by_zero else (a * d, b * c) |> reduce

  module Infix = struct
    let ( + ) = add
    let ( - ) = sub
    let ( * ) = mul
    let ( / ) = div
  end
end

module Expression = struct
  type op = Add | Sub | Mul | Div
  type t = Unary of Rational.t | Binary of (op * string * string)

  let eval op x y =
    let open Rational.Infix in
    match op with Add -> x + y | Sub -> x - y | Mul -> x * y | Div -> x / y
end

module Monkeys = struct
  include Map.Make (String)

  let of_list monkeys =
    List.fold_left (fun acc (name, expr) -> add name expr acc) empty monkeys
end

module Parser = struct
  open Angstrom
  open Expression

  let operator =
    let add = string " + " *> return (fun op1 op2 -> Binary (Add, op1, op2)) in
    let sub = string " - " *> return (fun op1 op2 -> Binary (Sub, op1, op2)) in
    let mul = string " * " *> return (fun op1 op2 -> Binary (Mul, op1, op2)) in
    let div = string " / " *> return (fun op1 op2 -> Binary (Div, op1, op2)) in
    add <|> sub <|> mul <|> div

  let is_digit = function '0' .. '9' -> true | _ -> false

  let number =
    take_while1 is_digit >>| fun num ->
    Unary (int_of_string num |> Rational.of_int)

  let name = take_while1 (function 'a' .. 'z' -> true | _ -> false)

  let expression =
    number
    <|> ( name >>= fun op1 ->
          operator >>= fun operator ->
          name >>| fun op2 -> operator op1 op2 )

  let monkey =
    name <* string ": " >>= fun name ->
    expression >>| fun expression -> (name, expression)

  let parse input =
    parse_string ~consume:All
      (many (monkey <* end_of_line) >>| Monkeys.of_list)
      input
    |> Result.get_ok
end

let yell ?(root = "root") monkeys =
  let rec aux monkey =
    let open Expression in
    match Monkeys.find monkey monkeys with
    | Unary num -> num
    | Binary (op, monkey1, monkey2) -> eval op (aux monkey1) (aux monkey2)
  in
  aux root

let part1 monkeys = yell ~root:"root" monkeys

let part2 monkeys =
  match Monkeys.find "root" monkeys with
  | Expression.Binary (_, left, right) -> (
      let rec aux ?(reverse = false) monkeys lower pivot upper =
        let monkeys' =
          Monkeys.add "humn" (Expression.Unary (Rational.of_int pivot)) monkeys
        in
        let yell1 = yell ~root:left monkeys' in
        let yell2 = yell ~root:right monkeys' in
        match (Rational.compare yell1 yell2, reverse) with
        | 0, _ -> Some pivot
        | -1, false | 1, true ->
            let pivot' = (lower + pivot) / 2 in
            if pivot' = pivot then None
            else aux ~reverse monkeys lower pivot' pivot
        | 1, false | -1, true ->
            let pivot' = (pivot + upper) / 2 in
            if pivot' = pivot then None
            else aux ~reverse monkeys pivot pivot' upper
        | _ -> assert false
      in
      let max_range = 1 lsl 48 in
      let lower, pivot, upper = (-max_range, 0, max_range) in
      match aux monkeys lower pivot upper with
      | Some pivot -> pivot
      | None -> (
          match aux ~reverse:true monkeys lower pivot upper with
          | Some pivot -> pivot
          | None -> failwith "No solution found!"))
  | _ -> assert false

let example =
  {|root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32
|}
  |> Parser.parse

let input = Util.get_input_all () |> Parser.parse

let%expect_test "part1" =
  print_int (part1 example |> Rational.to_int);
  [%expect {| 152 |}];
  print_int (part1 input |> Rational.to_int);
  [%expect {| 93813115694560 |}]

let%expect_test "part2" =
  print_int (part2 example);
  [%expect {| 301 |}];
  print_int (part2 input);
  [%expect {| 3910938071092 |}]
