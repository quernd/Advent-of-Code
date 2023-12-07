open Util

type hand = {
  typ :
    [ `Five | `Four | `Full_house | `Three | `Two_pair | `One_pair | `High ];
  cards : char list;
  bid : int;
  value : int list;
}
[@@deriving show]

let value_of_typ = function
  | `Five -> 7
  | `Four -> 6
  | `Full_house -> 5
  | `Three -> 4
  | `Two_pair -> 3
  | `One_pair -> 2
  | `High -> 1

let value_of_card = function
  | 'A' -> 14
  | 'K' -> 13
  | 'Q' -> 12
  | 'J' -> 11
  | 'T' -> 10
  | '2' .. '9' as c -> int_of_char c - 48
  | _ -> assert false

let hand ~bid cards =
  let list_of_cards hand =
    let rec aux acc = function
      | [] -> acc
      | hd :: tl ->
          let acc =
            match List.assoc_opt hd acc with
            | Some n -> (hd, n + 1) :: List.remove_assoc hd acc
            | None -> (hd, 1) :: acc
          in
          aux acc tl
    in
    aux [] hand
  in
  let has n list =
    List.exists (function _, x when x = n -> true | _ -> false) list
  in
  let typ =
    let list = list_of_cards cards in
    if has 5 list then `Five
    else if has 4 list then `Four
    else if has 3 list && has 2 list then `Full_house
    else if has 3 list then `Three
    else if List.filter (fun card -> snd card = 2) list |> List.length = 2 then
      `Two_pair
    else if has 2 list then `One_pair
    else `High
  in
  let value = value_of_typ typ :: List.map value_of_card cards in
  { typ; cards; bid; value }

let parse input =
  let open Angstrom in
  let number = take_while P.is_digit >>| int_of_string in
  let is_card = function
    | 'A' | 'K' | 'Q' | 'J' | 'T' | '2' .. '9' -> true
    | _ -> false
  in
  let hand =
    many1 (satisfy is_card) <* char ' ' >>= fun cards ->
    number >>| fun bid -> hand ~bid cards
  in
  let p = sep_by1 (char '\n') hand in
  parse_string ~consume:Prefix p input |> Result.get_ok

let part1 input =
  let sorted =
    parse input
    |> List.sort (fun hand1 hand2 -> compare hand1.value hand2.value)
  in
  List.fold_left
    (fun (i, acc) hand -> (i + 1, acc + (i * hand.bid)))
    (1, 0) sorted
  |> snd

let example = {|32T3K 765
T55J5 684
KK677 28
KTJJT 220
QQQJA 483
|}

let input = get_input_all ()

let%expect_test "part1" =
  part1 example |> print_int;
  [%expect {| 6440 |}];
  part1 input |> print_int;
  [%expect {| 248569531 |}]
