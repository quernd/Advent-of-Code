let get_input () = In_channel.open_text "input"

let rec fold_channel f acc channel =
  match In_channel.input_line channel with
  | Some line -> fold_channel f (f acc line) channel
  | None ->
      In_channel.close channel;
      acc

let rec seq_of_channel channel () =
  match In_channel.input_line channel with
  | Some line -> Seq.Cons (line, seq_of_channel channel)
  | None ->
      In_channel.close channel;
      Nil

(* let lines_of_channel channel =
   fold_channel (fun lines line -> line :: lines) [] channel *)

let lines_of_channel channel = List.of_seq (seq_of_channel channel)
let get_input_lines () = get_input () |> lines_of_channel

let insert item list =
  let rec insert item = function
    | [] -> []
    | hd :: tl ->
        (item :: hd :: tl) :: List.map (fun l -> hd :: l) (insert item tl)
  in
  insert item list @ [ list @ [ item ] ]

let perm list =
  let rec loop = function
    | [] -> [ [] ]
    | hd :: tl -> List.concat_map (fun l -> insert hd l) (loop tl)
  in
  loop list

type int_list = int list [@@deriving show]
type int_list_list = int list list [@@deriving show]

let%expect_test "perm" =
  print_endline (show_int_list_list (insert 0 [ 1; 2; 3; 4 ]));
  [%expect
    {|
    [[0; 1; 2; 3; 4]; [1; 0; 2; 3; 4]; [1; 2; 0; 3; 4]; [1; 2; 3; 0; 4];
      [1; 2; 3; 4; 0]] |}];
  List.length (perm [ 0; 1; 2; 3; 4 ]) |> print_int;
  [%expect {| 120 |}];
  print_endline (show_int_list_list (perm [ 0; 1; 2; 3; 4 ]));
  [%expect
    {|
    [[0; 1; 2; 3; 4]; [1; 0; 2; 3; 4]; [1; 2; 0; 3; 4]; [1; 2; 3; 0; 4];
      [1; 2; 3; 4; 0]; [0; 2; 1; 3; 4]; [2; 0; 1; 3; 4]; [2; 1; 0; 3; 4];
      [2; 1; 3; 0; 4]; [2; 1; 3; 4; 0]; [0; 2; 3; 1; 4]; [2; 0; 3; 1; 4];
      [2; 3; 0; 1; 4]; [2; 3; 1; 0; 4]; [2; 3; 1; 4; 0]; [0; 2; 3; 4; 1];
      [2; 0; 3; 4; 1]; [2; 3; 0; 4; 1]; [2; 3; 4; 0; 1]; [2; 3; 4; 1; 0];
      [0; 1; 3; 2; 4]; [1; 0; 3; 2; 4]; [1; 3; 0; 2; 4]; [1; 3; 2; 0; 4];
      [1; 3; 2; 4; 0]; [0; 3; 1; 2; 4]; [3; 0; 1; 2; 4]; [3; 1; 0; 2; 4];
      [3; 1; 2; 0; 4]; [3; 1; 2; 4; 0]; [0; 3; 2; 1; 4]; [3; 0; 2; 1; 4];
      [3; 2; 0; 1; 4]; [3; 2; 1; 0; 4]; [3; 2; 1; 4; 0]; [0; 3; 2; 4; 1];
      [3; 0; 2; 4; 1]; [3; 2; 0; 4; 1]; [3; 2; 4; 0; 1]; [3; 2; 4; 1; 0];
      [0; 1; 3; 4; 2]; [1; 0; 3; 4; 2]; [1; 3; 0; 4; 2]; [1; 3; 4; 0; 2];
      [1; 3; 4; 2; 0]; [0; 3; 1; 4; 2]; [3; 0; 1; 4; 2]; [3; 1; 0; 4; 2];
      [3; 1; 4; 0; 2]; [3; 1; 4; 2; 0]; [0; 3; 4; 1; 2]; [3; 0; 4; 1; 2];
      [3; 4; 0; 1; 2]; [3; 4; 1; 0; 2]; [3; 4; 1; 2; 0]; [0; 3; 4; 2; 1];
      [3; 0; 4; 2; 1]; [3; 4; 0; 2; 1]; [3; 4; 2; 0; 1]; [3; 4; 2; 1; 0];
      [0; 1; 2; 4; 3]; [1; 0; 2; 4; 3]; [1; 2; 0; 4; 3]; [1; 2; 4; 0; 3];
      [1; 2; 4; 3; 0]; [0; 2; 1; 4; 3]; [2; 0; 1; 4; 3]; [2; 1; 0; 4; 3];
      [2; 1; 4; 0; 3]; [2; 1; 4; 3; 0]; [0; 2; 4; 1; 3]; [2; 0; 4; 1; 3];
      [2; 4; 0; 1; 3]; [2; 4; 1; 0; 3]; [2; 4; 1; 3; 0]; [0; 2; 4; 3; 1];
      [2; 0; 4; 3; 1]; [2; 4; 0; 3; 1]; [2; 4; 3; 0; 1]; [2; 4; 3; 1; 0];
      [0; 1; 4; 2; 3]; [1; 0; 4; 2; 3]; [1; 4; 0; 2; 3]; [1; 4; 2; 0; 3];
      [1; 4; 2; 3; 0]; [0; 4; 1; 2; 3]; [4; 0; 1; 2; 3]; [4; 1; 0; 2; 3];
      [4; 1; 2; 0; 3]; [4; 1; 2; 3; 0]; [0; 4; 2; 1; 3]; [4; 0; 2; 1; 3];
      [4; 2; 0; 1; 3]; [4; 2; 1; 0; 3]; [4; 2; 1; 3; 0]; [0; 4; 2; 3; 1];
      [4; 0; 2; 3; 1]; [4; 2; 0; 3; 1]; [4; 2; 3; 0; 1]; [4; 2; 3; 1; 0];
      [0; 1; 4; 3; 2]; [1; 0; 4; 3; 2]; [1; 4; 0; 3; 2]; [1; 4; 3; 0; 2];
      [1; 4; 3; 2; 0]; [0; 4; 1; 3; 2]; [4; 0; 1; 3; 2]; [4; 1; 0; 3; 2];
      [4; 1; 3; 0; 2]; [4; 1; 3; 2; 0]; [0; 4; 3; 1; 2]; [4; 0; 3; 1; 2];
      [4; 3; 0; 1; 2]; [4; 3; 1; 0; 2]; [4; 3; 1; 2; 0]; [0; 4; 3; 2; 1];
      [4; 0; 3; 2; 1]; [4; 3; 0; 2; 1]; [4; 3; 2; 0; 1]; [4; 3; 2; 1; 0]] |}]

let rec zip lol =
  let heads_and_tails =
    try Some (List.map List.hd lol, List.map List.tl lol) with _ -> None
  in
  match heads_and_tails with
  | Some (heads, tails) -> heads :: zip tails
  | None -> []

let%expect_test "zip" =
  let lol = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ] in
  print_endline (show_int_list_list (zip lol));
  [%expect {| [[1; 4; 7]; [2; 5; 8]; [3; 6; 9]] |}]

let explode s = String.to_seq s |> List.of_seq
