module Path = struct
  let move_in = List.cons
  let move_out = List.tl
  let root = []
  let to_string path = "/" ^ (List.rev path |> String.concat "/")
end

module String_map = Map.Make (String)

module Parser = struct
  open Angstrom

  let is_eol = function '\n' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let ls = string "ls" *> return `Ls
  let cd = string "cd " *> take_till is_eol >>| fun dir -> `Cd dir
  let command = string "$ " *> (ls <|> cd)

  let file =
    take_while1 is_digit >>= fun size ->
    take_till is_eol *> return (`File (int_of_string size))

  let dir = string "dir " *> take_till is_eol *> return `Dir
  let response = file <|> dir
  let line = command <|> response <* satisfy is_eol

  let parse input =
    parse_string ~consume:All (Angstrom.many line) input |> Result.get_ok
end

let compute_sizes lines =
  let rec add_size sizes path size =
    let sizes' =
      String_map.update (Path.to_string path)
        (function Some size' -> Some (size' + size) | None -> Some size)
        sizes
    in
    match path with [] -> sizes' | _ :: tl -> add_size sizes' tl size
  in
  List.fold_left
    (fun (path, sizes) line ->
      match line with
      | `Dir -> (path, sizes)
      | `Ls -> (path, sizes)
      | `Cd "/" -> (Path.root, sizes)
      | `Cd ".." -> (Path.move_out path, sizes)
      | `Cd dir -> (Path.move_in dir path, sizes)
      | `File size -> (path, add_size sizes path size))
    ([], String_map.empty) lines
  |> snd

let sum_at_most ?(max_size = 100_000) sizes =
  String_map.fold
    (fun _ size acc -> if size <= max_size then acc + size else acc)
    sizes 0

let smallest_sufficient ?(total = 70_000_000) ?(needed = 30_000_000) sizes =
  let used = String_map.find Path.(root |> to_string) sizes in
  let unused = total - used in
  let min_size = needed - unused in
  String_map.fold
    (fun _ size acc -> if size >= min_size && size <= acc then size else acc)
    sizes Int.max_int

let example =
  {|$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k
|}

let%expect_test "part1" =
  let parsed = Parser.parse example in
  let sizes = compute_sizes parsed in
  String_map.iter (fun path size -> Format.printf "%s %i\n" path size) sizes;
  [%expect {|
    / 48381165
    /a 94853
    /a/e 584
    /d 24933642 |}];
  print_int (sum_at_most sizes);
  [%expect {| 95437 |}];
  print_int
    (Util.get_input () |> In_channel.input_all |> Parser.parse |> compute_sizes
   |> sum_at_most);
  [%expect {| 1449447 |}]

let%expect_test "part2" =
  print_int (Parser.parse example |> compute_sizes |> smallest_sufficient);
  [%expect {| 24933642 |}];
  print_int
    (Util.get_input () |> In_channel.input_all |> Parser.parse |> compute_sizes
   |> smallest_sufficient);
  [%expect {| 8679207 |}]
