open Bigarray

module Array2 = struct
  include Array2

  let transpose array =
    let transposed = create Int c_layout (dim2 array) (dim1 array) in
    for i = 0 to dim1 array - 1 do
      for ii = 0 to dim2 array - 1 do
        Array2.set transposed ii i (Array2.get array i ii)
      done
    done;
    transposed

  let nonzero array =
    let total = ref 0 in
    for i = 0 to dim1 array - 1 do
      for ii = 0 to dim2 array - 1 do
        if Array2.get array i ii > 0 then total := !total + 1
      done
    done;
    !total

  let max array =
    let max = ref 0 in
    for i = 0 to dim1 array - 1 do
      for ii = 0 to dim2 array - 1 do
        if Array2.get array i ii > !max then max := Array2.get array i ii
      done
    done;
    !max

  let math op array1 array2 =
    let result = create Int c_layout (dim1 array1) (dim2 array2) in
    for i = 0 to dim1 array1 - 1 do
      for ii = 0 to dim2 array1 - 1 do
        Array2.set result i ii
          (op (Array2.get array1 i ii) (Array2.get array2 i ii))
      done
    done;
    result

  let ( + ) = math Int.add
  let ( * ) = math Int.mul

  let print array =
    for i = 0 to dim1 array - 1 do
      let slice = slice_left array i in
      List.init (Array1.dim slice) (fun ii -> Array1.get slice ii)
      |> List.map string_of_int |> String.concat " " |> print_endline
    done
end

module Blockers = struct
  module Int_map = Map.Make (Int)
  include Int_map

  let nearest blockers height = Int_map.find blockers height

  let update blockers index height =
    let rec update' blockers = function
      | 0 -> Int_map.add 0 index blockers
      | n ->
          let blockers' = Int_map.add n index blockers in
          update' blockers' (n - 1)
    in
    update' blockers height
end

let parse input =
  List.map
    (fun line ->
      Util.explode line
      |> List.map (Format.sprintf "%c")
      |> List.map int_of_string |> Array.of_list)
    input
  |> Array.of_list
  |> Array2.of_array Int c_layout

let compute_highest highest next visible =
  let new_highest =
    List.mapi
      (fun index highest' ->
        let tree = Array1.get next index in
        match highest' with
        | Some highest when highest >= tree -> Some highest
        | _ ->
            Array1.set visible index 1;
            Some tree)
      highest
  in
  new_highest

let visible_vertical ?(reverse = false) forest =
  let height = Array2.dim1 forest in
  let width = Array2.dim2 forest in
  let all_visible = Array2.create Int c_layout height width in
  let _ =
    List.fold_left
      (fun highest index ->
        let index = if reverse then height - index - 1 else index in
        let slice = Array2.slice_left forest index in
        let visible = Array2.slice_left all_visible index in
        let new_highest = compute_highest highest slice visible in
        new_highest)
      (List.init width (fun _ -> None))
      (List.init height Fun.id)
  in
  all_visible

let visible forest =
  let open Array2 in
  let top = visible_vertical forest in
  let bottom = visible_vertical ~reverse:true forest in
  let transposed = transpose forest in
  let left = visible_vertical transposed |> transpose in
  let right = visible_vertical ~reverse:true transposed |> transpose in
  let sum = top + bottom + left + right in
  nonzero sum

let compute_scenic row_index highest next scenic_scores =
  let new_highest =
    List.mapi
      (fun index blockers ->
        let tree = Array1.get next index in
        let blocker = Blockers.find tree blockers in
        let view = row_index - blocker in
        Array1.set scenic_scores index view;
        Blockers.update blockers row_index tree)
      highest
  in
  new_highest

let scenic_vertical ?(reverse = false) forest =
  let init_blockers =
    List.fold_left
      (fun blockers i -> Blockers.add i 0 blockers)
      Blockers.empty (List.init 10 Fun.id)
  in

  let height = Array2.dim1 forest in
  let width = Array2.dim2 forest in
  let scenic_scores = Array2.create Int c_layout height width in
  let _ =
    List.fold_left
      (fun blockers index ->
        let index' = if reverse then height - index - 1 else index in
        let slice = Array2.slice_left forest index' in
        let visible = Array2.slice_left scenic_scores index' in
        let new_blockers = compute_scenic index blockers slice visible in
        new_blockers)
      (List.init width (fun _ -> init_blockers))
      (List.init height Fun.id)
  in
  scenic_scores

let scenic forest =
  let open Array2 in
  let top = scenic_vertical forest in
  let bottom = scenic_vertical ~reverse:true forest in
  let transposed = transpose forest in
  let left = scenic_vertical transposed |> transpose in
  let right = scenic_vertical ~reverse:true transposed |> transpose in
  let product = top * bottom * left * right in
  max product

let example = {|30373
25512
65332
33549
35390|} |> String.split_on_char '\n'

let%expect_test "part1" =
  let parsed = parse example in
  print_int (visible parsed);
  [%expect {| 21 |}];
  let input = Util.(get_input () |> lines_of_channel) |> parse in
  print_int (visible input);
  [%expect {| 1829 |}]

let%expect_test "part2" =
  let parsed = parse example in
  print_int (scenic parsed);
  [%expect {| 8 |}];
  let input = Util.(get_input () |> lines_of_channel) |> parse in
  print_int (scenic input);
  [%expect {| 291840 |}]
