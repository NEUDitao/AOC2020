let rec build_list (ic, l) =
  match input_line ic with
  | line ->  build_list (ic, line :: l)
  | exception End_of_file -> close_in ic; List.rev l

let explode input = input |> String.to_seq |> List.of_seq

let tree row col =
  List.nth (explode row) col == '#'

let tobaggan_time (l : string list) col_diff=
  let rec tobaggan_time_acc (l : string list) col =
    match l with
    | [] -> 0
    | first::rest -> 
      let new_col = (col + col_diff) mod String.length first in
      if tree first col
      then 1 + tobaggan_time_acc rest new_col
      else tobaggan_time_acc rest new_col in
  tobaggan_time_acc l 0

let tobaggan_every_other l col_diff = 
  let rec tobaggan_every_other_acc l col check =
    match l with
    | [] -> 0
    | first::rest ->
      let new_col = (col + col_diff) mod String.length first in
      if check && (tree first col)
        then 1 + tobaggan_every_other_acc rest new_col (not check)
        else if check then tobaggan_every_other_acc rest new_col (not check)
        else tobaggan_every_other_acc rest col (not check)
      in
    tobaggan_every_other_acc l 0 true

let part2 l =
  let one_one = tobaggan_time l 1 in
  let three_one = tobaggan_time l 3 in
  let five_one = tobaggan_time l 5 in
  let seven_one = tobaggan_time l 7 in
  let one_two = tobaggan_every_other l 1 in
  one_one * three_one * five_one * seven_one * one_two

let () =
  let ic = open_in "input.txt" in
  let l = build_list (ic, []) in
  print_endline ("part 1: "^string_of_int(tobaggan_time l 3)); (*214*)
  print_endline ("part 2: "^string_of_int(part2 l)) (* 8336352024 *)