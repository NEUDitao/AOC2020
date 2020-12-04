
let rec build_int_list (ic, l) =
  match input_line ic with
  | line -> build_int_list (ic, (int_of_string line) :: l)
  | exception End_of_file -> close_in ic; List.rev l

let rec string_of_list_of_int (l: int list)  =
  match l with
  | [] -> ""
  | first::rest -> (string_of_int first)^", "^(string_of_list_of_int rest)

let rec find_2020_pair (l1 : int list) (l2 : int list): int =
  
  let rec find_2020_pair_with_int ((i: int), (l: int list)) =
    match l with
    | [] -> (-1, -1)
    | first::rest -> if first + i == 2020 then (first, i) else find_2020_pair_with_int(i, rest)
  in

  match l1 with
  | [] -> -1
  | first::rest -> 
      let (pair1, pair2) = find_2020_pair_with_int(first, l2) in
      if pair1 != -1 && pair2 != -1
      then pair1 * pair2
      else find_2020_pair rest l2
      

let () =
  let ic = open_in "input.txt" in
  let l = build_int_list(ic, []) in
  print_endline ("part 1: "^string_of_int (find_2020_pair l l))


