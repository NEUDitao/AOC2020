let rec build_list (ic, l) =
  match input_line ic with
  | line ->  build_list (ic, ((String.sub line 0 7), (String.sub line 7 3)) :: l)
  | exception End_of_file -> close_in ic; List.rev l

let explode input = input |> String.to_seq |> List.of_seq

let find_seat_id code =
  let rec locate c low high=
    match c with
    | [] -> low
    | first::rest ->
    match first with
      | 'F' | 'L' -> locate rest low ((low + high) / 2)
      | 'B' | 'R' -> locate rest (((low + high) / 2) + 1) high 
      | _ -> raise (Failure "ahh")
  in
  let row = locate (explode (fst code)) 0 127 in
  let col = locate (explode (snd code)) 0 7 in
  row * 8 + col

let find_max_seat_id codes =
  List.fold_right (fun x y -> max (find_seat_id x) y ) codes 0

let find_missing_id codes =
  let sorted_codes = List.sort (fun a b -> a - b) codes in
  let small = List.nth sorted_codes 0 in
  let rec find_missing codes next =
    match codes with
    | [] -> raise (Failure "oh no")
    | first::rest -> if first != next then next else find_missing rest (next + 1)
    in
    find_missing sorted_codes small

let () =
  let ic = open_in "input.txt" in
  let l = (build_list (ic, [])) in
  print_endline ("part 1: "^string_of_int(find_max_seat_id l)); (*963*)
  print_endline ("part 2: "^string_of_int(find_missing_id (List.map find_seat_id l)));; (*592*)
  