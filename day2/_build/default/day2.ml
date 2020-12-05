open Scanf

let rec build_list (ic, l) =
  match input_line ic with
  | line ->  build_list (ic, line :: l)
  | exception End_of_file -> close_in ic; List.rev l

let explode input = input |> String.to_seq |> List.of_seq

let rec count_chars_in_str (str : char list)  c =
  match str with
  | [] -> 0
  | first::rest -> if first == c then
    1 + count_chars_in_str rest c
    else count_chars_in_str rest c

let parse_line line =
  (* let params = Str.split (Str.regexp ":") line in
  let constraints = List.nth params 0 in
  let password = explode (List.nth params 1) in
  let bounds = Str.split (Str.regexp "-") constraints in
  let low = int_of_string( List.nth bounds 0) in
  let upper_split = Str.split (Str.regexp " ") (List.nth bounds 1) in
  let high = int_of_string (List.nth upper_split 0) in
  let c = List.nth upper_split 1 in *)
  sscanf line "%d-%d %c: %s" (fun low high c password -> (low, high, c, (explode password)))

let verify_password pass = 
  let (low, high, c, password) = parse_line pass in
  low <= (count_chars_in_str password c )
  && (count_chars_in_str password c) <= high

let verify_part_2 pass = 
  let (low, high, c, password) = parse_line pass in
  ((List.nth password (low-1)) == c) <> ((List.nth password (high-1)) == c)
  

let rec verify_passwords l verifier =
  match l with
  | [] -> 0
  | first::rest -> if verifier first
    then 1 + verify_passwords rest verifier
    else verify_passwords rest verifier

let () =
  let ic = open_in "input.txt" in
  let l = build_list (ic, []) in
    print_endline ("part 1: "^string_of_int(verify_passwords l verify_password)); (* 546 *)
    print_endline ("part 2: "^string_of_int(verify_passwords l verify_part_2)); (* 411 *)
