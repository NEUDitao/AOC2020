
let rec print_list l =
  match l with
  | [] -> print_endline "done"
  | first::rest -> print_endline first; print_list rest

let build_list (ic) =
  let rec build_list_with_acc ic l acc =
    match input_line ic with
    | line -> if String.length line == 0 
      then build_list_with_acc ic ((String.concat " " acc)::l) []
      else build_list_with_acc ic l (line::acc)
    | exception End_of_file -> close_in ic; List.rev ((String.concat " " acc)::l)
  in
  build_list_with_acc ic [] []

let parse_list l =
  let strs = List.map (fun x -> Str.split (Str.regexp " ") x) l in
  let rec str_list_to_hash_table l ht =
    match l with
    | [] -> ht
    | first::rest -> 
      print_endline first;
      let line = Scanf.sscanf first "%s@:%s" (fun key value -> (key, value)) in
      Hashtbl.add ht (fst line) (snd line);
      str_list_to_hash_table rest ht in
  List.map (fun x -> str_list_to_hash_table x (Hashtbl.create 8)) strs

let fields = [ "ecl" ; "pid" ; "eyr" ; "hcl" ; "byr" ; "iyr" ; "hgt"]

let contains_passport_fields passport = 
  let keys = Hashtbl.fold (fun k v acc -> ignore(v); k::acc) passport [] in
  List.fold_right (fun x y -> List.mem x keys && y) fields true

let rec verify_passports l valid_passport =
  match l with
  | [] -> 0
  | first::rest -> if valid_passport first then
    1 + verify_passports rest valid_passport 
    else verify_passports rest valid_passport

let () =
  let ic = open_in "input.txt" in
  let l = parse_list (build_list (ic)) in
  print_endline ("part 1: "^string_of_int(verify_passports l contains_passport_fields)); (* 216 *)