
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
      let line = Scanf.sscanf first "%s@:%s" (fun key value -> (key, value)) in
      Hashtbl.add ht (fst line) (snd line);
      str_list_to_hash_table rest ht in
  List.map (fun x -> str_list_to_hash_table x (Hashtbl.create 8)) strs

let fields = [ "ecl" ; "pid" ; "eyr" ; "hcl" ; "byr" ; "iyr" ; "hgt"]

let contains_passport_fields passport = 
  let keys = Hashtbl.fold (fun k _ acc -> k::acc) passport [] in
  List.fold_right (fun x y -> List.mem x keys && y) fields true

let verify_fields passport : bool =
  let is_int input = Str.string_match (Str.regexp "[0-9]+") input 0 in
  if contains_passport_fields passport then
    List.fold_right (fun x y ->
      let value = Hashtbl.find passport x in
      let m = (match x with
      | "ecl" -> List.mem value ["amb"; "blu" ; "brn"; "gry"; "grn"; "hzl"; "oth"]
      | "pid" -> is_int value && String.length value == 9
      | "eyr" -> is_int value && int_of_string value >= 2020 && int_of_string value <= 2030
      | "hcl" -> Str.string_match (Str.regexp "#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]") value 0
      | "byr" -> is_int value && int_of_string value >= 1920 && int_of_string value <= 2002
      | "iyr" -> is_int value && int_of_string value >= 2010 && int_of_string value <= 2020
      | "hgt" -> if Str.string_match (Str.regexp "[0-9]+cm") value 0 then 
        Scanf.sscanf value "%dcm" (fun hgt -> hgt >= 150 && hgt <= 193)
        else if Str.string_match (Str.regexp "[0-9]+in") value 0 then
        Scanf.sscanf value "%din" (fun hgt -> hgt >= 59 && hgt <= 76)
        else false
      | _ -> false
      ) in
      
      m && y) fields true
  else false

let verify_passports l valid_passport =
  List.fold_right (fun x y -> (if valid_passport x then 1 else 0) + y) l 0

let () =
  let ic = open_in "input.txt" in
  let l = parse_list (build_list (ic)) in
  print_endline ("part 1: "^string_of_int(verify_passports l contains_passport_fields)); (* 216 *)
  print_endline ("part 2: "^string_of_int(verify_passports l verify_fields)); (* 216 *)