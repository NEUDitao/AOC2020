let build_list (ic) =
  let rec build_list_with_acc ic l acc =
    match input_line ic with
    | line -> if String.length line == 0 
      then build_list_with_acc ic ((String.concat " " acc)::l) []
      else build_list_with_acc ic l (line::acc)
    | exception End_of_file -> close_in ic; List.rev ((String.concat " " acc)::l)
  in
  build_list_with_acc ic [] []

let rec print_char_list l =
  match l with
  | [] -> print_endline "end_inner"
  | first::rest -> print_char first; print_char_list rest

let rec print_char_list_list l =
  match l with
  | [] -> print_endline "done"
  | first::rest -> print_char_list first; print_char_list_list rest

let explode input = input |> String.to_seq |> List.of_seq
let get_customs_counts l =
  let unique_l = List.map (fun x -> List.filter (fun c -> c != ' ')
   (List.sort_uniq (fun a b -> int_of_char a - int_of_char b) (explode x))) l in

  List.fold_right (fun x y -> List.length x + y) unique_l 0

let get_everyones_counts l =
  let split_l = List.map (fun x -> String.split_on_char ' ' x) l in
  
  let rec get_everyones_counts_acc l =
    match l with 
    | [] -> 0
    | first::rest -> 
      let first_in_list = explode (List.nth first 0) in
      (List.fold_right (fun x z -> if (List.for_all (fun y -> String.contains y x) first)
        then 1 + z else z) first_in_list 0) + get_everyones_counts_acc rest in

  
  get_everyones_counts_acc split_l

let () =
  let ic = open_in "input.txt" in
  let l = (build_list (ic)) in
  print_endline ("part 1: "^string_of_int(get_customs_counts l)); (* 6596 *)
  print_endline ("part 2: "^string_of_int(get_everyones_counts l));
