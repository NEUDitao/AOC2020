
let rec build_int_list (ic, l) =
  match input_line ic with
  | line -> build_int_list (ic, (int_of_string line) :: l)
  | exception End_of_file -> close_in ic; List.rev l

let rec string_of_list_of_int (l: int list)  =
  match l with
  | [] -> ""
  | first::rest -> (string_of_int first)^", "^(string_of_list_of_int rest)

let rec find_2020_pair_with_int (i: int) (l: int list) : (int*int) option =
  match l with
  | [] -> None
  | first::rest -> if first + i == 2020 then Some((first, i)) else find_2020_pair_with_int i rest
  

let rec find_int (i : int) ( l : int list) : int option =
  match l with
  | [] -> None
  | first::rest -> if first == i then Some(first) else find_int i rest

let rec find_2020_pair (l1 : int list) (l2 : int list): int =
  match l1 with
  | [] -> -1
  | first::rest -> 
      let pair = find_int (2020 - first) l2 in
      match pair with
      | None -> find_2020_pair rest l2
      | Some p -> p * first

let rec find_2020_triplet (l1: int list) (l2: int list) (l3: int list): int =

  let rec find_2020_triplet_two_lists i l2 l3 =
    match l2 with
    | [] -> None
    | first::rest -> 
      let find = find_int (2020 - i - first) l3 in
      match find with
      | None -> find_2020_triplet_two_lists i rest l3
      | Some(p) -> Some(i, first, p) 
    in

  match l1 with
  | [] -> -1
  | first::rest ->
    let triplet = find_2020_triplet_two_lists first l2 l3 in
    match triplet with
    | None -> find_2020_triplet rest l2 l3
    | Some (first, second, third) -> first * second * third

let () =
  let ic = open_in "input.txt" in
  let l = build_int_list(ic, []) in
  
  print_endline ("part 1: "^string_of_int (find_2020_pair l l));(* 719796 *)
  print_endline ("part 2: "^string_of_int (find_2020_triplet l l l));


