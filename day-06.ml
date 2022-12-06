(* Advent of code 2022 day 06 *)

(* Input Parsing *)
let parse_input filename =
	let ic = open_in filename in
	let line = input_line ic in
	close_in ic; line;;

(* Commons *)
let find_start size packet =
	let rec all_diff s n = if n < 0 then true else (if String.contains_from s (n+1) s.[n] then false else all_diff s (n-1)) in
	let rec rfind a = if all_diff (String.sub packet a size) (size-2) then a else rfind (a+1) in
	size + rfind 0;;  

(* Silver *)
let silver input = find_start 4 input;;
	
(* Gold *)
let gold input = find_start 14 input;;	
	
(* Main *)
let main () =
	let input = parse_input "./input-06.txt" in
	Printf.printf "Silver: %d\n" (silver input);
	Printf.printf "Gold  : %d\n" (gold input);;