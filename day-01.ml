(* Advent of code 2022 day 01 *)

(* Input Parsing *)
let parse_input filename =
	let ic = open_in filename in
	let rec get_elves acc l =
		match input_line ic with
		| "" -> get_elves [] (acc::l)
		| s -> get_elves ((int_of_string s)::acc) l
		| exception End_of_file -> (acc::l) in
	get_elves [] [];;
	
(* Commons *)
let sorted_weights l =
	List.map (List.fold_left ( + ) 0) l |> List.sort (fun x y -> - compare x y);;

(* Silver *)
let silver input =
	let h::tl = sorted_weights input in h;;
	
(* Gold *)
let gold input = 
	let fs::sc::th::tl = sorted_weights input in fs + sc + th;;
	
(* Main *)
let main () =
	let input = parse_input "./input-01.txt" in
	Printf.printf "Silver: %d\n" (silver input);
	Printf.printf "Gold  : %d\n" (gold input);;