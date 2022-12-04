(* Advent of code 2022 day 02*)

(* Input Parsing *)
let parse_input filename =
	let ic = open_in filename in
	let rec process_lines () =
		match input_line ic with
		| line -> (Scanf.sscanf line "%c %c" (fun a b -> (a, b)))::(process_lines ())
		| exception End_of_file -> close_in ic; [] in
	process_lines ();;
	
(* Commons *)
let points c = Char.code c - Char.code 'W';;

(* Silver *)
let silver input = 
	let score (a, b) =
		let win = match (a, b) with
			| ('A', 'Y') | ('B', 'Z') | ('C', 'X') -> 6
			| ('A', 'X') | ('B', 'Y') | ('C', 'Z') -> 3
			| _ -> 0 in
		win + points b in
	List.fold_left (fun p ins -> p + score ins) 0 input;;
		
(* Gold *)
let gold input = 
	let score (a, b) =
		let move (x, y) =
			match (x, y) with
			| ('A', 'X') | ('B', 'Z') | ('C', 'Y') -> 'Z'
			| ('A', 'Y') | ('B', 'X') | ('C', 'Z') -> 'X'
			| _ -> 'Y' in
		let win c = 
			match c with
			| 'Z' -> 6
			| 'Y' -> 3
			| _ -> 0 in
		(win b) + (points (move (a, b))) in
	List.fold_left (fun p ins -> p + score ins) 0 input;;
	
(* Main *)
let main () =
	let input = parse_input "./input-02.txt" in
	Printf.printf "Silver: %d\n" (silver input);
	Printf.printf "Gold  : %d\n" (gold input);;