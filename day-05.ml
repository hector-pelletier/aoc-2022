(* Advent of code 2022 day 05 *)

(* Input Parsing *)
let parse_input filename =
	let ic = open_in filename in
	let rec parse_instructions () =
		match input_line ic with
		| line -> (Scanf.sscanf line "move %u from %u to %u" (fun a b c -> (a, b-1, c-1))) :: (parse_instructions ())
		| exception End_of_file -> close_in ic; [] in
	let crates = [|
					['N';'T';'B';'S';'Q';'H';'G';'R'];
					['J';'Z';'P';'D';'F';'S';'H'];
					['V';'H';'Z'];
					['H';'G';'F';'J';'Z';'M'];
					['R';'S';'M';'L';'D';'C';'Z';'T'];
					['J';'Z';'H';'V';'W';'T';'M'];
					['Z';'L';'P';'F';'T'];
					['S';'W';'V';'Q'];
					['C';'N';'D';'T';'M';'L';'H';'W']
				 |] in
	(crates, parse_instructions ());;
	

(* Commons *)
let str_from_piles crates =
	let buf = Buffer.create 16 in
	Array.iter (fun l -> Buffer.add_char buf (List.hd l)) crates; Buffer.contents buf;;
	
let rec follow_ins l move crates =
		match l with
		| h::t ->
			let (n, i, j) = h in
			let (a, b) = move n crates.(i) crates.(j) in
			crates.(i) <- a; crates.(j) <- b; follow_ins t move crates
		| [] -> crates
	
	
(* Silver *)
let silver (crates, ins) = 
	let rec move n a b =
		if n = 0 then (a, b) else let h::t = a in move (n-1) t (h::b) in
	str_from_piles (follow_ins ins move crates);;
	
	
(* Gold *)
let gold (crates, ins) = 
	let move n a b =
		let rec h_move n a b p = 
			if n = 0
			then match p with
				| h::t -> h_move n a (h::b) t
				| [] -> (a, b)
			else let h::t = a in h_move (n-1) t b (h::p) in
		h_move n a b [] in
	str_from_piles (follow_ins ins move crates);;
	
	
(* Main *)
let main () =
	let (crates, ins) = parse_input "./input-05.txt" in
	Printf.printf "Silver: %s\n" (silver (Array.copy crates, ins));
	Printf.printf "Gold  : %s\n" (gold (Array.copy crates, ins));;