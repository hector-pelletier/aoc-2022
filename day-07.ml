(* Advent of code 2022 day 07 *)

type fnode = 
	| File of int
	| Dir of (fnode option) * ((string, fnode) Hashtbl.t)

(* Input Parsing *)
let parse_input filename =
	let ic = open_in filename in
	let rec read_lines () = 
		match input_line ic with
		| line -> (Scanf.sscanf line "%s %s %s" (fun a b c -> (a,b,c)))::(read_lines ())
		| exception End_of_file -> close_in ic; [] in
	
	let fs = Dir (None, Hashtbl.create 100) in
	
	let rec parse ins cd =
		match ins with
		| [] -> fs
		| h::tl -> let Dir(up, down) = cd in 
			match h with 
			| ("$", "cd", "/")  -> parse tl fs
			| ("$", "cd", "..") -> let Some d = up in parse tl d
			| ("$", "cd", s) 	-> parse tl (Hashtbl.find down s)
			| ("$", "ls", _) 	-> parse tl cd
			| ("dir", n, _) 	-> 
				Hashtbl.add down n (Dir(Some cd, Hashtbl.create 100)); 
				parse tl cd
			| (i, n, _) 		-> 
				Hashtbl.add down n (File(int_of_string i)); 
				parse tl cd in
				
	parse (read_lines ()) fs;;
	
(* Tree walk *)
let treefold cd acc acc_f =
	let rec aux acc cd =
		match cd with 
		| File(i) -> (acc, i)
		| Dir(_, down) -> 
			let f k v (a, s) = 
				let (na, ds) = aux a v in (na, s + ds) in 
			let (acc, size) = Hashtbl.fold f down (acc, 0) in
			(acc_f acc size, size) in
	aux acc cd;;	
	
(* Silver *)
let silver input = 
	let sum_small acc size = acc + (if size <= 100000 then size else 0) in
	fst (treefold input 0 sum_small);;
	
(* Gold *)
let gold input = 
	let sizes = fst (treefold input [] (fun l s -> s::l)) in
	let f s = (List.hd sizes) - s <= 40000000 in
	List.filter f sizes |> List.sort compare |> List.hd;;
		
(* Main *)
let main () =
	let input = parse_input "./input-07.txt" in
	Printf.printf "Silver: %d\n" (silver input);
	Printf.printf "Gold  : %d\n" (gold input);;