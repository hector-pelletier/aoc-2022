(* Advent of code 2022 day 08 *)

(* Input Parsing *)
let parse_input filename =
	let ic = open_in filename in
	let grid = Hashtbl.create 10000 in
	let rec read_input i j =
		match input_char ic with
		| '\n' 	-> read_input (i+1) 0
		| c		-> 
			Hashtbl.add grid (i, j) (int_of_char c); 
			read_input i (j+1)
		| exception End_of_file -> close_in ic; grid in
	read_input 0 0;;

(* Commons *)
let dirs = [(0, 1); (0, -1); (1, 0); (-1, 0)];;

(* Silver *)
let silver input =
	let rec visible_from h (x, y) (dx, dy) =
		let np = (x + dx, y + dy) in
		match Hashtbl.find input np with
		| hp -> if hp < h then visible_from h np (dx, dy) else false
		| exception Not_found -> true in
	let visible h (x, y) = 
		List.fold_left (fun a d -> a || visible_from h (x,y) d) false dirs in	
	Hashtbl.fold (fun k v a -> a + (if visible v k then 1 else 0)) input 0;;
	
(* Gold *)
let gold input =
	let rec visibility n h (x, y) (dx, dy) =
		let np = (x + dx, y + dy) in
		match Hashtbl.find input np with
		| hp -> if hp < h then visibility (n+1) h np (dx, dy) else (n+1)
		| exception Not_found -> n in
	let score h (x, y) =
		List.fold_left (fun a d -> a * visibility 0 h (x,y) d) 1 dirs in
	Hashtbl.fold (fun k v a -> let s = score v k in if a > s then a else s) input 0;;
	
(* Main *)
let main () =
	let input = parse_input "./input-08.txt" in
	Printf.printf "Silver: %d\n" (silver input);
	Printf.printf "Gold  : %d\n" (gold input);;