(* Advent of code  day  *)

(* Input Parsing *)
let parse_input filename =
	let ic = open_in filename in
	"";;
	

(* Commons *)

(* Silver *)
let silver input = 0;;
	
	
(* Gold *)
let gold input = 0;;
	
	
(* Main *)
let main () =
	let input = parse_input "./input-01.txt" in
	Printf.printf "Silver: %d\n" silver input;
	Printf.printf "Gold  : %d\n" gold input;;