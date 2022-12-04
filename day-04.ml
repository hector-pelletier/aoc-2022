(* Advent of code 2022 day 04 *)

(* Input Parsing *)
let parse_input filename =
	let ic = open_in filename in
	let rec process () =
		match input_line ic with
		| line -> ( Scanf.sscanf line "%u-%u,%u-%u" (fun a b c d -> ((a,b),(c,d))) ) :: (process ())
		| exception End_of_file -> close_in ic; [] in
	process ();;

(* Silver *)
let silver input = 
	let contains (a, b) (c, d) = a <= c && b >= d in
	List.filter (fun (i1, i2) -> contains i1 i2 || contains i2 i1) input |> List.length;;
	
(* Gold *)
let gold input = 
	let overlaps (a, b) (c, d) = (a <= c && b >= c) || (c <= a && d >= a) in
	List.filter (fun (i1, i2) -> overlaps i1 i2) input |> List.length;;
	
(* Main *)
let main () =
	let input = parse_input "./input-04.txt" in
	Printf.printf "Silver: %d\n" (silver input);
	Printf.printf "Gold  : %d\n" (gold input);;