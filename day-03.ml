(* Advent of code 2022 day 03 *)

module CSet = Set.Make(Char);;

(* Input Parsing *)
let parse_input filename =
	let ic = open_in filename in
	let rec str_to_sets i1 i2 s1 s2 s =
		if i1 < 0 then (s1, s2) 
		else str_to_sets (i1 - 1) (i2 - 1) (CSet.add s.[i1] s1) (CSet.add s.[i2] s2) s in
	let rec pack_sacks () =
		match input_line ic with
		| line -> 
			let len = String.length line in
			(str_to_sets (len / 2 - 1) (len - 1) CSet.empty CSet.empty line)::(pack_sacks ())
		| exception End_of_file -> close_in ic; [] in
	pack_sacks ();;
	
(* Commons *)
let priority c =
	let code = Char.code c in 
	if code >= Char.code 'a' then code - Char.code 'a' + 1 else code - Char.code 'A' + 27;;

(* Silver *)
let silver input = 
	let find_error (s1, s2) = CSet.inter s1 s2 |> CSet.choose |> priority in 
	List.fold_left (fun s pack -> s + find_error pack) 0 input;;
	
(* Gold *)
let gold input =
	let find_badge e1 e2 e3 = 
		let sack (s1, s2) = CSet.union s1 s2 in
		CSet.inter (sack e1) (sack e2) |> CSet.inter (sack e3) |> CSet.choose |> priority in
	let rec sum_groups s l =
		match l with
		| a::b::c::tl -> sum_groups (s + find_badge a b c) tl
		| [] -> s in
	sum_groups 0 input;;
	
(* Main *)
let main () =
	let input = parse_input "./input-03.txt" in
	Printf.printf "Silver: %d\n" (silver input);
	Printf.printf "Gold  : %d\n" (gold input);;