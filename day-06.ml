(* Advent of code 2022 day 06 *)

(* Input Parsing *)
let parse_input filename =
	let ic = open_in filename in
	let rec s_to_list n l s =
		if n < 0 then l else s_to_list (n-1) (s.[n]::l) s in
	let line = input_line ic in
	close_in ic; s_to_list (String.length line - 1) [] line;;

(* Commons *)
let find_start size l =
	let rec all_diff l =
		match l with
		| h::tl -> if List.exists (fun x -> x = h) tl then false else all_diff tl
		| [] -> true in
	let rec rfind n l cbuf =
		match l with
		| h::tl -> 
			let ncbuf = (if n > size then List.tl cbuf else cbuf) @ [h] in 
			if n >= size && (all_diff ncbuf)
			then n
			else rfind (n+1) tl ncbuf
		| _ -> 0 in
	rfind 1 l [];;
	

(* Silver *)
let silver input = find_start 4 input;;
	
(* Gold *)
let gold input = find_start 14 input;;	
	
(* Main *)
let main () =
	let input = parse_input "./input-06.txt" in
	Printf.printf "Silver: %d\n" (silver input);
	Printf.printf "Gold  : %d\n" (gold input);;