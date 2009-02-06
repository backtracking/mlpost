open Format
open Dvi

let _ =
  match Array.length Sys.argv with
    | 1 ->
	printf "Usage : dvi <file1.dvi> <file2.dvi> ...\n"
    | n ->
	for i = 1 to n-1 do
	  let s = Sys.argv.(i) in
	    Print.print_doc s std_formatter (read_file s)
	done
