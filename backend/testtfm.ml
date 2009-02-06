open Format
open Tfm

let _ =
  match Array.length Sys.argv with
    | 1 ->
	printf "Usage : tfm <file1.tfm> <file2.tfm> ...\n"
    | n ->
	for i = 1 to n-1 do
	  let s = Sys.argv.(i) in
	    Print.print_tfm s std_formatter (read_file s)
	done
