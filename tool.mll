
{ 
  open Format
  open Lexing
  open Arg

  let files = 
    Queue.create ()

  let add_file f = 
    if not (Filename.check_suffix f ".ml") then begin
      eprintf "mlpost: don't know what to do with %s@." f;
      exit 1
    end;
    if not (Sys.file_exists f) then begin
      eprintf "mlpost: %s: no such file@." f;
      exit 1
    end;
    Queue.add f files

  let spec =
    [ ]

  let () = 
    Arg.parse spec add_file "usage: mlpost [options] files..."
}

(* scan the main LaTeX file to extract its prelude *)

rule scan = parse
  | eof { () }

{
  let command s =
    let out = Sys.command s in
    if out <> 0 then begin
      eprintf "mlpost: the following command failed:@\n%s@." s;
      exit out
    end

  let ocaml args =
    let cmd = "ocaml " ^ String.concat " " (Array.to_list args) in
    let out = Sys.command cmd in
    if out <> 0 then Unix.execvp "ocaml" args

  let compile f =
    let bn = Filename.chop_extension f in
    let mlf = bn ^ "_mlpost.ml" in
    let cout = open_out mlf in
    Printf.fprintf cout "open Mlpost\n";
    Printf.fprintf cout "# 1 \"%s\"\n" f;
    begin 
      let cin = open_in f in
      try while true do output_char cout (input_char cin) done
      with End_of_file -> ()
    end;
    let fmp = bn ^ ".mp" in
    Printf.fprintf cout "let () = Command.dump \"%s\"\n" fmp;
    close_out cout;
    ocaml [|"mlpost.cma"; mlf|];
    command (sprintf "mpost %s end" fmp);
    Sys.remove mlf

  let () = Queue.iter compile files
}
