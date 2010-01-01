(* This is an implementation of a macro preprocessor compatible with the
 * Camlp4Macroparser. *)
(* it currently only supports IFDEF statements:
  IFDEF <uident> THEN <structure_items> [ ELSE <structure_items> ] (END | ENDIF)
*)


{
  let b = Buffer.create 1024
  let symbol_table : (string, unit) Hashtbl.t = Hashtbl.create 17

  let char = Buffer.add_char b
  let def = Hashtbl.mem symbol_table
  let add_symbol s = Hashtbl.add symbol_table s ()

  let linenr = ref 0
  let newline () = incr linenr

  let error = 
    Format.ksprintf 
      (fun s -> 
        let s = Format.sprintf "parse error on line %d: %s" !linenr s in
        failwith s)

  let fn = ref None
  let args = 
    [ "-D", Arg.String add_symbol, "<s>  mark <s> as defined"
    ]

  let usage = "Usage: myocamlmacroparser <options> inputfile"

  let _ = Arg.parse args (fun s -> fn := Some s) usage
}

let alpha = [ 'A' - 'Z' 'a' - 'z' '0' - '9' ]
let other = ( alpha | '_')
let blank = [ ' ' '\t' '\n' ]
let uident =  alpha other*

rule normal = parse
  | "IFDEF" blank* (uident as id) blank* "THEN" 
     { 
       if def id then thenbranch lexbuf else skiptoelse lexbuf
     }
  | ("ELSE" | "END" | "THEN" | "ENDIF" as s)
    { error "unexpected token: %s" s }
  | eof { () }
  | _ as c { char c ; normal lexbuf }

and thenbranch = parse
  | "ELSE" { skiptoend lexbuf }
  | "END" | "ENDIF" { normal lexbuf }
  | ("THEN" as s) { error "unexpected token in then branch: %s" s }
  | eof { error "unexpected end of file %s" "" }
  | _ as c { char c; thenbranch lexbuf }

and skiptoelse = parse
  | "ELSE" { elsebranch lexbuf }
  | "END" | "ENDIF" { normal lexbuf }
  | eof { error "unexpected end of file%s" "" }
  | _ { skiptoelse lexbuf }

and skiptoend = parse
  | "END" | "ENDIF" { normal lexbuf }
  | eof { error "unexpected end of file%s" "" }
  | _ { skiptoend lexbuf }

and elsebranch = parse
  | "END" | "ENDIF" { normal lexbuf }
  | ("ELSE" | "THEN" as s) { error "unexpected token: %s" s }
  | eof { error "unexpected end of file%s" "" }
  | _ as c { char c; elsebranch lexbuf }

{
  let _ = 
    let cin = 
      match !fn with
      | None -> stdin
      | Some s -> open_in s in
    normal (Lexing.from_channel cin);
    close_in cin;
    Buffer.output_buffer stdout b
}
