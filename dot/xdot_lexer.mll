(* File lexer.mll *)
{
  open Xdot_type
exception Eof
exception Xdot_error_n of int

let ios = int_of_string

}

let int = '-'? ['0'-'9']+
let blank = [' ' '\t']
let end_of_line = ['\n' ]
let ident = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9']*

rule main = parse
  | blank+   { main lexbuf }     (* skip blanks *)
  | end_of_line        { main lexbuf }
  | "digraph" blank* ident+ blank* '{' {digraph Xdot_type.digraph lexbuf}
  | eof            { raise Eof }

and digraph d = parse
  | blank+   { digraph d lexbuf }     (* skip blanks *)
  | end_of_line        { digraph d lexbuf }
  | "node" blank* '[' [^';']* ';' { digraph d lexbuf}
  | "edge" blank* '[' [^';']* ';' { digraph d lexbuf}
  | "graph" blank* '[' "bb" '=' '"' 
      (int as x1) ',' (int as y1) ',' (int as x2) ',' (int as y2) '"' 
      [^']']* ']' ';'
      {digraph {d with bounding_box = ((ios x1,ios y1),(ios x2,ios y2))} lexbuf}
  | (ident+ as id) blank* '[' 
      {digraph 
         {d with nodes = {nname = id; npos = node lexbuf; nshape=Plain}::d.nodes}
         lexbuf}
  | (ident+ as id1) blank* "->" blank* (ident+ as id2) blank* '[' 
      {digraph 
         {d with edges =
             {estart = id1;eend = id2;epath=Bspline (edge lexbuf)}::d.edges}
         lexbuf}
  | '}' {{d with 
            edges = List.rev d.edges;
            nodes = List.rev d.nodes}}

and node = parse
  | "pos=" '"' (int as x) ',' (int as y) [^']']* ']' ';' {(ios x,ios y)}
  | [^',']* ',' blank* {node lexbuf}

and edge = parse
  | "pos=" '"' "e," int ',' int {path lexbuf}
  | "pos=" '"'  (int as x) ',' (int as y) {(ios x,ios y)::(path lexbuf)}
  | [^',']* ',' blank* {edge lexbuf}

and path = parse
  | blank+ {path lexbuf}
  | (int as x) ',' (int as y) {(ios x,ios y)::(path lexbuf)}
  | '"' [^']']* ']' ';' {[]}

(*
rule draw conti = parse
  | 'c' blank* (int as i) blank* '-' {C (nchar (int_of_string i))}
  | 'e' blank* (int as x) blank* (int as y) blank* (int as w) 
      blank* (int as h) -> {Eu (x,y,w,h)}
  | blank* {draw lexbuf}
  | '"' {node lexbuf}
*)
(*rule nint n = parse
  | int as i {(int_of_string i)::(nint_aux (n-1))}
  | _ -> {raise (Xdot_error_n n)}

and nint_aux = function
  | 0 -> []
  | n -> nint n

rule nchar n = parse
  | c as c {c^(nint_aux (n-1))}
  | _ -> {raise (Xdot_error_n n)}

and nchar_aux = function
  | 0 -> ""
  | n -> nchar n
*)
