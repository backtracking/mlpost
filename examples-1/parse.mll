{
  open Lexing
}

let alpha_lower = ['a'-'z' ]
let alpha_upper = ['A'-'Z']
let alpha = ['a' - 'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = alpha_lower (alpha | digit | '\'' | '_')*
let blank = [' ' '\t' '\n' '\r' ]

rule scan = parse
  | ">>" { Printf.printf "</p> </div><hr>"; scan lexbuf }
  | "<<" (identifier as i)
      { 
        Printf.printf "<p id=\"%sa\"><img src=\"%s.png\" /></p>" i i;
        Printf.printf "<a href=\"#%sa\" onclick=\"\
                       toggle_visibility('%s');\">show/hide code</a>" i i;
        Printf.printf "<div id=\"%s\" style='display:none;'>" i;
        Printf.printf "<p>";
        scan lexbuf
      }
  | blank { scan lexbuf }
  | eof { Printf.printf "%!" }


{
let _ = 
  let buf = Lexing.from_channel stdin in
  scan buf

}

