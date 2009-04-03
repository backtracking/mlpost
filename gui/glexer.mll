
{

  type dimension =
    | Pt | Cm | Mm | Bp | Inch

  type value = float * dimension

  type elt = 
    | Num of string * value
    | Point of string * value * value

  let elements = ref []

  let add_elt e = 
    elements := e :: !elements

  let dim_of_string = function
    | "pt" -> Pt
    | "bp" -> Bp
    | "cm" -> Cm
    | "mm" -> Mm
    | "inch" -> Inch
    | _ -> assert false
}

let space = [' ' '\t' '\n' '\r']
let number = ['0'-'9']+ '.' ['0'-'9']* | ['0'-'9']* '.' ['0'-'9']+
let dimension = "pt" | "bp" | "cm" | "mm" | "inch"
let string = '"' [^ '"']* '"'

rule read = parse
  | '#' [^ '\n']* '\n'
      { read lexbuf }
  | "num" space* (string as s) space* (number as n) space* (dimension as d)
      { let s = String.sub s 1 ((String.length s )-2) in 
	  add_elt (Num (s, (float_of_string n, dim_of_string d)));
	read lexbuf }
  | "point" space* (string as s) space* (number as n1) space* 
      (dimension as d1) space* ',' space* (number as n2) space* (dimension as d2)
      {let s = String.sub s 1 ((String.length s )-2) in 
	 add_elt (Point (s, 
			(float_of_string n1, dim_of_string d1),
			(float_of_string n2, dim_of_string d2)));
	read lexbuf }
  | _
      { read lexbuf }
  | eof
      { () }

{
  let read_file f =
    let c = open_in f in
    let lb = Lexing.from_channel c in
    elements := [];
    read lb;
    close_in c;
    List.rev !elements
}


