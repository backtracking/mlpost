
open Format

type name = string

let new_name =
  let r = ref 0 in
  fun () -> incr r; "node" ^ string_of_int !r

type t = 
  | Circle of name * Point.t * Picture.t

let circle c p = 
  Circle (new_name (), c, p)

let center = function
  | Circle (_, c, _) -> c

let declare fmt = function
  | Circle (n, c, p) -> 
      fprintf fmt "circleit.%s(%a);" n Picture.print p;
      fprintf fmt "%s.c = %a;@\n" n Point.print c

let name = function
  | Circle (n, _, _) -> n

