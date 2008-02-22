module F = Format
module T = Transform

type position =
  | Center
  | PLeft
  | PRight
  | PTop
  | PBot
  | UpLeft
  | UpRight
  | LowLeft
  | LowRight

type picture = 
  | Tex of string
(* later in the mlpost module *)

type command = 
  | Draw of Path.t * Color.t option * Pen.t option
  | Label of picture * position * Point.t
  | DotLabel of picture * position * Point.t
  | Loop of int * int * (int -> command)

type t = command list

let label ?(pos=Center) pic point = Label (pic,pos,point)
(* replace later *)
let dotlabel ?(pos=Center) pic point = DotLabel (pic,pos,point)

let draw ?color ?pen t = 
  (* We don't use a default to avoid the output of 
     ... withcolor (0.00red+0.00green+0.00blue) withpen .... 
     for each command in the output file *)
    Draw (t, color, pen)

let iter from until f = Loop (from, until, f)

let tex s = Tex s

let print_position fmt = function
  | Center  -> F.fprintf fmt ""
  | PLeft   -> F.fprintf fmt ".lft"
  | PRight  -> F.fprintf fmt ".rt"
  | PTop    -> F.fprintf fmt ".top"
  | PBot    -> F.fprintf fmt ".bot"
  | UpLeft  -> F.fprintf fmt ".ulft"
  | UpRight -> F.fprintf fmt ".urt"
  | LowLeft -> F.fprintf fmt ".llft"
  | LowRight -> F.fprintf fmt ".lrt"

let print_pic fmt = function
  | Tex s -> F.fprintf fmt "btex %s etex" s

let print_option start printer fmt = function
  | None -> ()
  | Some o -> F.fprintf fmt "%s%a" start printer o

let rec print_command fmt  = function
  | Draw (path, color, pen) ->
      F.fprintf fmt "draw %a%a%a;@\n" Path.print path
        (print_option " withcolor " Color.print) color
        (print_option " withpen " Pen.print) pen
  | Label (pic,pos,p) ->
      F.fprintf fmt "label%a(%a,%a); @\n"
        print_position pos print_pic pic Point.print p
  | DotLabel (pic,pos,p) ->
      F.fprintf fmt "dotlabel%a(%a,%a); @\n"
        print_position pos print_pic pic Point.print p
  | Loop(from,until,cmd) ->
      for i = from to until - 1 do
	print_command fmt (cmd i);
      done

let print i fmt l =
  F.fprintf fmt "beginfig(%d)@\n %a endfig;@." i 
    (fun fmt l -> List.iter (print_command fmt) l)
    l
