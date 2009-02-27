open Format

type preamble = {
  pre_version : int;
  pre_num : int32;
  pre_den : int32;
  pre_mag : int32;
  pre_text : string;
}

type postamble = {
  last_page : int32;
  post_num : int32;
  post_den : int32;
  post_mag : int32;
  post_height : int32;
  post_width : int32;
  post_stack : int;
  post_pages : int;
}

type postpostamble = {
  postamble_pointer : int32;
  post_post_version : int;
}

type font_def = {
  checksum : int32;
  scale_factor : int32;
  design_size : int32;
  area : string;
  name : string;
}

type command =
  | SetChar of int32
  | SetRule of int32 * int32
  | PutChar of int32
  | PutRule of int32 * int32
  | Push
  | Pop
  | Right of int32
  | Wdefault
  | W of int32
  | Xdefault
  | X of int32
  | Down of int32
  | Ydefault
  | Y of int32
  | Zdefault
  | Z of int32
  | FontNum of int32
  | Special of string

type page = {
  counters : int32 array;
  previous : int32;
  commands : command list
}

module Int32Map = 
  Map.Make(struct type t = int32 let compare = Int32.compare end)

type t = {
  preamble : preamble;
  pages : page list;
  postamble : postamble;
  postpostamble : postpostamble;
  font_map : font_def Int32Map.t
}

module Print = struct
  let print_preamble fmt p =
    fprintf fmt "* Preamble :\n";
    fprintf fmt "\tversion number = %d\n" p.pre_version;
    fprintf fmt "\tnumerator/denominator = %ld/%ld\n" 
      p.pre_num p.pre_den;
    fprintf fmt "\tmagnification = %ld\n" p.pre_mag;
    fprintf fmt "\tcomment : %s\n" p.pre_text

  let print_font k fmt f =
    fprintf fmt "\tFont number %ld (%s in directory [%s]) :\n"
      k f.name f.area;
    fprintf fmt "\t Checksum = %lx\n" f.checksum;
    fprintf fmt "\t Scale factor / Design size : %ld / %ld\n" 
      f.scale_factor f.design_size

  let print_page fmt 
      {counters = c; previous = prev; commands = cmds} =
    fprintf fmt "* Page number :";
    Array.iter (fun c -> fprintf fmt "%ld;" c) c; fprintf fmt "\n";
    fprintf fmt "\tPrevious page can be found at %ld\n" prev;
    fprintf fmt "\t<list of commands skipped ...>"
      
  let print_pages fmt =
    List.iter (fun p -> fprintf fmt "%a\n" print_page p)

  let print_fonts fmt fonts =
    fprintf fmt "* Fonts defined in this file :\n";
    Int32Map.iter (fun k f -> print_font k fmt f) fonts

  let print_postamble fmt p =
    fprintf fmt "* Postamble :\n";
    fprintf fmt "\tlast page = %ld\n" p.last_page;
    fprintf fmt "\tnumerator/denominator = %ld/%ld\n" 
      p.post_num p.post_den;
    fprintf fmt "\tmagnification = %ld\n" p.post_mag;
    fprintf fmt "\theight - width = %ld - %ld\n" 
      p.post_height p.post_width;
    fprintf fmt "\tmaximum stack depth = %d\n"  p.post_stack;
    fprintf fmt "\ttotal # of pages = %d\n"  p.post_pages  

  let print_postpostamble fmt p =
    fprintf fmt "* Postpostamble :\n";
    fprintf fmt "\tPostamble can be found at %ld.\n" p.postamble_pointer;
    fprintf fmt "\tDVI version : %d\n" p.post_post_version
      
  let print_doc name fmt doc =
    fprintf fmt "***********************\n";
    fprintf fmt "Reading DVI file : %s\n" name;
    fprintf fmt "%a%a%a%a%a"
      print_preamble doc.preamble
      print_pages doc.pages
      print_fonts doc.font_map
      print_postamble doc.postamble
      print_postpostamble doc.postpostamble

end

exception DviError of string ;;
let dvi_error s = raise (DviError s)

let preamble bits = 
  bitmatch bits with
    | { 247 : 8;              (* Preamble opcode *)
	version : 8;          (* DVI version *)
	num : 32 : bigendian; (* numerator *)
	den : 32 : bigendian; (* denominator *)
	mag : 32 : bigendian; (* magnification *)
	k : 8;                (* size of string x *)
	x : 8*k : string;     (* file comment *)
	bits : -1 : bitstring
      } ->
	{ pre_version = version;
	  pre_num = num; pre_den = den; pre_mag = mag;
	  pre_text = x
	}, bits
    | { _ : -1 : bitstring } ->
	dvi_error "Ill-formed preamble"

let add_font k font map = 
  if Int32Map.mem k map then
    dvi_error "Redefinition of font not allowed"
  else
    Int32Map.add k font map

let font_def bits =
  bitmatch bits with
    | { checksum : 32 : bigendian; (* checksum of the TMF file *)
	scale : 32 : bigendian;    (* scale factor *)
	design : 32 : bigendian;   (* design size *)
	a : 8;                     (* size of the area *)
	l : 8;                     (* size of the filename *)
	name : (a+l)*8 : string;   (* the full name w/ area *)
	bits : -1 : bitstring
      } ->
(* 	fprintf fmt " on est passé dans une déf de fonte %s\n" name; *)
	{ checksum = checksum;
	  scale_factor = scale;
	  design_size = design;
	  area = String.sub name 0 a;
	  name = String.sub name a l;
	}, bits
    | { _ : -1 : bitstring } ->
	dvi_error "Ill_formed font definition"

let page_counters bits =
  bitmatch bits with
    | { c0 : 32 : bigendian;
	c1 : 32 : bigendian;
	c2 : 32 : bigendian;
	c3 : 32 : bigendian;
	c4 : 32 : bigendian;
	c5 : 32 : bigendian;
	c6 : 32 : bigendian;
	c7 : 32 : bigendian;
	c8 : 32 : bigendian;
	c9 : 32 : bigendian;
	prev : 32 : bigendian;
	bits : -1 : bitstring
      } ->
	[| c0; c1; c2; c3; c4; c5; c6; c7; c8; c9 |], prev, bits
    | { _ : -1 : bitstring } ->
	dvi_error "Ill-formed counters after bop"

let signed i j unsigned =
  if Int32.zero = Int32.logand unsigned i then
    unsigned
  else
    Int32.logor unsigned j

let signed_8 = signed (Int32.shift_left Int32.one 23) (Int32.logxor Int32.minus_one (Int32.of_int 0xff))
let signed_16 = signed (Int32.shift_left Int32.one 15) (Int32.logxor Int32.minus_one (Int32.of_int 0xffff))
let signed_24 = signed (Int32.shift_left Int32.one 23) (Int32.logxor Int32.minus_one (Int32.of_int 0xffffff))
	
let command bits = 
  bitmatch bits with
      (* Setting Characters *)
    | { k : 8 ; bits : -1 : bitstring } when 0 <= k && k <= 127 ->
	SetChar (Int32.of_int k), bits
    | { 128 : 8; k : 8; bits : -1 : bitstring } ->
	SetChar (Int32.of_int k), bits
    | { 129 : 8; k : 16; bits : -1 : bitstring } ->
	SetChar (Int32.of_int k), bits
    | { 130 : 8; k : 24; bits : -1 : bitstring } ->
	SetChar (Int32.of_int k), bits
    | { 131 : 8; k : 32; bits : -1 : bitstring } ->
	SetChar k, bits
      (* Setting a Rule *)
    | { 132 : 8; a : 32; b: 32; bits : -1 : bitstring } ->
	SetRule(a, b), bits
      (* Putting Characters *)
    | { 133 : 8; k : 8; bits : -1 : bitstring } ->
	PutChar (Int32.of_int k), bits
    | { 134 : 8; k : 16; bits : -1 : bitstring } ->
	PutChar (Int32.of_int k), bits
    | { 135 : 8; k : 24; bits : -1 : bitstring } ->
	PutChar (Int32.of_int k), bits
    | { 136 : 8; k : 32; bits : -1 : bitstring } ->
	PutChar k, bits
      (* Putting a Rule *)
    | { 137 : 8; a : 32; b: 32; bits : -1 : bitstring } ->
	PutRule(a, b), bits
      (* Stack operations *)
    | { 141 : 8; bits : -1 : bitstring } ->
	Push, bits
    | { 142 : 8; bits : -1 : bitstring } ->
	Pop, bits
      (* Moving to the right *)
          (* Must be signed but bitstring 2.0.0 fails*)
    | { 143 : 8; b : 8 ; bits : -1 : bitstring } ->
	Right (signed_8 (Int32.of_int b)), bits
    | { 144 : 8; b : 16 ; bits : -1 : bitstring } ->
	Right (signed_16 (Int32.of_int b)), bits
    | { 145 : 8; b : 24 ; bits : -1 : bitstring } ->
	Right (signed_24 (Int32.of_int b)), bits
    | { 146 : 8; b : 32 ; bits : -1 : bitstring } ->
	Right b, bits
      (* Moving/spacing to the right w *)
    | { 147 : 8; bits : -1 : bitstring } ->
	Wdefault, bits
    | { 148 : 8; b : 8; bits : -1 : bitstring } ->
	W (signed_8 (Int32.of_int b)), bits
    | { 149 : 8; b : 16; bits : -1 : bitstring } ->
	W (signed_16 (Int32.of_int b)), bits
    | { 150 : 8; b : 24; bits : -1 : bitstring } ->
	W (signed_24 (Int32.of_int b)), bits
    | { 151 : 8; b : 32; bits : -1 : bitstring } ->
	W b, bits
      (* Moving/spacing to the right x *)
    | { 152 : 8; bits : -1 : bitstring } ->
	Xdefault, bits
    | { 153 : 8; b : 8; bits : -1 : bitstring } ->
	X (signed_8 (Int32.of_int b)), bits
    | { 154 : 8; b : 16; bits : -1 : bitstring } ->
	X (signed_16 (Int32.of_int b)), bits
    | { 155 : 8; b : 24; bits : -1 : bitstring } ->
	X (signed_24 (Int32.of_int b)), bits
    | { 156 : 8; b : 32; bits : -1 : bitstring } ->
	X b, bits
      (* Moving down *)
    | { 157 : 8; a : 8; bits : -1 : bitstring } ->
	Down (signed_8 (Int32.of_int a)), bits
    | { 158 : 8; a : 16; bits : -1 : bitstring } ->
	Down (signed_16 (Int32.of_int a)), bits
    | { 159 : 8; a : 24; bits : -1 : bitstring } ->
	Down (signed_24 (Int32.of_int a)), bits
    | { 160 : 8; a : 32; bits : -1 : bitstring } ->
	Down a, bits
      (* Moving/spacing down y *)
    | { 161 : 8; bits : -1 : bitstring } ->
	Ydefault, bits
    | { 162 : 8; a : 8; bits : -1 : bitstring } ->
	Y (signed_8 (Int32.of_int a)), bits
    | { 163 : 8; a : 16; bits : -1 : bitstring } ->
	Y (signed_16 (Int32.of_int a)), bits
    | { 164 : 8; a : 24; bits : -1 : bitstring } ->
	Y (signed_24 (Int32.of_int a)), bits
    | { 165 : 8; a : 32; bits : -1 : bitstring } ->
	Y a, bits
      (* Moving/spacing down z *)
    | { 166 : 8; bits : -1 : bitstring } ->
	Zdefault, bits
    | { 167 : 8; a : 8; bits : -1 : bitstring } ->
	Z (signed_8 (Int32.of_int a)), bits
    | { 168 : 8; a : 16; bits : -1 : bitstring } ->
	Z (signed_16 (Int32.of_int a)), bits
    | { 169 : 8; a : 24; bits : -1 : bitstring } ->
	Z (signed_24 (Int32.of_int a)), bits
    | { 170 : 8; a : 32; bits : -1 : bitstring } ->
	Z a, bits
      (* Setting Fonts *)
    | { k : 8 ; bits : -1 : bitstring } when 171 <= k && k <= 234 ->
	FontNum (Int32.of_int (k-171)), bits
    | { 235 : 8; k : 8; bits : -1 : bitstring } ->
	FontNum (Int32.of_int k), bits
    | { 236 : 8; k : 16; bits : -1 : bitstring } ->
	FontNum (Int32.of_int k), bits
    | { 237 : 8; k : 24; bits : -1 : bitstring } ->
	FontNum (Int32.of_int k), bits
    | { 238 : 8; k : 32; bits : -1 : bitstring } ->
	FontNum k, bits
      (* Special Commands *)
    | { 239 : 8; k : 8; x : k * 8 : string; bits : -1 : bitstring } ->
	Special x, bits
    | { 240 : 8; k : 16; x : k * 8 : string; bits : -1 : bitstring } ->
	Special x, bits
    | { 241 : 8; k : 24; x : k * 8 : string; bits : -1 : bitstring } ->
	Special x, bits
    | { 242 : 8; k : 32; x : (Int32.to_int k) * 8 : string; 
	bits : -1 : bitstring } ->
	Special x, bits
    | { _ : -1 : bitstring } ->
	dvi_error "bad command !"

let rec page commands fonts bits =
  bitmatch bits with
    | { 140 : 8; bits : -1 : bitstring } -> (* End of page opcode *)
	commands, fonts, bits
      (* nop opcode *) 
    | { 138 : 8; bits : -1 : bitstring } ->
	page commands fonts bits
      (* font definitions *) 
    | { 243 : 8; k : 8; bits : -1 : bitstring } ->
	let font, bits = font_def bits in
	  page commands (add_font (Int32.of_int k) font fonts) bits
    | { 244 : 8; k : 16 : bigendian; bits : -1 : bitstring } ->
	let font, bits = font_def bits in
	  page commands (add_font (Int32.of_int k) font fonts) bits
    | { 245 : 8; k : 24 : bigendian; bits : -1 : bitstring } ->
	let font, bits = font_def bits in
	  page commands (add_font (Int32.of_int k) font fonts) bits
    | { 246 : 8; k : 32 : bigendian; bits : -1 : bitstring } ->
	let font, bits = font_def bits in
	  page commands (add_font k font fonts) bits
      (* normal command *)
    | { bits : -1 : bitstring } ->
	let cmd, bits = command bits in
	  page (cmd::commands) fonts bits

let rec pages p fonts bits =
  bitmatch bits with
      (* nop opcode *) 
    | { 138 : 8; bits : -1 : bitstring } ->
	pages p fonts bits
      (* font definitions *) 
    | { 243 : 8; k : 8; bits : -1 : bitstring } ->
	let font, bits = font_def bits in
	  pages p (add_font (Int32.of_int k) font fonts) bits
    | { 244 : 8; k : 16 : bigendian; bits : -1 : bitstring } ->
	let font, bits = font_def bits in
	  pages p (add_font (Int32.of_int k) font fonts) bits
    | { 245 : 8; k : 24 : bigendian; bits : -1 : bitstring } ->
	let font, bits = font_def bits in
	  pages p (add_font (Int32.of_int k) font fonts) bits
    | { 246 : 8; k : 32 : bigendian; bits : -1 : bitstring } ->
	let font, bits = font_def bits in
	  pages p (add_font k font fonts) bits
      (* begin page opcode *)
    | { 139 : 8; bits : -1 : bitstring } ->
	let counters, previous, bits = page_counters bits in
	let cmds, fonts, bits = page [] fonts bits in
	let newp = 
	  {counters = counters; previous = previous; commands = cmds} in
        (* Pages in reverse order *)
	  pages (newp::p) fonts bits
    | { bits : -1 : bitstring } ->
	p, fonts, bits
	(* dvi_error "Expected : nop, font_definition, or new page" *)

let postamble bits = 
  let rec skip_font_defs bits =
    bitmatch bits with
	(* nop opcode *) 
      | { 138 : 8; bits : -1 : bitstring } ->
	  skip_font_defs bits
        (* font definitions *) 
      | { 243 : 8; k : 8; bits : -1 : bitstring } ->
	  let _, bits = font_def bits in skip_font_defs bits
      | { 244 : 8; k : 16 : bigendian; bits : -1 : bitstring } ->
	  let _, bits = font_def bits in skip_font_defs bits
      | { 245 : 8; k : 24 : bigendian; bits : -1 : bitstring } ->
	  let _, bits = font_def bits in skip_font_defs bits
      | { 246 : 8; k : 32 : bigendian; bits : -1 : bitstring } ->
	  let _, bits = font_def bits in skip_font_defs bits
      | { bits : -1 : bitstring } ->
	  bits
  in
  bitmatch bits with
    | { 248 : 8;                    (* Postamble opcode *)
	last_page : 32 : bigendian; (* DVI version *)
	num : 32 : bigendian;       (* numerator *)
	den : 32 : bigendian;       (* denominator *)
	mag : 32 : bigendian;       (* magnification *)
	height : 32 : bigendian;    (* tallest page *)
	width : 32 : bigendian;     (* widest page *)
	stack : 16 : bigendian;     (* stack depth *)
	pages : 16 : bigendian;     (* number of pages *)
	bits : -1 : bitstring
      } ->
	{ last_page = last_page;
	  post_num = num; post_den = den; post_mag = mag;
	  post_height = height; post_width = width;
	  post_stack = stack; post_pages = pages 
	}, skip_font_defs bits
    | { _ : -1 : bitstring } ->
	dvi_error "Ill-formed postamble"

let postpostamble bits =
  let rec read_223 bits =
    bitmatch bits with
      | { 223 : 8;
	  rest : -1 : bitstring
	} ->
	  read_223 rest
      | { rest : -1 : bitstring } ->
	  if Bitstring.bitstring_length rest = 0 then ()
	  else dvi_error "Ill-formed suffix : only 223 expected."
  in
    bitmatch bits with
      | { 249 : 8;
	  postamble_pointer : 32 : bigendian;
	  version : 8;
	  rest : -1 : bitstring
	} ->
	  read_223 rest;
	  { postamble_pointer = postamble_pointer;
	    post_post_version = version
	  }
      | { _ : -1 : bitstring } ->
	  dvi_error "ill-formed postpostamble"

let read_file file =
  let bits = Bitstring.bitstring_of_file file in
  let preamble, bits = preamble bits in
  let pages, fonts, bits = pages [] Int32Map.empty bits in
  let postamble, bits = postamble bits in
  let postpostamble = postpostamble bits in
    { preamble = preamble;
      pages = List.rev pages;
      postamble = postamble;
      postpostamble = postpostamble;
      font_map = fonts
    }
