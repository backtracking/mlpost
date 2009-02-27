/* File parser.mly */
%{
  open Fonts_type

let enc = ref None
let slant = ref None
let extend = ref None
let pfab = ref None

let compose tex human =
  match !pfab with
    |None -> (Parsing.parse_error "No pfab");assert false
    |Some v_pfab ->
       let font = {tex_name = tex;
                   human_name = human;
                   enc_name = !enc;
                   pfab_name = v_pfab;
                   slant = !slant;
                   extend = !extend
                  } in
  (slant := None;
  extend := None;
  enc := None;
  pfab := None;font)

%}
%token <float> FLOAT
%token <string> ID IDENC IDPFAB
%token EOL EOF
%token REMAP SLANT EXTEND
%token DQUOTE LESS
%token DEFAULT NONE
%token REENCODEFONT
%type <Fonts_type.font_map list> pdftex_main
%start pdftex_main
%%
/*dvipdfm_main pr:
    dvipdfm_line EOL dvipdfm_main        { pr $1 }
    dvipdfm_line EOF {[$1]}
;

dvipdfm_line:
  ID ID ID ID
;*/


pdftex_main :
  | pdftex_line EOL pdftex_main {$1::$3}
  | pdftex_line EOF {[$1]}
  | EOF              {[]}

;

pdftex_line:
  | ID ID pdftex_options {compose $1 $2}
  | ID pdftex_options {compose $1 $1}

pdftex_options:
  | {}
  | DQUOTE pdftex_options_aux DQUOTE pdftex_options {$2}
  | IDENC pdftex_options                {enc:=Some $1}
  | IDPFAB pdftex_options              {pfab:=Some $1}

pdftex_options_aux:
  |                    {}
  | FLOAT SLANT pdftex_options_aux {slant:=Some $1}
  | FLOAT EXTEND pdftex_options_aux {extend:=Some $1}
  | ID REENCODEFONT pdftex_options_aux {}
