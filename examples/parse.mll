{
  open Lexing

  let togglescript = 
    "<script type=\"text/javascript\" src=\"prototype.js\"></script>
<script type=\"text/javascript\">
<!--
    function toggle_visibility(id) {$(id).toggle(); }
    function toggle_css_ident(event,class) 
      {$$(class).invoke('toggle');
       var elt = event.findElement();
       if(elt.textContent.startsWith('\\nhide')){
         elt.textContent = elt.textContent.sub('hide','show');
       }else{
         elt.textContent = elt.textContent.sub('show','hide');
         }
       }
//-->
</script>
<a href=\"javascript::void(0)\"
   onclick=\"toggle_css_ident(event,'.mpost')\"
   >
hide mpost</a>
<a href=\"javascript:void(0)\" 
   onclick=\"toggle_css_ident(event,'.png_cairo')\"
   >
show cairo png</a>
<a href=\"javascript:void(0)\" 
   onclick=\"toggle_css_ident(event,'.ps_cairo')\"
   >
show cairo ps</a>
<a href=\"javascript:void(0)\" 
   onclick=\"toggle_css_ident(event,'.pdf_cairo')\"
   >
show cairo pdf</a>
<a href=\"javascript:void(0)\" 
   onclick=\"toggle_css_ident(event,'.svg_cairo')\"
   >
show cairo svg</a>

"
}

let alpha_lower = ['a'-'z' ]
let alpha_upper = ['A'-'Z']
let alpha = ['a' - 'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = alpha_lower (alpha | digit | '\'' | '_')*
let blank = [' ' '\t' '\n' '\r' ]

rule scan = parse
  | "<<togglescript>>" { Printf.printf "%s" togglescript; scan lexbuf }
  | ">>" { Printf.printf "</p> </div><hr>"; scan lexbuf }
  | "<<" (identifier as i)
      { 
       
        Printf.printf 
"<div class=\"table mpost\" title=\"with mpost : -ps\">\
<img src=\"%s.png\" /></div>" i;
        Printf.printf 
"<div class=\"table png_cairo\" style=\"display:none;\">\
<img title=\"with cairo : -cairo -png\" src=\"png_cairo_%s.png\" /></div>" i;
        Printf.printf 
"<div class=\"table ps_cairo\" style=\"display:none;\">\
<img title=\"with cairo : -cairo -ps\" src=\"ps_cairo_%s.png\" /></div>" i;
        Printf.printf 
"<div class=\"table pdf_cairo\" style=\"display:none;\">\
<img title=\"with cairo : -cairo -pdf\" src=\"pdf_cairo_%s.png\" /></div>" i;
        Printf.printf 
"<div class=\"table svg_cairo\" style=\"display:none;\">\
<object title=\"with cairo : -cairo -svg\" data=\"svg_cairo_%s.svg\" />\
</div>" i;
        Printf.printf "<div style=\"clear:both;\">";
        Printf.printf "<a href=\"javascript:toggle_visibility('%s')\">" i;
        Printf.printf "show/hide code</a>";
        Printf.printf "</div>";
        Printf.printf "<div id=\"%s\" style=\"display:none;\" >" i;
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

