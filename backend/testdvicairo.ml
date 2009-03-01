open Format

let ps = ref false
let pdf = ref false
let svg = ref false
let gtk = ref false
let png = ref false
let calc_dim = ref false
let separate_page = ref false
let margin = ref 2.54
let trace = ref false
let debug = ref false

module Saved_device = Dviinterp.Interp(Dev_save.Dev_save)

module Cairo_device = Dev_save.Dev_load(Dvicairo.Cairo_device)

let format_prefix s max = Printf.sprintf "%s%03i" s


let all_output_aux h w x y prefix saved =
  if !ps then
    if !debug then printf "PS generation@.";
    Dvicairo.create_ps h w x y (Cairo_device.replay !trace saved) (prefix^".ps");
  if !pdf then
    if !debug then printf "PDF generation@.";
    Dvicairo.create_pdf h w x y (Cairo_device.replay !trace saved) (prefix^".pdf");
  if !svg then
    if !debug then printf "SVG generation@.";
    Dvicairo.create_svg h w x y (Cairo_device.replay !trace saved) (prefix^".svg");
  if !png then
    if !debug then printf "PNG generation@.";
    Dvicairo.create_png h w x y (Cairo_device.replay !trace saved) (prefix^".png");
  if !gtk then
    if !debug then printf "GTK generation@.";
    Dvicairo.create_gtk h w x y (Cairo_device.replay !trace saved) prefix

let all_output s =
  let prefix = try Filename.chop_extension s 
  with Invalid_argument _ -> s in
  let doc = Dvi.read_file s in
  let saved = Saved_device.load_doc () doc in
    if !separate_page then
      let pages = Dev_save.separe_pages saved in
      let fprefix = format_prefix prefix (Dev_save.nb_pages saved) in
      let count = ref 0 in
      List.iter (
        function page,x_min,y_min,x_max,y_max ->
          let w = x_max -. x_min in
          let h = y_max -. y_min in
          all_output_aux h w (-.x_min) (-.y_min) (incr(count);fprefix !count) page 
      ) pages
    else
      let height = Dvi.get_height_cm doc +. 2. *. !margin in
      let width = Dvi.get_width_cm doc +. 2. *. !margin in
      all_output_aux height width !margin !margin prefix saved
 
  
let options =
  [("-m",Arg.Set_float margin,"Set the margin (cm) around the pages (2.54 cm = 1 in by default)");
   ("--pdf",Arg.Set pdf,"Output in pdf (dviname.pdf)");
   ("--ps",Arg.Set ps,"Output in ps (dviname.pdf)");
   ("--svg",Arg.Set svg,"Output in svg (dviname.svg)");
(*   ("--gtk",Arg.Set gtk,"Output in a gtk window");
   ("--png",Arg.Set gtk,"Output in png window (dviname%d.png)");*)
   (*("--calc_dim",Arg.Set calc_dim,"Don't use the dvi dimension");*)
   ("--separate_page",Arg.Set separate_page,"Generate one file for each page");
   ("--trace",Arg.Set trace,"Trace the bounding box");
   ("-v",Arg.Set debug,"Verbose")
]

let _ = 
  Arg.parse options all_output "Usage :dvicairo [options] <file1.dvi> <file2.dvi> ...\n"
