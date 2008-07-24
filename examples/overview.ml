open Mlpost
open Metapost
open Command
open Picture
open Path
open Num
open Num.Infix
open Helpers

let box = Shapes.rounded_rect_path (f 740.) (f 540.) zero zero
let boxpen = Pen.circle ~tr:[Transform.scaled (f 2.5)] ()

let separation = path ~style:JLine [(0., 270.); (0., -270.)]
let sepdash = Dash.scaled 8. Dash.evenly
let frame = [draw ~pen:boxpen box; 
	     draw ~pen:boxpen ~dashed:sepdash  separation]

let ccolor = Color.color "light green"
let acolor = Color.color "blanched almond"

let center_tex ?(scale=2.5) xi xf y p =
  let p = Picture.transform [Transform.scaled (f scale)] p in
  let w = width p in 
  let x = ((f xf -/ f xi) -/ w) // (f 2.) in
    place_up_left p (Point.pt (f xi +/ x, f y))
		     
let coq_side = 
  center_tex (-370.) 0. 250. (tex "{\\it Coq Side}") 
let abstract_side = 
  center_tex 0. 370. 250. (tex "{\\it Abstract Side}")

let texbox ?(fill=ccolor) p =
  Picture.make 
    (draw_box ~fill ~boxed:false
       (Box.round_rect ~dx:(f 10.) ~dy:(f 10.) (Point.pt (zero, zero)) p))
       
let coqform =
  center_tex ~scale:2. (-370.) 0. 160. 
    (texbox
       (tex "\\begin{tabular}{c} Coq Formula \\\\ {\\texttt{$\\Phi$ : Prop}} \
             \\end{tabular}"))

let absform =
  center_tex ~scale:2. 0. 370. 160.
    (texbox ~fill:acolor
       (tex "\\begin{tabular}{c} Abstract Formula \\\\ \
               {\\texttt{$f$ : formula}} \
             \\end{tabular}"))

let proofphi =
  center_tex  ~scale:2. (-370.) 0. (-120.)
    (texbox
       (tex "\\begin{tabular}{c} Proof of $\\Phi$ \\\\ \
               by {\\it reflection} \
             \\end{tabular}"))

let dpll =
  center_tex ~scale:2. 0. 370. 40.
    (Picture.make 
       (draw_box ~boxed:true ~fill:(Color.color "light cyan")
	  (Box.rect ~dx:(f 10.) ~dy:(f 10.) (Point.pt (zero, zero)) 
	     (tex "\\begin{tabular}{c} DPLL Procedure \\\\ \\\\ \
               \\texttt{formula $\\rightarrow$ result} \
             \\end{tabular}"))))
       
let myarrow ?(scale=1.) ?(outd=NoDir) ?(ind=NoDir) place label p1 p2 =
  let p = jointpathk [NoDir, p1, outd; ind, p2, NoDir] [JCurve] in
  let dp = draw_arrow ~pen:boxpen p in
  let lbl = 
    place (Picture.transform [Transform.scaled (f scale)] label) 
      (Point.sub p2 (Point.pt (zero, f 10.))) in
    Command.seq [dp; draw_pic lbl]

let uarrow =
  myarrow ~scale:1.66 ~outd:(Vec Point.down) ~ind:(Vec Point.left)
    place_up_left 
    (texbox ~fill:acolor (tex "\\texttt{UNSAT}"))
    (Point.segment 0.5 (llcorner absform) (lrcorner absform))
    (Point.pt (Point.xpart (llcorner absform) +/ f 30., f (-155.)))

let sarrow =
  myarrow ~scale:1.66 ~outd:(Vec Point.down) ~ind:(Vec Point.right)
    place_up_right
    (texbox ~fill:acolor (tex "\\texttt{SAT}"))
    (Point.segment 0.5 (llcorner absform) (lrcorner absform))
    (Point.pt (Point.xpart (lrcorner absform) -/ f 30., f (-155.)))

let pointe l h w =
  Path.path ~cycle:JLine ~style:JLine
    [(0., 0.); (l, 0.); (l +. h, w/.2.); (l, w); (0., w)]
     
let pointe1 = 
  center (make (draw ~pen:boxpen (pointe 100. 30. 25.)))
    (Point.pt (f 0., f 120.))
let ltac =
  center_tex ~scale:2. (-100.) 100. 170.
    (texbox ~fill:Color.white
       (tex "\\begin{tabular}{c} $\\mathcal{L}$tac \\\\ \
               \\\\ {\\it meta} function \
             \\end{tabular}"))

let pointe2 =
  center (Picture.transform [Transform.rotated 180.] 
	    (make (draw ~pen:boxpen (pointe 100. 30. 25.))))
    (Point.pt (f 0., f (-155.)))
let sound =
  center_tex ~scale:2. (-100.) 100. (-105.)
    (texbox ~fill:Color.white
       (tex "\\begin{tabular}{c} {\\it Soundness} lemma \\\\ \\\\ \
               {\\scriptsize \\texttt{DPLL f = UNSAT $\\rightarrow \\Phi$}}
             \\end{tabular}"))

let trapeze bl d h =
  Path.path ~cycle:JLine ~style:JLine
    [(0., 0.); (bl, 0.); (bl -. d, h); (d, h)]
let counter_model =
  place_up_left (make (draw ~pen:boxpen (trapeze 90. 20. 60.)))
    (Point.pt (Point.xpart (lrcorner absform) -/ f 30., f (-155.)))
let cm =
  let c = 
    Point.segment 0.5 (ulcorner counter_model) (lrcorner counter_model) in
    Picture.center 
      (Picture.transform [Transform.scaled (f 1.3)]
	 (tex "\\begin{tabular}{c} Counter \\\\ Model \\end{tabular}")) c
    

let bigfig =
  (draw_pic pointe1)::(draw_pic pointe2)::
    (draw_pic counter_model)::(draw_pic cm)::
    (draw_pic coqform)::(draw_pic absform)::(draw_pic proofphi)::
    (draw_pic dpll)::(draw_pic coq_side)::(draw_pic abstract_side)::
      uarrow::sarrow::(draw_pic ltac)::(draw_pic sound)::frame  

let fig = 
  [draw_pic (Picture.transform [Transform.scaled (f 0.25)] 
	       (make (seq (List.rev bigfig))))]
