open Mlpost
open Command
open Num
open Color
open Box

(*parse <<togglescript>> *)


(*parse <<radar1 *)
let radar1 =
  let pic = 
    Radar.stack 
      ~pen:(Pen.scale (bp 3.) Pen.circle) 
      ~color:[blue;red;green]
      ~label:["first";"second";"third";"fourth";"fifth"] 
      [[3.;4.;5.;6.;4.];
       [6.;5.;2.;1.;1.];
       [1.;7.;2.;4.;5.]]
  in
  draw_pic pic

(*parse >> <<radar2 *)
let radar2 =
  let pics = 
    Radar.compare
      ~pen:(Pen.scale (bp 1.5) Pen.circle) 
      ~color:[lightblue;lightred;lightgreen] ~fill:true
      [[3.;4.;5.;6.;4.];
       [6.;5.;2.;1.;1.];
       [1.;7.;2.;4.;5.]]
  in
  Box.draw (Box.vbox ~padding:(bp 10.) (List.map (Box.pic ~stroke:None) pics))


(*parse >> *)

let _ = 
  List.iter (fun (name,fig) -> Metapost.emit name fig)
  [ "radar1", radar1;
    "radar2", radar2;
  ]
