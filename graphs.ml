open Path
open Helpers
module F = Format
module C = Convenience

let draw1 = 1, 
  [ draw (straight (map_bp [20.,20.; 0.,0.; 0.,30.; 30.,0.; 0.,0.]))]

let z0 = 0.,0.
let z1 = 60.,40.
let z2 = 40.,90.
let z3 = 10.,70.
let z4 = 30.,50.
let l1 = z0::z1::z2::z3::z4::[]

let draw3 = 3,
  [ draw (curved (map_bp l1))]

let draw4a = 104, [ draw (cycle JCurve (curved (map_bp l1)))]
let draw4a' = 104, [ Convenience.draw ~cycle:true l1]

let draw4b = 204, [ draw 
		 (append 
		    (curved (map_bp [z0;z1;z2;z3]))
                    JLine
		    (straight (map_bp [z4;z0])))]
(* no easy alternative way to draw this one, and that's fine *)
let draw4b' =204, [ draw (curved (map_bp [z0;z1;z2;z3]));
		draw (straight (map_bp [z3;z4;z0])) ]

let l1dirs = List.map (fun p -> NoDir,p,NoDir) (map_bp l1)
let lcontrols =
  [(26.8, -1.8), (51.4,14.6);
   (67.1, 61.), (59.8,84.6);
   (25.4, 94.), (10.5,84.5);
   (9.6, 58.8), (18.8,49.6)]

let draw5 = 5,
  [ draw 
      (List.fold_left2 concat (start (List.hd l1dirs))
	 (List.map (fun (p1,p2) -> JControls(bpp p1,bpp p2)) lcontrols)
	 (List.tl l1dirs));
    (** sur ce dessin, il y aussi le tracé "droit" qui suit les points
	de controle, il faudra ajouter les pointillés pour ça. *)
    let hull = 
      List.fold_left2 (fun acc (c1, c2) f -> f::c2::c1::acc) 
	[z0] lcontrols (List.tl l1) in
      Convenience.draw ~style:JLine (List.rev hull)]
      
let draw6 = 6,
  [ draw 
      (path_fold JCurve
	 [NoDir,bpp z0,NoDir;
          NoDir,bpp z1,Vec up;
	  NoDir,bpp z2,Vec left;
	  NoDir,bpp z3,NoDir;
	  NoDir,bpp z4,NoDir])
  ]


let lex = (NoDir,bpp (0.0,0.0),Vec(dir 45.))
let rex a = (Vec(dir (10.*.a)), p (cm 6., bp 0.), NoDir)
let draw7 = 7,
  map_from_to 
    (fun a ->
       draw (concat (start lex) JCurve 
	       (rex (float_of_int (-a))))) 0 9

let draw8 = 8,
  map_from_to
    (fun a ->
       draw (concat (start lex) JCurve 
	       (rex (float_of_int a)))) 0 7

let z0 = p( inch (-1.), bp 0.)
let z1 = p( bp 0., inch 0.2)
let z2 = p(inch 1., bp 0.)
let draw9a = 109,
  [ draw (path_fold JCurve
      [NoDir, z0, Vec up; NoDir, z1, Vec right;
       NoDir, z2, Vec down] ) ]

let draw9b = 209,
  [ draw (path_fold JCurveNoInflex
      [NoDir, z0, Vec up; NoDir, z1, Vec right;
       NoDir, z2, Vec down] ) ]

let u l = 1.5 /. 10. *. l
let z0 = (u (-5.)), 0.
let z1 = (u (-3.)),u 2.
let z2 = (u 3.),u 2.
let z3 = (u 5.),u 0.
let l1 = [z0;z1;z2;z3]

let draw10a = 110, [ C.draw ~scale:C.IN l1 ]
let draw10b = 210, 
  [ draw 
      (concat
         (concat 
            (concat (start (NoDir, inp z0, NoDir)) JCurve (NoDir, inp z1, NoDir)) 
            (JTension(1.3,1.3)) (NoDir, inp z2, NoDir))
         JCurve (NoDir, inp z3, NoDir)) ]

let draw10b' = 210, 
  let jl = [JCurve; JTension(1.3,1.3); JCurve] in
    [ draw
        (List.fold_left2
           (fun acc s p -> concat acc s (NoDir,inp p,NoDir)) 
           (start (NoDir, inp z0, NoDir)) jl [z1;z2;z3]) ]

let draw10c = 310,
  let jl = [JCurve; JTension(1.5,1.0); JCurve] in
    [ draw
        (List.fold_left2
           (fun acc s p -> concat acc s (NoDir,inp p,NoDir)) 
           (start (NoDir, inp z0, NoDir)) jl [z1;z2;z3]) ]

let u l = 1.4 /. 10. *. l
let z0 = u (2.), u (-5.)
let z1 = 0., 0.
let z2 = u 2., u 5.
let cl = [0.; 1.; 2.; infinity]

let draw11 =
  let labels = [111; 211; 311; 411] in
    List.map2
      (fun c lab -> lab,
         [draw
           (path_fold JCurve
              [ (NoDir, inp z0, Curl c);
                (NoDir, inp z1, NoDir);
                (Curl c, inp z2, NoDir) ]) ] )
      cl labels

let figs = [ draw1; draw3; draw4a; draw4a'; draw4b; draw4b'; 
             draw5; draw6; draw7; draw8; draw9a; draw9b;
             draw10a; draw10b; draw10b'; draw10c;] @ draw11

let generate_tex tf tmpl1 tmpl2 l =
  let minipage fmt i tmpl =
    F.fprintf fmt "@[<hov 2>\\begin{minipage}[tb]{0.5\\textwidth}@\n";
    F.fprintf fmt "@[<hov 2>\\begin{center}@\n";
    F.fprintf fmt "\\includegraphics{%s.%i}" tmpl i;
    F.fprintf fmt "@]@\n\\end{center}@\n";
    F.fprintf fmt "@]@\n\\end{minipage}@\n"
  in
  let chan = open_out tf in
  let fmt = F.formatter_of_out_channel chan in
    F.fprintf fmt "\\documentclass[a4paper]{article}@.";
    F.fprintf fmt "\\usepackage[]{graphicx}@.";
    F.fprintf fmt "@[<hov 2>\\begin{document}@.";
    List.iter
      (fun (i,_) ->
        F.fprintf fmt "@\n@[<hov 2>\\begin{figure}[htpb]@\n";
        F.fprintf fmt "\\caption{figure %d}@\n" i;
        minipage fmt i tmpl1;
        minipage fmt i tmpl2;
        F.fprintf fmt "\\label{fig:%d}" i;
        F.fprintf fmt "@]@\n\\end{figure}";
        F.fprintf fmt "@\n \\clearpage@\n"
        ) l ;
    F.fprintf fmt "@]@\n\\end{document}@.";
    close_out chan

let generate_mp fmt l =
  List.iter (fun (i,f) -> print_fig i fmt f) l

let mpostfile = "test.mp"
let texfile = "test.tex"

let _ =
  let ch = open_out mpostfile in
  let fmt = F.formatter_of_out_channel ch in
    generate_mp fmt figs;
    close_out ch;
    generate_tex texfile "manual/manual" "test" figs


