(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  Jean-Christophe Filliatre, Romain Bardou and Francois Bobot           *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2.1, with the special exception on linking            *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

type point = Cairo.point =
    { x : float ; 
      y : float }


let point_of_cm cm = (0.3937 *. 72.) *. cm
let two_pi = 8. *. atan 1.

let create create_surface height width (draw:Cairo.t -> unit) out_file =
  let height = point_of_cm height and width = point_of_cm width in
  let oc = open_out out_file in
  let s = create_surface oc ~width_in_points:width ~height_in_points:height in
  let cr = Cairo.create s in
  draw cr;
  Cairo.surface_finish s;
  close_out oc

let create_ps = create Cairo_ps.surface_create_for_channel

let create_pdf = create Cairo_pdf.surface_create_for_channel

let create_svg = create Cairo_svg.surface_create_for_channel

let draw_control_line cr a b w =
  Cairo.save cr ; begin
    Cairo.set_source_rgb cr 0. 0. 1. ;
    Cairo.set_line_width cr w ;
    Cairo.move_to cr a.x a.y ;
    Cairo.line_to cr b.x b.y ;
    Cairo.stroke cr end ;
  Cairo.restore cr

let draw_point cr col pt  =
  Cairo.save cr ;
  (match col with
    |`Green -> Cairo.set_source_rgba cr 0. 1. 0. 0.5
    |`Yellow -> Cairo.set_source_rgba cr 0. 1. 1. 0.5
    |`Red -> Cairo.set_source_rgba cr 1. 0. 0. 0.5);
  Cairo.new_path cr ;
  Cairo.arc cr 
    pt.x pt.y
    (10. /. 1.25)
    0. two_pi ;
  Cairo.fill cr;
  Cairo.restore cr


let draw_spline cr a b c d =
  Cairo.save cr ;
  begin
    Cairo.move_to cr  a.x a.y ;
    Cairo.curve_to cr 
      b.x b.y 
      c.x c.y 
      d.x d.y ;
    
    Cairo.stroke cr ;
    
    draw_control_line cr a b 2. ;
    draw_control_line cr d c 2. ;
    
    List.iter (draw_point cr `Red) [a;b;c;d];
  end;
  Cairo.restore cr
    
let ribbon = Spline_lib.create 
  {x=110.;y=20.} {x=310.;y=300.}
  {x=10.;y= 310.} {x=210.;y=20.}

let arc = Spline_lib.create
  {x=67.;y=129.} {x=260.;y=256.}
  {x=231.;y=43.} {x=104.;y=47.}


let point_dist_min = {x = 100.;y=100.}

let _ = Spline_lib.inter_depth := 10

let draw cr =
  Cairo.set_line_width cr 10. ;
  (* The first page intersection*)
  Cairo.save cr;
  (* Intersection between the ribbon and the arc on the ribbon *)
  Spline_lib.iter (draw_spline cr) ribbon;
  Spline_lib.iter (draw_spline cr) arc;
  List.iter (draw_point cr `Green)
    (List.map (fun (t,_) -> Spline_lib.abscissa_to_point ribbon t)
       (Spline_lib.intersection ribbon arc));

  Cairo.translate cr 400. 0. ;

  (* Intersection between the ribbon and the arc on the arc *)
  Spline_lib.iter (draw_spline cr) ribbon;
  Spline_lib.iter (draw_spline cr) arc;
  List.iter (draw_point cr `Green)
    (List.map (fun (_,t) -> Spline_lib.abscissa_to_point arc t)
       (Spline_lib.intersection ribbon arc));
  Cairo.restore cr;

  Cairo.show_page cr;

  (* Nearest point between the ribbon (resp. arc) and the point *)
  Spline_lib.iter (draw_spline cr) ribbon;
  Spline_lib.iter (draw_spline cr) arc;
  draw_point cr `Yellow point_dist_min;
  draw_point cr `Green (Spline_lib.abscissa_to_point ribbon
                          (Spline_lib.dist_min_point ribbon point_dist_min));
  draw_point cr `Green (Spline_lib.abscissa_to_point arc
                          (Spline_lib.dist_min_point arc point_dist_min));

  Cairo.show_page cr;
  
  (* Nearest point between the ribbon and two different arc *)
  let ribbon_t1 = Spline_lib.translate ribbon {x=400.;y=0.} in
  let arc_t3 = Spline_lib.translate arc {x=400.;y=250.} in
  Spline_lib.iter (draw_spline cr) ribbon_t1;
  Spline_lib.iter (draw_spline cr) arc;
  Spline_lib.iter (draw_spline cr) arc_t3;
  let (t1,t2) = Spline_lib.dist_min_path ribbon_t1 arc in
  draw_point cr `Green (Spline_lib.abscissa_to_point ribbon_t1 t1);
  draw_point cr `Green (Spline_lib.abscissa_to_point arc t2);
  let (t1,t2) = Spline_lib.dist_min_path ribbon_t1 arc_t3 in
  draw_point cr `Green (Spline_lib.abscissa_to_point ribbon_t1 t1);
  draw_point cr `Green (Spline_lib.abscissa_to_point arc_t3 t2);

  Cairo.show_page cr;

  let arc_t1 = Spline_lib.translate arc {x=400.;y=0.} in
  let arc_t2 = Spline_lib.translate arc {x=0.;y=250.} in
  let ribbon_t2 = Spline_lib.translate ribbon {x=0.;y=250.} in
  Spline_lib.iter (draw_spline cr) ribbon;
  Spline_lib.iter (draw_spline cr) arc;
  let (t1,t2) = (Spline_lib.one_intersection ribbon arc) in
  draw_point cr `Green (Spline_lib.abscissa_to_point ribbon t1);
  Spline_lib.iter (draw_spline cr) (fst (Spline_lib.split arc_t1 t2));
  Spline_lib.iter (draw_spline cr) (snd (Spline_lib.split arc_t2 t2));
  Spline_lib.iter (draw_spline cr) (fst (Spline_lib.split ribbon_t1 t1));
  Spline_lib.iter (draw_spline cr) (snd (Spline_lib.split ribbon_t2 t1));


    Cairo.show_page cr;
    
  let segment1 = Spline_lib.create {x=150.;y=50.} {x=50.;y=150.} {x=150.;y=50.} {x=50.;y=150.} in
  let segment2 = Spline_lib.translate segment1 {y=50.;x=50.} in
  Spline_lib.iter (draw_spline cr) segment1;
  Spline_lib.iter (draw_spline cr) segment2;
  let (t1,t2) = Spline_lib.dist_min_path segment1 segment2 in
  draw_point cr `Green (Spline_lib.abscissa_to_point segment1 t1);
  draw_point cr `Green (Spline_lib.abscissa_to_point segment2 t2);

    Cairo.show_page cr;
  
  let segment1 = Spline_lib.create {x=150.;y=50.} {x=50.;y=150.} {x=150.;y=50.} {x=50.;y=150.} in
  let segment2 = Spline_lib.translate segment1 {y=50.;x=75.} in
  Spline_lib.iter (draw_spline cr) segment1;
  Spline_lib.iter (draw_spline cr) segment2;
  let (t1,t2) = Spline_lib.dist_min_path segment1 segment2 in
  draw_point cr `Green (Spline_lib.abscissa_to_point segment1 t1);
  draw_point cr `Green (Spline_lib.abscissa_to_point segment2 t2);


  Cairo.show_page cr;

(*  let texs = Gentex.create "" ["$\\frac{a}{b}$";"Bonjour";"$\\sqrt{1+2+3}^x_i$"] in*)
  let label = ["Hello World";
               "$\\frac{a^3}{b}$";
               "$A^3_i$";
               "$\\sqrt{1+2+3}^x_i$";
               "\\parbox{3cm}{Haut\\newline Bas}";
               "\\newlength{\\hautw}\\settowidth{\\hautw}{Haut}\\parbox{\\hautw}{Haut\\newline Bas}"
              ] in
  let texs = Gentex.create "" label in
  let box draw_bases tex = 
    let (x_min,y_min,x_max,y_max) = Gentex.get_dimen_pt tex in
    Cairo.save cr;
    Cairo.set_source_rgb cr 0. 0. 1. ;
    Cairo.set_line_width cr 0.5 ;
    Cairo.move_to cr x_min y_min ;
    Cairo.line_to cr x_min y_max ;
    Cairo.line_to cr x_max y_max ;
    Cairo.line_to cr x_max y_min ;
    Cairo.line_to cr x_min y_min ;
    Cairo.stroke cr;
    if draw_bases then
      List.iter (fun x -> 
                   Cairo.set_source_rgb cr 0. 1. 0. ;
                   Cairo.move_to cr x_min x;
                   Cairo.line_to cr x_max x;
                   Cairo.stroke cr) (Gentex.get_bases_pt tex);
    Cairo.restore cr;
    Gentex.draw cr tex in

  Cairo.translate cr 50. 50.;

  let draw_all tx ty =
    Cairo.save cr;
    List.iter (fun tex ->
                 box false tex;
                 Cairo.translate cr tx 0.) texs;
    Cairo.restore cr;
    
    Cairo.save cr;
    Cairo.translate cr 0. ty;
    List.iter (fun tex ->
                 box true tex;
                 Cairo.translate cr tx 0.) texs;
    Cairo.restore cr in

  draw_all 100. 100.;
  Cairo.translate cr 0. 200.;
  Cairo.scale cr 2. 2.;
  draw_all 60. 50.


  
let _ = 
  create_pdf 20. 29.7 draw "testspline_lib.pdf"
