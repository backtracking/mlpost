module C = Command
module P = Picture

type ('a,'b) node = { values : 'b;
                      node : 'a}

type 'a curve = ('a,(float -> float) list) node

type 'a graph = 'a curve list


let curve f node  = { values = [f];
                      node = node}

let curvel lf node  = { values = lf;
                       node = node}


let graph x = x

exception Undefined

let rec calc_one_value x = function
  | [] -> None
  | f::lf -> 
      try Some (f x) with Undefined -> calc_one_value x lf

let calc xmin xmax pitch {values=cf;node=node} =
  let rec aux acc = function
    | x when x<xmin -> (xmin,calc_one_value xmin cf)::acc
    | x when x=xmin -> (x,calc_one_value x cf)::acc
    | x -> aux ((x,calc_one_value x cf)::acc) (x-.pitch) in
  {values=aux [] xmax;node=node}

open MetaPath

let cons_opt x l = match x with None -> l | Some x -> (to_path x)::l

let rec pathn_opt acc current = function
  | [] -> cons_opt current acc
  | v::l -> 
      match v,current with
        | None, _ -> pathn_opt (cons_opt current acc) None l
        | Some v, None -> pathn_opt acc 
            (Some (start (knotn v))) l
        | Some v, Some c -> pathn_opt acc 
            (Some (concat ~style:jLine c (knotn v))) l

let draw_aux ?label values =
  C.seq (
    List.map 
      (fun (values,brush) ->
         let line = pathn_opt [] None values in
         C.seq (List.map (Path.draw ~brush) line)) values)

open Num

let ysep = 10

let rec tick pitch xmax nb =
  let rec aux acc x = function
    | n when n <= 0 -> acc
    | n -> aux (x::acc) (x-.pitch) (n-1) in
  aux [] xmax nb

let rec tick_log xmax =
  let rec aux acc = function
    | x when x > xmax -> acc
    | x -> aux (x::acc) (x*.10.) in
  aux [] 1.

let rec tick_logneg xmin =
  let rec aux acc = function
    | x when x < xmin -> acc
    | x -> aux (x::acc) (x*.10.) in
  aux [] (-.1.)


let vtick = 
  let t = Point.bpp (2.5,0.) in
  fun v ->  
  (Point.sub v t, Point.add v t)

let draw_axes 
    ~logarithmic ~ytick ~xmin ~xmax ~ymin ~ymax ~yzero ~xzero ~pitch =
  let ytick = C.seq ytick in
  let vert = 
    Arrow.simple (Path.pathn ~style:Path.jLine [xzero,ymin;xzero,ymax]) in
  let hori = 
    Arrow.simple (Path.pathn ~style:Path.jLine [xmin,yzero;xmax,yzero]) in
  C.seq [ytick;vert;hori]

let count_max iter =
  let y = ref neg_infinity in
  iter (function 
          | Some x -> y := max !y x
          | None -> ());
  !y

let count_min iter =
  let y = ref infinity in
  iter (function 
          | Some x -> y := min !y x
          | None -> ());
  !y

let filter_opt f l = 
  {l with values = List.map (function 
                              | (x,Some y) as p when f y -> p
                              | (x,_) -> (x,None)) l.values}

let draw ?(logarithmic=false) ?curve_brush
  ?label ?ymin ?ymax ~xmin ~xmax ~pitch ~width ~height graph =
  let values = List.map (calc xmin xmax pitch) graph in
  (* ymin, ymax calculation *)
  let values = match ymin,ymax with
    | None,None -> values
    | _ -> let f = 
        match ymin,ymax with
          | None,None -> assert false
          | Some ymin, None -> (fun f -> f >= ymin)
          | Some ymin, Some ymax -> (fun f -> f >= ymin && f <= ymax)
          | None, Some ymax -> (fun f -> f <= ymax) in
      List.map (filter_opt f) values in
  let yvalues = (fun f -> List.iter 
                   (fun x -> List.iter 
                      (fun x -> f (snd x)) x.values) values) in
  let ymax = match ymax with None -> count_max yvalues | Some ymax -> ymax in
  (*let ymax =  if ymax = 0. then 1. else ymax in *)
  let ymin = match ymin with None -> count_min yvalues | Some ymin -> ymin in
  (*let ymin = if ymin = 0. then -.1. else ymin in  *)
  let ymax = if ymin=ymax then ymin +. 1. else ymax in
    (* scale *)
  let conv = 
    if logarithmic then 
      fun v ->
        if abs_float v < 1. 
        then v 
        else ((log10 (abs_float v)) +. 1.) *. (v/.(abs_float v)) 
    else fun v -> v in
  let scaley = Num.divn height (Num.bp ((conv ymax)-. (conv ymin))) in
  let scalex = Num.divn width (Num.bp (xmax-.xmin)) in
  let scale (x,y) = Num.multn (Num.bp (x-.xmin)) scalex,
              Num.multn (Num.bp ((conv y)-.(conv ymin))) scaley in
  let scale_opt = function 
    | (x,Some y) -> Some (scale (x,y))
    | (x,None) -> None in
  let xzero,yzero = scale (0.,0.) in                   
  (* tick vertical *)
  let ymm = ymax -. ymin in
  (*let xmm = xmax -. xmin in*)
  let ypitchl = 
    if logarithmic then
      let l1 = tick_log ymax in
      let l2 = tick_logneg ymin in
      l1@l2
    else 
      let ypitch = 10.**(floor (log10 (ymm/.(float ysep)))) in
      let ymax2 = ypitch *. (floor (ymax/.ypitch)) in
      let ysep = int_of_float (ymm/.ypitch) in
      tick ypitch ymax2 ysep in

  let ypitchl = (*ymin::ymax::*)ypitchl in
  let ytick = List.map (fun y -> 
                          let p = Point.pt (scale (0.,y)) in
                          let (p1,p2) = (vtick p) in
                          let label = Format.sprintf "{%2.1f}" y in
                          C.seq [
                            C.label ~pos:`West (Picture.tex label) p1;
                            Path.draw (Path.pathp ~style:Path.jLine [p1;p2])])
    ypitchl in
  (* values *)
  let values = List.map (fun x -> 
                           {x with values = List.map scale_opt x.values}
                        ) values in
  (* Brush and legend *)
  let color = Color.color_gen 1. 1. in
  let curve_brush _ = Brush.t () in
  let colors = List.map (fun x -> 
                           let b = curve_brush x.node in
                           let b,c = match Brush.color b with
                             | Some c -> b,c
                             | None -> 
                                 let c = color () in
                                 Brush.t ~color:c ~brush:b (),c in 
                           (b,c,x)) values in
  let legend = 
    match label with
      | None -> C.nop
      | Some label -> 
          let legend = 
            Legend.legend 
              (List.map (fun (_,c,x) -> (c,label x.node)) colors) in
        C.label ~pos:`East legend (Point.pt (scale (xmax,(ymax+.ymin)/.2.))) in
  let values = List.map (fun (b,_,x) -> (x.values,b)) colors in
  let xmin,ymin = scale (xmin,ymin) in
  let xmax,ymax = scale (xmax,ymax) in
  let axes = draw_axes 
    ~logarithmic ~ytick ~xmin ~xmax ~ymin ~ymax ~yzero ~xzero ~pitch in
  C.seq [axes;
         draw_aux ?label values;
        legend]
    
  
