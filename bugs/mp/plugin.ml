open Mlpost
open Box

(* Some custom values *)

let padding = Num.bp 15.

let delta = Num.bp 10.

let unused = "$^\\star$"

let big_title s = "\\textbf{\\Large{" ^ s ^ "}}"

let small_title s = "\\textbf{\\large{" ^ s ^ "}}"

let external_color = Color.rgb8 255 165 0

let internal_color = Color.rgb8 50 205 50

let plugin_color = Color.lightcyan

let make_color = (*Color.rgb8 46 139 97*) Color.rgb8 250 128 114

(* Some very useful functions: should be in some way in the mlpost API? *)

let box_width ?name ?style ?fill ?dy w b =
  box ?name ?style ?fill ?dy ~dx:(Num.divf (Num.subn w (width b)) 2.) b

let box_height ?name ?style ?fill ?dx h b =
  box ?name ?style ?fill ?dx ~dy:(Num.divf (Num.subn h (height b)) 2.) b

let box_hw ?name ?style ?fill h b =
  box ?name ?style ?fill
    ~dx:(Num.divf (Num.subn h (width b)) 2.)
    ~dy:(Num.divf (Num.subn h (height b)) 2.)
    b

let vbox_same_width ?name ?padding ~style l =
  let max = List.fold_left (fun acc b -> Num.maxn acc (width b)) Num.zero l in
  vbox ?name ?padding
    (List.map
       (fun b ->
         box_width max ~style ?fill:(get_fill b) ~dy:Num.zero (clear_stroke b))
       l)

let simulate_box ?name b = empty ?name ~width:(width b) ~height:(height b) ()

let xmed ?(coef = 0.5) p1 p2 =
  Num.multf coef (Num.addn (Point.xpart p1) (Point.xpart p2))

let ymed ?(coef = 0.5) p1 p2 =
  Num.multf coef (Num.addn (Point.ypart p1) (Point.ypart p2))

let med ?xcoef ?ycoef p1 p2 =
  Point.pt (xmed ?coef:xcoef p1 p2, ymed ?coef:ycoef p1 p2)

(* Some special functions for this figure *)

let modul ?(color = external_color) ?same_height ?same_width s =
  let t = tex s in
  match (same_height, same_width) with
  | None, None -> box ~name:s ~fill:color ~style:Rect t
  | Some h, None -> box_height ~name:s ~fill:color ~style:Rect h t
  | None, Some w -> box_width ~name:s ~fill:color ~style:Rect w t
  | Some h, Some w ->
      assert (h = w);
      box_hw ~name:s ~fill:color ~style:Rect w t

(* The figure itself *)

let fig =
  (* special external modules: Makefile.dynamic + Design *)
  let design =
    rect ~fill:external_color ~name:"design"
      (vbox [ tex ("Design" ^ unused); tex "(GUI extension point)" ])
  in
  let std_modul = modul ~same_height:(height design) in
  let dynmake = std_modul ~color:make_color "Makefile.dynamic" in

  (* empty modules *)
  let empty_modules =
    let empty_module =
      std_modul ~color:internal_color ~same_width:(height design) ""
    in
    let points = tex "\\dots" in
    hbox ~padding [ empty_module; points; empty_module ]
  in

  (* Plug-in implem *)
  let title = tex (small_title "Plug-in implementation") in
  let register = std_modul ~color:internal_color "Register" in
  let options = std_modul ~color:internal_color "Options" in
  let b = vbox_same_width ~padding:delta ~style:Rect [ register; options ] in
  let b = vbox ~padding:delta [ title; b; empty_modules ] in
  let implem =
    round_rect ~name:"implem" ~fill:internal_color ~dx:delta ~dy:delta b
  in

  (* Plug-in GUI *)
  let gui = tex (small_title "Plug-in GUI$^\\star$") in
  let gui =
    round_rect ~name:"gui" ~dx:delta
      ~dy:(Num.addn delta (Num.divf padding 2.))
      ~fill:internal_color
      (vbox ~padding:delta [ gui; empty_modules ])
  in

  (* Makefile *)
  let makefile =
    let t = tex "\\large{Makefile}" in
    box_height ~name:"makefile" ~style:RoundRect ~fill:make_color
      (Num.subn (height implem) (Num.addn (height gui) padding))
      t
  in

  (* Makefile + Plug-in GUI *)
  let right_box =
    vbox_same_width ~name:"right" ~padding ~style:RoundRect [ makefile; gui ]
  in

  (* left column *)
  let db = std_modul "Db.Main" in
  let dyn = std_modul ("Dynamic" ^ unused) in
  let journal = std_modul ("Journal" ^ unused) in
  let plugin = std_modul "Plugin" in
  let prj = std_modul ("Project" ^ unused) in
  let typ = std_modul ("Type" ^ unused) in
  let left_box =
    vbox_same_width ~padding ~style:Rect [ db; dyn; plugin; typ; journal; prj ]
  in

  (* setting the components as a matrix *)
  let figure =
    hbox ~padding:(Num.multf 3. padding)
      [
        left_box;
        tabularl ~hpadding:padding ~vpadding:(Num.multf 3. padding)
          [
            [ empty (); dynmake ];
            [
              simulate_box ~name:"ei" implem; simulate_box ~name:"er" right_box;
            ];
            [ empty (); design ];
          ];
      ]
  in
  let getf s = get s figure in

  (* add the Plug-in directory and merge it in the matrix *)
  let nwp = north_west (getf "ei") in
  let sep = south_east (getf "er") in
  let main_box = hbox ~padding [ implem; right_box ] in
  let title = tex (big_title "Plug-in directory") in
  let b = vbox ~padding:delta [ title; main_box ] in
  let directory_box =
    let r = round_rect ~fill:plugin_color ~dx:padding ~dy:delta b in
    center (med nwp sep) r
  in

  (* caption *)
  let caption =
    tabularl ~pos:`Right ~hpadding:delta
      [
        [ tex "\\textbf{Caption:}"; empty () ];
        [ tex "$\\star$"; tex "part not covered in this tutorial" ];
        [
          hbox ~padding [ empty ~name:"c1" (); empty ~name:"c2" () ];
          tex "registration points";
        ];
      ]
  in

  let full_box = vbox ~padding ~pos:`Right [ figure; caption ] in

  let arrow src dst = Helpers.box_arrow src dst in
  let third_arrow coef ?(yscale = 1.) src dst =
    let p1 = Point.yscale (Num.bp yscale) (west src) in
    let p4 =
      (*east dst*)
      (* JS: don't know why it doesn't work *)
      Point.pt (Point.xpart (east left_box), Point.ypart (east dst))
    in
    let x = xmed ~coef p1 p4 in
    let p2 = Point.pt (x, Point.ypart p1) in
    let p3 = Point.pt (x, Point.ypart p4) in
    (* Why [Arrow.draw] does not provide the same result? *)
    Arrow.simple (Path.pathp ~style:Path.jLine [ p1; p2; p3; p4 ])
  in
  let getf s = get s full_box in
  Command.seq
    [
      draw full_box;
      draw directory_box;
      arrow (getf "c1") (getf "c2");
      arrow (get "gui" directory_box) (getf "design");
      arrow (get "makefile" directory_box) (getf "Makefile.dynamic");
      third_arrow 0.32 (get "Options" directory_box) (getf "Plugin");
      third_arrow 0.35
        (get "Register" directory_box)
        (getf ("Dynamic" ^ unused));
      third_arrow 0.35 (get "Register" directory_box) (getf "Db.Main");
      third_arrow 0.4 ~yscale:1.23
        (get "implem" directory_box)
        (getf ("Journal" ^ unused));
      third_arrow 0.4 ~yscale:1.23
        (get "implem" directory_box)
        (getf ("Type" ^ unused));
      third_arrow 0.55 ~yscale:1.5 directory_box (getf ("Project" ^ unused));
    ]

let _ = Metapost.emit "plugin" fig
