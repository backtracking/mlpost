(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
(*                                                                        *)
(*  This software is free software; you can redistribute it and/or        *)
(*  modify it under the terms of the GNU Library General Public           *)
(*  License version 2, with the special exception on linking              *)
(*  described in file LICENSE.                                            *)
(*                                                                        *)
(*  This software is distributed in the hope that it will be useful,      *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                  *)
(*                                                                        *)
(**************************************************************************)

open Types
module C = Compiled_types

let nop = C.CSeq []
let (++) c1 c2 =
  match c1,c2 with
    | C.CSeq [], _ -> c2
    | _, C.CSeq [] -> c1
    | _, _ -> C.CSeq [c1 ; c2]


module HPic = Hashtbl.Make(struct 
  type t = picture let equal = (==) let hash = Hashtbl.hash 
end)
let known_pictures = HPic.create 17

module HPath = Hashtbl.Make(struct 
  type t = path let equal = (==) let hash = Hashtbl.hash 
end)
let known_paths = HPath.create 17

let option_compile f = function
  | None -> None, nop
  | Some obj -> 
      let obj, c = f obj in
        Some obj, c

let rec num = function
  | F f -> C.F f, nop
  | NXPart p -> 
      let p,c = point p in
      C.NXPart p, c
  | NYPart p ->
      let p,c = point p in
      C.NYPart p, c
  | NAdd (n1,n2) ->
      let n1,c1 = num n1 in
      let n2,c2 = num n2 in
        C.NAdd (n1,n2), c1 ++ c2
  | NMinus (n1,n2) ->
      let n1,c1 = num n1 in
      let n2,c2 = num n2 in
        C.NMinus (n1,n2), c1 ++ c2
  | NMult (n1,n2) ->
      let n1,c1 = num n1 in
      let n2,c2 = num n2 in
        C.NMult (n1,n2), c1 ++ c2
  | NDiv (n1,n2) ->
      let n1,c1 = num n1 in
      let n2,c2 = num n2 in
        C.NDiv (n1,n2), c1 ++ c2
  | NMax (n1,n2) ->
      let n1,c1 = num n1 in
      let n2,c2 = num n2 in
	C.NMax (n1,n2), c1 ++ c2
  | NMin (n1,n2) ->
      let n1,c1 = num n1 in
      let n2,c2 = num n2 in
	C.NMin (n1,n2), c1 ++ c2
  | NGMean (n1,n2) ->
      let n1,c1 = num n1 in
      let n2,c2 = num n2 in
	C.NGMean (n1,n2), c1 ++ c2

and point = function
  | PTPair (f1,f2) -> 
      let f1, c1 = num f1 in
      let f2,c2 = num f2 in 
      C.PTPair (f1,f2), c1 ++ c2
  | PTPointOf (f,p) -> 
      let p, code = path p in
        C.PTPointOf (f,p) , code
  | PTAdd (p1,p2) -> 
      let p1,c1 = point p1 in
      let p2,c2 = point p2 in
        C.PTAdd (p1,p2),  c1 ++ c2
  | PTSub (p1,p2) ->
      let p1,c1 = point p1 in
      let p2,c2 = point p2 in
        C.PTSub (p1,p2),  c1 ++ c2
  | PTMult (f,p) ->
      let f, c1 = num f in
      let p1,c2 = point p in
        C.PTMult (f,p1), c1 ++ c2
  | PTRotated (f,p) ->
      let p1,c1 = point p in
        C.PTRotated (f,p1), c1
  | PTPicCorner (pic, corner) ->
      let pic, code = picture pic in
        C.PTPicCorner (pic, corner) , code
  | PTTransformed (p,tr) ->
      let p, c1 = point p in
      let tr, c2 = transform_list tr in
        C.PTTransformed (p,tr),  c1 ++ c2

and path p =
  let find_name_with_cont k p =
    (* find the name of path [p] if it exists,
     * otherwise go on with continuation [k] *)
    match p with
      | PAName n -> C.PAName n, nop
      | _ -> try
              let n = HPath.find known_paths p in
                C.PAName n, nop
             with Not_found -> k p
  in
  let rec new_name old =
    (* give a new name to the path [old] *)
    let p, code = compile_path' old in
    let n = Name.path () in
    let () = HPath.add known_paths old n in
      C.PAName n, code ++ C.CDeclPath (n,p)

      (* compile [p], if it hasn't a name yet *)
  and compile_path p = find_name_with_cont compile_path' p

      (* compile [p] and give it a name, if it hasn't a name yet *)
  and comp_save_path p = find_name_with_cont new_name p
  and compile_path' = function
    (* compile the argument path ; 
     * use [compile_path] for path components to be able to use
     * path components that already have a name *)
    | PACycle (d,j,p) ->
        let d, c1 = direction d in
        let j, c2 = joint j in
        let p, c3 = compile_path p in
        C.PACycle (d,j,p), c1 ++ c2 ++ c3
    | PAConcat (k,j,p) ->
        let p, c1 = compile_path p in
        let k, c2 = knot k in
        let j, c3 = joint j in
        C.PAConcat (k,j,p), c1 ++ c2 ++ c3
    | PATransformed (p,tr) ->
        let p, c1 = compile_path p in
        let tr, c2 = transform_list tr in
        (* group transformations, for slightly smaller metapost code *)
        (* this only happens in the Metapost AST, to be able to use
         * path components that already have a name *)
        C.pa_transformed p tr, c1 ++ c2
    | PACutAfter (p1,p2) ->
        let p1, c1 = compile_path p1 in
        let p2, c2 = compile_path p2 in
        C.PACutAfter (p1,p2), c1 ++ c2
    | PACutBefore (p1,p2) ->
        let p1, c1 = compile_path p1 in
        let p2, c2 = compile_path p2 in
        C.PACutBefore (p1,p2), c1 ++ c2
    | PAAppend (p1,j,p2) ->
        let p1, c1 = compile_path p1 in
        let j, c2 = joint j in
        let p2, c3 = compile_path p2 in
        C.PAAppend (p1,j,p2), c1 ++ c2 ++ c3
    | PABuildCycle pl ->
        let npl = List.map compile_path pl in
        C.PABuildCycle (List.map fst npl), C.CSeq (List.map snd npl)
    | PASub (f1, f2, p) ->
        (* the Metapost subpath command needs a path name as argument
         * we use comp_save_path here to assure that *)
        let p, code = comp_save_path p in
        begin
          match p with
            | C.PAName n -> C.PASub (f1,f2,n), code
            | _ -> assert false
        end
        (* simple paths are not compiled nor given names *)
    | PABBox p ->
        let p, code = picture p in
        C.PABBox p, code
    | PAKnot k ->
        let k, code = knot k in
        C.PAKnot k, code
    | PAName n -> C.PAName n, nop
    | PAUnitSquare -> C.PAUnitSquare, nop
    | PAQuarterCircle -> C.PAQuarterCircle, nop
    | PAHalfCircle -> C.PAHalfCircle, nop
    | PAFullCircle -> C.PAFullCircle, nop
  in
    comp_save_path p

and knot (d1,p,d2) =
  let d1, c1 = direction d1 in
  let p, c2 = point p in
  let d2, c3 = direction d2 in
    (d1,p,d2), c1 ++ c2 ++ c3

and joint = function
  | JLine -> C.JLine, nop
  | JCurve -> C.JCurve, nop
  | JCurveNoInflex -> C.JCurveNoInflex, nop
  | JTension (a,b) -> C.JTension (a,b), nop
  | JControls (p1,p2) ->
      let p1,c1 = point p1 in
      let p2,c2 = point p2 in
        C.JControls (p1,p2), c1 ++ c2

and direction = function
  | Vec p -> 
      let p, code = point p in
        C.Vec p, code
  | Curl f -> C.Curl f, nop
  | NoDir  -> C.NoDir, nop

and transform = function
  | TRRotated f -> C.TRRotated f, nop
  | TRScaled f -> 
      let f,c = num f in
      C.TRScaled f, c
  | TRSlanted f -> 
      let f,c = num f in
      C.TRSlanted f, c
  | TRXscaled f -> 
      let f,c = num f in
      C.TRXscaled f, c
  | TRYscaled f -> 
      let f,c = num f in
      C.TRYscaled f, c
  | TRShifted p -> 
      let p, code = point p in
        C.TRShifted p, code
  | TRZscaled p -> 
      let p, code = point p in
        C.TRZscaled p, code
  | TRReflect (p1,p2) ->
      let p1, c1 = point p1 in
      let p2, c2 = point p2 in
        C.TRReflect (p1,p2), c1 ++ c2
  | TRRotateAround (p,f) ->
      let p, code = point p in
        C.TRRotateAround (p,f), code
and transform_list l =
  let l1,l2 = List.fold_right
                (fun tr (trl, cl) -> 
                   let tr,c =  transform tr in
                     tr::trl, c::cl ) l ([],[]) in
    l1, C.CSeq l2

and picture =
  let compile_picture pn = function
    | PIName _ -> assert false
    | PIMake c -> C.CDefPic (pn, command c)
    | PITransform (tr,p) ->
        let tr, c1 = transform_list tr in
        let pic, c2 = picture p in
          c1 ++ c2 ++ C.CSimplePic (pn, C.PITransform (tr,pic))
    | PITex s -> C.CSimplePic (pn,C.PITex s)
    | PIClip (pic,pth) ->
        let pic, c1 = picture pic in
        let pth, c2 = path pth in
          (* clip (pic,pth) is compiled to:
             newpic := pic;
             clip newpic to pth;
             *)
          c1 ++ c2 ++ C.CSimplePic (pn,C.PSimPic pic) ++ C.CClip (pn,pth)
  in
  function
  | PIName n -> C.PIName n, nop
  | _ as p ->
       begin
         try
           let pic = HPic.find known_pictures p in
             C.PIName pic, nop
         with Not_found ->
           let pn = Name.picture () in
           let cmd = compile_picture pn p in
             HPic.add known_pictures p pn;
             C.PIName pn, cmd
       end

and pen = function
  | PenCircle -> C.PenCircle, nop
  | PenSquare -> C.PenSquare, nop
  | PenFromPath p -> 
      let p, code = path p in
        C.PenFromPath p, code
  | PenTransformed (p, tr) ->
      let p, c1 = pen p in
      let tr, c2 = transform_list tr in
        C.PenTransformed (p,tr), c1 ++ c2


and dash = function
  | DEvenly -> C.DEvenly, nop
  | DWithdots -> C.DWithdots, nop
  | DScaled (f, d) -> 
      let d,c = dash d in
        C.DScaled (f,d) , c
  | DShifted (p,d) ->
      let p, c1 = point p in
      let d, c2 = dash d in
        C.DShifted (p,d), c1 ++ c2
  | DPattern l -> 
      let l1,l2 = List.fold_right
        (fun pat (patl, cl) -> 
           let pat,c =  dash_pattern pat in
             pat::patl, c::cl ) l ([],[]) in
	C.DPattern l1, C.CSeq l2

and dash_pattern = function
  | On f -> 
      let f1, c1 = num f in C.On f1, c1
  | Off f -> 
      let f1, c1 = num f in C.Off f1, c1

and command = function
  | CDraw (p, color, pe, dsh) ->
      let p, c1 = path p in
      let pe, c2 = (option_compile pen) pe in
      let dsh, c3 = (option_compile dash) dsh in
      C.CSeq [c1; c2; c3; C.CDraw (p, color, pe, dsh)]
  | CDrawArrow (p, color, pe, dsh) ->
      let p, c1 = path p in
      let pe, c2 = (option_compile pen) pe in
      let dsh, c3 = (option_compile dash) dsh in
      C.CSeq [c1; c2; c3; C.CDrawArrow (p, color, pe, dsh)]
  | CDrawPic p ->
      let p, code = picture p in
      C.CSeq [code; C.CDrawPic p]
  | CFill (p, c) ->
      let p, code = path p in
      C.CSeq [code; C.CFill (p, c)]
  | CSeq l ->
      C.CSeq (List.map command l)
  | CLoop (i, j, f) ->
      C.CLoop (i, j, fun k -> command (f k))
  | CDotLabel (pic, pos, pt) -> 
      let pic, c1 = picture pic in
      let pt, c2 = point pt in
        c1 ++ c2 ++ C.CDotLabel (pic,pos,pt)
  | CLabel (pic, pos ,pt) -> 
      let pic, c1 = picture pic in
      let pt, c2 = point pt in
      c1 ++ c2 ++ C.CLabel (pic,pos,pt)
  | CDrawBox (c, b, {bpath = pa; pic = pi}) ->
      let pa, c1 = path pa in
      let pi, c2 = picture pi in
      let path_cmd =
        match b with
        | Boxed -> C.CDraw (pa, None, None, None)
        | Unboxed -> nop
      in
      let box_cmd =
        match c with
          | None -> C.CDrawPic pi
          | Some c -> C.CFill (pa, Some c) ++ C.CDrawPic pi
      in
        C.CSeq [c1;c2; path_cmd ++ box_cmd]


let reset () = 
  HPath.clear known_paths;
  HPic.clear known_pictures;
