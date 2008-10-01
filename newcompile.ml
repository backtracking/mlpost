open Types
open Hashcons

module C = Compiled_types
module D = Duplicate

let nop = C.CSeq []
let (++) c1 c2 =
  match c1,c2 with
    | C.CSeq [], _ -> c2
    | _, C.CSeq [] -> c1
    | _, _ -> C.CSeq [c1 ; c2]

let num_names = D.NM.create 257
let point_names = D.PtM.create 257
let path_names = D.PthM.create 257
let picture_names = D.PicM.create 257

let option_compile f = function
  | None -> None, nop
  | Some obj -> 
      let obj, c = f obj in
        Some obj, c

let rec num' = function
  | F f -> C.F f, nop
  | NXPart p -> 
      let p,c = point p in
      C.NXPart p, c
  | NYPart p ->
      let p,c = point p in
      C.NYPart p, c
  | NAdd(n1,n2) ->
      let n1,c1 = num n1 in
      let n2,c2 = num n2 in
        C.NAdd (n1,n2), c1 ++ c2
  | NSub(n1,n2) ->
      let n1,c1 = num n1 in
      let n2,c2 = num n2 in
        C.NSub (n1,n2), c1 ++ c2
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
  | NLength p ->
      let p,c = path p in
      C.NLength p, c
and num n = 
  match n.node with
  | F f -> C.F f, nop
  | _ ->
      let cnt = try !(D.NM.find D.num_map n) with Not_found -> assert false in
      if cnt >= 2 then num_save n
      else num' n.node
and num_save n =
  try 
    let x = D.NM.find num_names n in
    C.NName x, nop
  with Not_found ->
    let n', code = num' n.node in
    let x = Name.num () in
    let () = D.NM.add num_names n x in
    C.NName x, code ++ C.CDeclNum (x,n')

and point' = function
  | PTPair (f1,f2) -> 
      let f1, c1 = num f1 in
      let f2,c2 = num f2 in 
      C.PTPair (f1,f2), c1 ++ c2
  | PTPointOf (f,p) -> 
      let p, code = path p in
        C.PTPointOf (f,p) , code
  | PTDirectionOf (f,p) -> 
      let p, code = path p in
        C.PTDirectionOf (f,p) , code
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
      let tr, c2 = transform tr in
        C.PTTransformed (p,tr),  c1 ++ c2
and point p =
  match p.node with
  | PTPair _ -> point' p.node
  | _ ->
    let cnt = try !(D.PtM.find D.point_map p) with Not_found -> assert false in
    if cnt >= 2 then point_save p
    else point' p.node
and point_save p = 
  try 
    let x = D.PtM.find point_names p in
    C.PTName x, nop
  with Not_found ->
    let p', code = point' p.node in
    let x = Name.point () in
    let () = D.PtM.add point_names p x in
    C.PTName x, code ++ C.CDeclPoint (x,p')

and knot k =
  match k.Hashcons.node with
    | { knot_in = d1 ; knot_p = p ; knot_out = d2 } ->
        let d1, c1 = direction d1 in
        let p, c2 = point p in
        let d2, c3 = direction d2 in
        (d1,p,d2), c1 ++ c2 ++ c3

and joint j = 
  match j.Hashcons.node with
  | JLine -> C.JLine, nop
  | JCurve -> C.JCurve, nop
  | JCurveNoInflex -> C.JCurveNoInflex, nop
  | JTension (a,b) -> C.JTension (a,b), nop
  | JControls (p1,p2) ->
      let p1,c1 = point p1 in
      let p2,c2 = point p2 in
      C.JControls (p1,p2), c1 ++ c2

and direction d = 
  match d.Hashcons.node with
  | Vec p -> 
      let p, code = point p in
      C.Vec p, code
  | Curl f -> C.Curl f, nop
  | NoDir  -> C.NoDir, nop

and path' = function
  | PACycle (d,j,p) ->
      let d, c1 = direction d in
      let j, c2 = joint j in
      let p, c3 = path p in
      C.PACycle (d,j,p), c1 ++ c2 ++ c3
  | PAConcat (pa,j,p) ->
      let p, c1 = path p in
      let pa, c2 = knot pa in
      let j, c3 = joint j in
      C.PAConcat (pa,j,p), c1 ++ c2 ++ c3
  | PATransformed (p,tr) ->
      let p, c1 = path p in
      let tr, c2 = transform tr in
      (* group transformations, for slightly smaller metapost code *)
      (* this only happens in the Metapost AST, to be able to use
       * path components that already have a name *)
      C.PATransformed(p,tr), c1 ++ c2
  | PACutAfter (p1,p2) ->
      let p1, c1 = path p1 in
      let p2, c2 = path p2 in
      C.PACutAfter (p1,p2), c1 ++ c2
  | PACutBefore (p1,p2) ->
      let p1, c1 = path p1 in
      let p2, c2 = path p2 in
      C.PACutBefore (p1,p2), c1 ++ c2
  | PAAppend (p1,j,p2) ->
      let p1, c1 = path p1 in
      let j, c2 = joint j in
      let p2, c3 = path p2 in
      C.PAAppend (p1,j,p2), c1 ++ c2 ++ c3
  | PABuildCycle pl ->
      let npl = List.map path pl in
      C.PABuildCycle (List.map fst npl), C.CSeq (List.map snd npl)
  | PASub (f1, f2, p) ->
      (* the Metapost subpath command needs a path name as argument *)
      let p', code = path_save p in
      begin
        match p' with
        | C.PAName x -> C.PASub (f1,f2,x), nop
        | _ -> assert false
      end
  | PABBox p ->
      let p, code = picture p in
      C.PABBox p, code
  | PAKnot path ->
      let path, code = knot path in
      C.PAKnot path, code
  | PAUnitSquare -> C.PAUnitSquare, nop
  | PAQuarterCircle -> C.PAQuarterCircle, nop
  | PAHalfCircle -> C.PAHalfCircle, nop
  | PAFullCircle -> C.PAFullCircle, nop
and path p =
  match p.node with
  | PAKnot _ -> path' p.node
  | _ -> 
      let cnt = !(D.PthM.find D.path_map p) in
      if cnt >= 2 then path_save p
      else path' p.node
and path_save p =
  try 
    let x = D.PthM.find path_names p in
    C.PAName x, nop
  with Not_found ->
    let p', code = path' p.node in
    let x = Name.path () in
    let () = D.PthM.add path_names p x in
    C.PAName x, code ++ C.CDeclPath (x,p')

and picture' = function
  | PITransformed (p,tr) ->
      let tr, c1 = transform tr in
      let pic, c2 = picture p in
      C.PITransformed (pic,tr), c1 ++ c2
  | PITex s -> C.PITex s, nop
  | PIMake c -> 
      let pn = Name.picture () in
      C.PIName pn, C.CDefPic (pn, command c)
  | PIClip (pic,pth) -> 
      let pic, c1 = picture_save pic in
      let pth, c2 = path pth in
      let pn = Name.picture () in
      (* slight redundance here *)
      C.PIName pn, c1 ++ c2 ++ C.CSimplePic (pn,pic) ++ C.CClip (pn,pth)
and picture pic = 
  let cnt = !(D.PicM.find D.picture_map pic) in
  if cnt >= 2 then picture_save pic
  else picture' pic.node
and picture_save pic = 
  try 
    let x = D.PicM.find picture_names pic in
    C.PIName x, nop
  with Not_found ->
    let x = Name.picture () in
    let () = D.PicM.add picture_names pic x in
    let pic', code = picture' pic.node in
    C.PIName x, code ++ C.CSimplePic (x,pic')

and transform t = 
  match t.Hashcons.node with
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

and pen p = 
    match p.Hashcons.node with
  | PenCircle -> C.PenCircle, nop
  | PenSquare -> C.PenSquare, nop
  | PenFromPath p -> 
      let p, code = path p in
        C.PenFromPath p, code
  | PenTransformed (p, tr) ->
      let p, c1 = pen p in
      let tr, c2 = transform tr in
        C.PenTransformed (p,tr), c1 ++ c2

and dash d = 
    match d.Hashcons.node with
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

and dash_pattern o = 
    match o.Hashcons.node with
      | On f -> 
	  let f1, c1 = num f in C.On f1, c1
      | Off f -> 
	  let f1, c1 = num f in C.Off f1, c1
	
and command c = 
    match c.Hashcons.node with 
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
  | CDotLabel (pic, pos, pt) -> 
      let pic, c1 = picture pic in
      let pt, c2 = point pt in
        c1 ++ c2 ++ C.CDotLabel (pic,pos,pt)
  | CLabel (pic, pos ,pt) -> 
      let pic, c1 = picture pic in
      let pt, c2 = point pt in
      c1 ++ c2 ++ C.CLabel (pic,pos,pt)

let reset () =
  D.NM.clear D.num_map;
  D.NM.clear num_names;
  D.PtM.clear point_names;
  D.PtM.clear D.point_map;
  D.PthM.clear D.path_map;
  D.PthM.clear path_names;
  D.PicM.clear D.picture_map;
  D.PicM.clear picture_names