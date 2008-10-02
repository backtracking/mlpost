(**************************************************************************)
(*                                                                        *)
(*  Copyright (C) Johannes Kanig, Stephane Lescuyer                       *)
(*  and Jean-Christophe Filliatre                                         *)
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

open Types
module C = Compiled_types

let nop = C.CSeq []
let (++) c1 c2 =
  match c1,c2 with
    | C.CSeq [], _ -> c2
    | _, C.CSeq [] -> c1
    | _, _ -> C.CSeq [c1 ; c2]


module type COMP =
sig
  type node
  type t = node Hashcons.hash_consed
  type out
  val is_simple : t -> bool
  val name : name -> out
  val new_name : unit -> name
  val declare : name -> out -> C.command

  (* [compile] takes two functions as argument
   * both are intended to be used by [compile] for
   * recursive subcalls instead of calling itself
   * the first function simply checks if its argument has already been defined,
   * and returns that name instead of compiling it
   * the second enforces that a name is given to its argument *)
  val compile : (t -> out * C.command) -> (t -> out * C.command) -> t 
                 -> out * C.command
end

module type OUT =
sig
  type t
  type out
  val compile : t -> out * C.command 
  val reset : unit -> unit
end

module Remember 
  (E : COMP) =
struct
  module HM = Hashtbl.Make(
    struct
      type t = E.t 
      let equal = (==) 
      let hash x = x.Hashcons.hkey
  end)


  type t = E.t
  type out = E.out
 
  let hmap = HM.create 257
  let reset () = HM.clear hmap

  let find_name_with_cont k o =
    (* find the name of object [o] if it exists,
     * otherwise go on with continuation [k] *)
     try
       let n = HM.find hmap o in
         E.name n, nop
     with Not_found -> k o

  let rec new_name old =
    let p, code = compile' old in
    let n = E.new_name () in
    let () = HM.add hmap old n in
      E.name n, code ++ E.declare n p

  (* compile [o], if it hasn't a name yet *)
  and compile o = find_name_with_cont compile' o

  (* compile [o] and give it a name, if it hasn't a name yet *)
  and comp_save o = find_name_with_cont new_name o

  (* compile [o], here we now it hasn't got a name yet *)
  and compile' o = E.compile compile comp_save o
  
  let compile o =
    if E.is_simple o then compile o else comp_save o
end
  
let option_compile f = function
  | None -> None, nop
  | Some obj -> 
      let obj, c = f obj in
        Some obj, c

module rec NumBase : COMP with type node = num_node and type out = C.num =
struct
  type node = num_node
  type t = num
  type out = C.num
  
  let new_name = Name.num
  let is_simple n = 
    match n.Hashcons.node with
      | F _ -> true 
      | _ -> false
  let name n = C.NName n
  let declare n nm = C.CDeclNum (n,nm)

  let compile k _ n = 
    match n.Hashcons.node with
    | F f -> C.F f, nop
    | NXPart p -> 
        let p,c = Point.compile p in
        C.NXPart p, c
    | NYPart p ->
        let p,c = Point.compile p in
        C.NYPart p, c
    | NAdd(n1,n2) ->
        let n1,c1 = k n1 in
        let n2,c2 = k n2 in
          C.NAdd (n1,n2), c1 ++ c2
    | NSub(n1,n2) ->
        let n1,c1 = k n1 in
        let n2,c2 = k n2 in
          C.NSub (n1,n2), c1 ++ c2
    | NMult (n1,n2) ->
        let n1,c1 = k n1 in
        let n2,c2 = k n2 in
          C.NMult (n1,n2), c1 ++ c2
    | NDiv (n1,n2) ->
        let n1,c1 = k n1 in
        let n2,c2 = k n2 in
          C.NDiv (n1,n2), c1 ++ c2
    | NMax (n1,n2) ->
        let n1,c1 = k n1 in
        let n2,c2 = k n2 in
          C.NMax (n1,n2), c1 ++ c2
    | NMin (n1,n2) ->
        let n1,c1 = k n1 in
        let n2,c2 = k n2 in
          C.NMin (n1,n2), c1 ++ c2
    | NGMean (n1,n2) ->
        let n1,c1 = k n1 in
        let n2,c2 = k n2 in
          C.NGMean (n1,n2), c1 ++ c2
    | NLength p ->
        let p,c = Path.compile p in
        C.NLength p, c
end
and PointBase : COMP with type node = point_node and type out = C.point =
struct
  type node = point_node
  type t = point
  type out = C.point
  let new_name = Name.point

  let is_simple p = match p.Hashcons.node with
    | PTPair _ -> true | _ -> false
  let name n = C.PTName n
  let declare n pt = C.CDeclPoint (n,pt)

  let compile k _ p = match p.Hashcons.node with
    | PTPair (f1,f2) -> 
        let f1, c1 = Num.compile f1 in
        let f2,c2 = Num.compile f2 in 
        C.PTPair (f1,f2), c1 ++ c2
    | PTPointOf (f,p) -> 
        let f, c = Num.compile f in
        let p, code = Path.compile p in
        C.PTPointOf (f, p), c ++ code
    | PTDirectionOf (f,p) -> 
        let f, c = Num.compile f in
        let p, code = Path.compile p in
        C.PTDirectionOf (f, p), c ++ code
    | PTAdd (p1,p2) -> 
        let p1,c1 = k p1 in
        let p2,c2 = k p2 in
          C.PTAdd (p1,p2),  c1 ++ c2
    | PTSub (p1,p2) ->
        let p1,c1 = k p1 in
        let p2,c2 = k p2 in
          C.PTSub (p1,p2),  c1 ++ c2
    | PTMult (f,p) ->
        let f, c1 = Num.compile f in
        let p1,c2 = k p in
          C.PTMult (f,p1), c1 ++ c2
    | PTRotated (f,p) ->
        let p1,c1 = k p in
          C.PTRotated (f,p1), c1
    | PTPicCorner (pic, corner) ->
        let pic, code = Picture.compile pic in
          C.PTPicCorner (pic, corner) , code
    | PTTransformed (p,tr) ->
        let p, c1 = k p in
        let tr, c2 = Other.transform tr in
          C.PTTransformed (p,tr),  c1 ++ c2
end
and PathBase : COMP with type node = path_node and type out = C.path =
struct
  type node = path_node
  type t = path
  type out = C.path
  let new_name = Name.path

  let is_simple p = match p.Hashcons.node with
  | PAFullCircle
  | PAHalfCircle
  | PAQuarterCircle
  | PAUnitSquare -> true
      (* DOUTEUX: peuvent etre tres complexe !!! *)
  | PAKnot _
  | PABBox _ -> true
  | _ -> false

  let name n = C.PAName n
  let declare n p = C.CDeclPath (n,p)

  let rec knot k =
    match k.Hashcons.node with
      | { knot_in = d1 ; knot_p = p ; knot_out = d2 } ->
	  let d1, c1 = direction d1 in
	  let p, c2 = Point.compile p in
	  let d2, c3 = direction d2 in
	  (d1,p,d2), c1 ++ c2 ++ c3

  and joint j = 
    match j.Hashcons.node with
    | JLine -> C.JLine, nop
    | JCurve -> C.JCurve, nop
    | JCurveNoInflex -> C.JCurveNoInflex, nop
    | JTension (a,b) -> C.JTension (a,b), nop
    | JControls (p1,p2) ->
        let p1,c1 = Point.compile p1 in
        let p2,c2 = Point.compile p2 in
        C.JControls (p1,p2), c1 ++ c2

  and direction d = 
    match d.Hashcons.node with
    | Vec p -> 
        let p, code = Point.compile p in
        C.Vec p, code
    | Curl f -> C.Curl f, nop
    | NoDir  -> C.NoDir, nop

  and compile k ks p = match p.Hashcons.node with
    (* compile the argument path ; 
     * use [k] for path components to be able to use
     * path components that already have a name *)
    | PACycle (d,j,p) ->
        let d, c1 = direction d in
        let j, c2 = joint j in
        let p, c3 = k p in
        C.PACycle (d,j,p), c1 ++ c2 ++ c3
    | PAConcat (k',j,p) ->
        let p, c1 = k p in
        let k', c2 = knot k' in
        let j, c3 = joint j in
        C.PAConcat (k',j,p), c1 ++ c2 ++ c3
    | PATransformed (p,tr) ->
        let p, c1 = k p in
        let tr, c2 = Other.transform tr in
        (* group transformations, for slightly smaller metapost code *)
        (* this only happens in the Metapost AST, to be able to use
         * path components that already have a name *)
        C.PATransformed(p,tr), c1 ++ c2
    | PACutAfter (p1,p2) ->
        let p1, c1 = k p1 in
        let p2, c2 = k p2 in
        C.PACutAfter (p1,p2), c1 ++ c2
    | PACutBefore (p1,p2) ->
        let p1, c1 = k p1 in
        let p2, c2 = k p2 in
        C.PACutBefore (p1,p2), c1 ++ c2
    | PAAppend (p1,j,p2) ->
        let p1, c1 = k p1 in
        let j, c2 = joint j in
        let p2, c3 = k p2 in
        C.PAAppend (p1,j,p2), c1 ++ c2 ++ c3
    | PABuildCycle pl ->
        let npl = List.map k pl in
        C.PABuildCycle (List.map fst npl), C.CSeq (List.map snd npl)
    | PASub (f1, f2, p) ->
        (* the Metapost subpath command needs a path name as argument
         * we use the ks argument here to assure that *)
        let p, code = ks p in
        begin
          match p with
            | C.PAName n -> C.PASub (f1,f2,n), code
            | _ -> assert false
        end
    | PABBox p ->
        let p, code = Picture.compile p in
        C.PABBox p, code
    | PAKnot k ->
        let k, code = knot k in
        C.PAKnot k, code
    | PAUnitSquare -> C.PAUnitSquare, nop
    | PAQuarterCircle -> C.PAQuarterCircle, nop
    | PAHalfCircle -> C.PAHalfCircle, nop
    | PAFullCircle -> C.PAFullCircle, nop
end
and PicBase : COMP with type node = picture_node and type out = C.picture =
struct
  type node = picture_node
  type t = picture
  type out = C.picture
  let new_name = Name.picture
  let name n = C.PIName n
  let declare n p = C.CSimplePic (n,p)

  let is_simple p = false

  and compile k ks pic = 
    match pic.Hashcons.node with
    | PITransformed (p,tr) ->
        let tr, c1 = Other.transform tr in
        let pic, c2 = k p in
        C.PITransformed (pic,tr), c1 ++ c2
    | PITex s -> C.PITex s, nop
    | PIMake c -> 
        let pn = new_name () in
        C.PIName pn, C.CDefPic (pn, Other.command c)
    | PIClip (pic',pth) -> 
        let pic', c1 = ks pic' in
        let pth, c2 = Path.compile pth in
        let pn = new_name () in
        (* slight redundance here *)
        C.PIName pn, c1 ++ c2 ++ C.CSimplePic (pn,pic') 
        ++ C.CClip (pn,pth)
end
and Num : OUT with type t = num and type out = C.num = Remember (NumBase)
and Point :OUT with type t = point and type out = C.point = Remember (PointBase)
and Path : OUT with type t = path and type out = C.path = Remember (PathBase)
and Picture : OUT with type t = picture and type out = C.picture = 
  Remember (PicBase)
and Other : 
  sig
    val command : command -> C.command
    val transform : transform -> C.transform * C.command
  end = 
struct

  let rec transform t = 
    match t.Hashcons.node with
  | TRRotated f -> C.TRRotated f, nop
  | TRScaled f -> 
      let f,c = Num.compile f in
      C.TRScaled f, c
  | TRSlanted f -> 
      let f,c = Num.compile f in
      C.TRSlanted f, c
  | TRXscaled f -> 
      let f,c = Num.compile f in
      C.TRXscaled f, c
  | TRYscaled f -> 
      let f,c = Num.compile f in
      C.TRYscaled f, c
  | TRShifted p -> 
      let p, code = Point.compile p in
        C.TRShifted p, code
  | TRZscaled p -> 
      let p, code = Point.compile p in
        C.TRZscaled p, code
  | TRReflect (p1,p2) ->
      let p1, c1 = Point.compile p1 in
      let p2, c2 = Point.compile p2 in
        C.TRReflect (p1,p2), c1 ++ c2
  | TRRotateAround (p,f) ->
      let p, code = Point.compile p in
        C.TRRotateAround (p,f), code

and pen p = 
    match p.Hashcons.node with
  | PenCircle -> C.PenCircle, nop
  | PenSquare -> C.PenSquare, nop
  | PenFromPath p -> 
      let p, code = Path.compile p in
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
      let p, c1 = Point.compile p in
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
	  let f1, c1 = Num.compile f in C.On f1, c1
      | Off f -> 
	  let f1, c1 = Num.compile f in C.Off f1, c1
	
and command c = 
    match c.Hashcons.node with 
  | CDraw (p, color, pe, dsh) ->
      let p, c1 = Path.compile p in
      let pe, c2 = (option_compile pen) pe in
      let dsh, c3 = (option_compile dash) dsh in
      C.CSeq [c1; c2; c3; C.CDraw (p, color, pe, dsh)]
  | CDrawArrow (p, color, pe, dsh) ->
      let p, c1 = Path.compile p in
      let pe, c2 = (option_compile pen) pe in
      let dsh, c3 = (option_compile dash) dsh in
      C.CSeq [c1; c2; c3; C.CDrawArrow (p, color, pe, dsh)]
  | CDrawPic p ->
      let p, code = Picture.compile p in
      C.CSeq [code; C.CDrawPic p]
  | CFill (p, c) ->
      let p, code = Path.compile p in
      C.CSeq [code; C.CFill (p, c)]
  | CSeq l ->
      C.CSeq (List.map command l)
  | CDotLabel (pic, pos, pt) -> 
      let pic, c1 = Picture.compile pic in
      let pt, c2 = Point.compile pt in
        c1 ++ c2 ++ C.CDotLabel (pic,pos,pt)
  | CLabel (pic, pos ,pt) -> 
      let pic, c1 = Picture.compile pic in
      let pt, c2 = Point.compile pt in
      c1 ++ c2 ++ C.CLabel (pic,pos,pt)
end

let command = Other.command

let reset () = 
  Num.reset ();
  Path.reset ();
  Point.reset ();
  Picture.reset ()
