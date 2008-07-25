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

(** This is Mlpost *)

(** {2 Interfaces to basic Metapost datatypes} *)

module rec Num : sig

  (** The Mlpost Num module *)

  type t
      (** The Mlpost numeric type is an abstract datatype *)
      
  (** {2 Conversion functions} *)
  (** The base unit in Mlpost are bp. The following functions 
      permit to specify values in other common units *)

  val f : float -> t
  val bp : float -> t
  val pt : float -> t
  val cm : float -> t
  val mm : float -> t
  val inch : float -> t

  val bpn : t -> t
  val ptn : t -> t
  val cmn : t -> t
  val mmn : t -> t
  val inchn : t -> t

  val addn : t -> t -> t
  val subn : t -> t -> t
  val multn : t -> t -> t
  val multf : float -> t -> t
  val divn : t -> t -> t
  val maxn : t -> t -> t
  val minn : t -> t -> t
  val gmean : t -> t -> t

  (** Infix operators for [addn], [subn], [muln] and [divn] *)

  module Infix : sig
    val (+/) : t -> t -> t
    val (-/) : t -> t -> t
    val ( */) : t -> t -> t
    val ( *./): float -> t -> t
    val (//) : t -> t -> t
  end

  (** {2 Useful constants and functions} *)

  val zero : t
  val one : t
  val two : t
  (** Shortcuts for [bp 0.], [bp 1.] and [bp 2.]. *)

  val pi : float
  val deg2rad : float -> float
  (** Converts degrees into radians *)

  type scale = float -> t

  module Scale : sig
    val bp : float -> scale
    val pt : float -> scale
    val cm : float -> scale
    val mm : float -> scale
    val inch : float -> scale
  end
end

and Point : sig

  (** The abstract type for points *)
  type t

  (**  Construct a point from two numeric values *)
  val pt : Num.t * Num.t -> t

  (** The following functions create points of length 1. 
      They are especially useful to specify directions with [Path.Vec] *)

  (** [dir f] is the point at angle [f] on the unit circle. 
      [f] shall be given in degrees *)
  val dir : float -> t
    
  (** The unitary vectors pointing up, down, left and right *)

  val up : t
  val down : t
  val left : t
  val right : t

  val origin : t

  val length : t -> Num.t
  (** [length p] is the length of vector from the origin to [p] *)

  (** [xpart p] is the x coordinate of point [p] *)
  val xpart : t -> Num.t

  (** [ypart p] is the y coordinate of point [p] *)
  val ypart : t -> Num.t

  (** {2 Operations on points} *)
    
  (** Apply a transformation to a point *)
  val transform : Transform.t -> t -> t

  (** [segment f p1 p2] is the point [(1-f)p1 + fp2] *)
  val segment : float -> t -> t -> t

  (** Sum two points *)
  val add : t -> t -> t
  
  (** Substract two points *)
  val sub : t -> t -> t
  
  (** Multiply a point by a scalar *)
  val mult : Num.t -> t -> t
  
  (** Rotate a point by an angle in degrees *)
  val rotated : float -> t -> t

  (** [rotate_around p1 f p2] rotates [p2] around [p1] by an angle [f] 
      in degrees *)
  val rotate_around : t -> float -> t -> t

  (** Scales the X coordinate of a point by a scalar *)
  val xscaled : Num.t -> t -> t

  (** Scales the Y coordinate of a point by a scalar *)
  val yscaled : Num.t -> t -> t

  (** {2 Convenient constructors} *)

  (** The following functions build a point at a 
      given scale (see {!Num.t} for scales) *)

  val bpp : float * float -> t
  val inp : float * float -> t
  val cmp : float * float -> t
  val mmp : float * float -> t
  val ptp : float * float -> t

  (** Same as the previous functions but build list of points *)

  val map_bp : (float * float) list -> t list
  val map_in: (float * float) list -> t list
  val map_cm: (float * float) list -> t list
  val map_mm: (float * float) list -> t list
  val map_pt: (float * float) list -> t list

  (** Builds a point from a pair of floats
      @param scale a scaling function to be applied to each float;
      see {!Num.t} for scaling functions for usual units *)
  val p : ?scale:(float -> Num.t) -> float * float -> t

  (** Same as [p], but builds a list of points *)
  val ptlist : ?scale:(float -> Num.t) -> (float * float) list -> t list

end

and Path : sig

  (** Paths are the objects used to describe lines, curves, and 
      more generally almost everything that is drawn with Mlpost *) 

  (** A [direction] is used to put constraints on paths:
      {ul {- [Vec p] defines a direction by a point (interpreted as a vector)}
      {- [Curl f] changes the curling factor of the extremity of a path; 
      higher curling factor means flatter curves}
      {- [NoDir] means no particular direction} } *)
  type direction =
      | Vec of Point.t
      | Curl of float
      | NoDir 

  (** A [knot] is the basic element of a path, and is simply a point 
      with an incoming and outgoing direction constraint *)
  type knot = direction * Point.t * direction

  (** A joint is the connection between two knots in a path. It is either
      {ul {- [JLine] for a straight line}
      {- [JCurve] for a spline curve}
      {- [JCurveNoInflex] to avoid inflexion points}
      {- [JTension] to specify "tension" on the joint; [JCurve] uses a default
      tension of 1. Higher tension means less "wild" curves}
      {- [JControls] to explicitely specify control points}} *)
  type joint =
      | JLine
      | JCurve
      | JCurveNoInflex
      | JTension of float * float
      | JControls of Point.t * Point.t

  (** The abstract type of paths *)
  type t

  (** {2 Labelled path constructors} *)
  (** Build a knot from a pair of floats
      @param l an incoming direction
      @param r an outgoing direction 
      @param scale a scaling factor applied to the floats *)
  val knot :
    ?l:direction -> ?r:direction -> 
    ?scale:(float -> Num.t) -> float * float -> knot

  (** Build a knot from a Num.t pair; the optional arguments are as in {!knot} *)
  val knotn : ?l:direction -> ?r:direction -> Num.t * Num.t -> knot

  (** Build a knot from a point; the optional arguments are as in {!knot} *)
  val knotp :
    ?l:direction -> ?r:direction -> Point.t -> knot

  (** Build a path from a list of pairs of floats
      @param style the joint style used for all joints in the path
      @param cycle if given, the path is closed using the given style
      @param scale permits to scale the whole path *)
  val path : 
    ?style:joint -> ?cycle:joint -> ?scale:(float -> Num.t) -> 
    (float * float) list -> t

  (** Same as [path], but uses a [Num.t] list *)
  val pathn : 
    ?style:joint -> ?cycle:joint -> (Num.t * Num.t) list -> t

  (** Same as [path], but uses a knot list *)
  val pathk :
    ?style:joint -> ?cycle:joint -> knot list -> t

  (** Same as [path] but uses a point list *)
  val pathp : ?style:joint -> ?cycle:joint -> Point.t list -> t

  (** Build a path from [n] knots and [n-1] joints *)
  val jointpathk : knot list -> joint list -> t

  (** Build a path from [n] points and [n-1] joints, 
      with default directions *)
  val jointpathp : Point.t list -> joint list -> t

  val jointpathn : (Num.t * Num.t) list -> joint list -> t

  (** Build a path from [n] float_pairs and [n-1] joints, 
      with default directions *)
  val jointpath : 
    ?scale:(float -> Num.t) -> (float * float) list -> 
    joint list -> t

  (** Close a path using direction [dir] and style [style] *)
  val cycle : ?dir:direction -> ?style:joint -> t -> t


  (** {2 Primitive path constructors} *)
  (** Add a knot at the end of a path  *)
  val concat : ?style:joint -> t -> knot -> t

  (** Create a simple path with one knot *)
  val start : knot -> t

  (** Append a path to another using joint [style] *)
  val append : ?style:joint -> t -> t -> t


  (** {2 More complex constructions on paths} *)

  (** [point f p] returns a certain point on the path [p]; [f] is
      given "in control points": [1.] means the first control point,
      [2.] the second and so on; intermediate values are accepted. *)
  val point : float -> t -> Point.t

  (** [subpath start end path] selects the subpath of [path] that lies
      between [start] and [end]. [start] and [end] are given in
      control points, as in {!point}. *)
  val subpath : float -> float -> t -> t

  (** Apply a transformation to a path *)
  val transform : Transform.t -> t -> t

  val scale : Num.t -> t -> t
  val rotate : float -> t -> t
  val shift : Point.t -> t -> t
  val yscale : Num.t -> t -> t
  val xscale : Num.t -> t -> t

  (** [cut_after p1 p2] cuts [p2] after the intersection with [p1]. 
      To memorize the order of the arguments, 
      you can read: "cut after [p1]" *)
  val cut_after : t -> t -> t

  (** Same as {!cut_after}, but cuts before *)
  val cut_before: t -> t -> t

  (** Build a cycle from a set of intersecting paths *)
  val build_cycle : t list -> t

  (** {2 Predefined values} *)

  (** The default joint style ([JLine]) *)
  val defaultjoint : joint

  (** A full circle of radius 1 and centered on the origin *)
  val fullcircle : t

  (** The upper half of [fullcircle] *)
  val halfcircle : t

  (** The right half of [halfcircle] *)
  val quartercircle: t

  (** A full square of size 1 and centered on the origin *)
  val unitsquare: t

end

and Pen : sig
  (** Pens are used to change the the way lines are drawn in Mlpost *)

  type t
    (** The abstract type of pens *)

  val transform : Transform.t -> t -> t
    (** Apply a transformation to pens *)
  val default : ?tr:Transform.t -> unit -> t
    (** The default pen; it corresponds to [transform [Transform.scaled 0.5]
   circle] *)
  val circle : ?tr:Transform.t -> unit -> t
    (** A circular pen of diameter 1 bp *)
  val square : ?tr:Transform.t -> unit -> t
    (** A pen in form of a square, of length 1 bp *)
  val from_path : Path.t -> t
    (** Construct a pen from a closed path *)

end

and Dash : sig
  (** This module permits to define dash patterns, that are used to draw lines
   in different styles *)

  type t
    (** The abstract type of dash patterns *)

  val evenly : t
    (** The pattern composed of evenly spaced dashes *)
  val withdots : t
    (** The pattern composed of evenly spaced dots *)

  val scaled : float -> t -> t
    (** Scale a dash pattern *)
  val shifted : Point.t -> t -> t
    (** Shift a dash pattern *)

  type on_off = On of Num.t | Off of Num.t


  val pattern : on_off list -> t
    (** This function, together with the type [on_off]  permits to construct
     custom dash patterns, by giving a list of [On] / [Off] constructors, with 
      corresponding lengths *)

end

and Color : sig

  (** Colors *)

  type t
    (** the abstract type of colors *)

  val default : t
    (** the default color is black *)

  val rgb : float -> float -> float -> t
    (** [rgb r g b] constructs the color that corresponds to the color code 
	RGB(r,g,b)  *)
  val rgb8 : int -> int -> int -> t
    (** similar to [rgb], but takes integers between 0 and 255 as argument *)

  val cmyk : float -> float -> float -> float -> t
    (** [cmyk c m y k] constructs the color that corresponds to the color code 
	CMYK(c,m,y,k)  *)

  (** {2 Predefined Colors} *)

  val red : t
  val orange : t
  val blue : t
  val purple : t
  val gray : float -> t
  val white : t
  val black : t
  val green : t
  val cyan : t
  val yellow : t
  val magenta : t

  (** {3 X11-named Colors} *)
  val color : string -> t
    (** [color n] returns the RGB color associated to name [n]
	(as defined in /etc/X11/rgb.txt). Raises [Not_found] if [n] does not
	correspond to a color *)
end

and Box : sig

  (** Boxes *)

  (** The abstract type of boxes *)
  type t

  (** {2 Creating boxes} *)

  val circle : ?dr:Num.t -> Point.t -> Picture.t -> t
    (** [circle p pic] creates a circle box of center [p] and of contents
	[pic]. Optional padding is given by arguments [dr]. *)

  val ellipse : ?dx:Num.t -> ?dy:Num.t -> Point.t -> Picture.t -> t
    (** [ellipse p pic] creates a elliptic box of center [p] and of contents
	[pic]. Optional padding is given by arguments [dx] and [dy]. *)

  val rect :  ?dx:Num.t -> ?dy:Num.t -> Point.t -> Picture.t -> t
    (** [rect p pic] creates a rectangular box of center [p] and of contents
	[pic]. Optional padding is given by arguments [dx] and [dy] ; 
	default is 2bp. *)

  val round_rect : ?dx:Num.t -> ?dy:Num.t -> Point.t -> Picture.t -> t
    (** [round_rect p pic] creates a rectangular box of center [p] and of contents
	[pic], with rounded corners. Optional padding is given by [dx] and
	[dy] ; default is 2bp *)


  (** Get the bounding path of a box *)
  val bpath : t -> Path.t

  (** Get the picture enclosed in a box *)
  val picture : t -> Picture.t

  (** {2 Special points on a box} *)

  val center : t -> Point.t
  val north : t -> Point.t
  val south : t -> Point.t
  val west  : t -> Point.t
  val east  : t -> Point.t 
  val north_west : t -> Point.t
  val south_west : t -> Point.t
  val north_east : t -> Point.t
  val south_east : t -> Point.t

  (** {2 Boxes alignment} *)

  val valign : ?dx:Num.t -> ?dy:Num.t -> Picture.t list -> t list
    (** [valign l] turns a list of pictures into a list of boxes,
	which contain the given pictures and are vertically aligned. 
	All boxes have the same width. Some padding can be specified with
	optional arguments [dx] and [dy]. *)

  val halign : ?dx:Num.t -> ?dy:Num.t -> Picture.t list -> t list
    (** [halign] is similar to [valign], but with an horizontal alignment. *)

  val tabular : 
    ?dx:Num.t -> ?dy:Num.t -> Picture.t array array -> t array array
    (** turns a matrix of pictures into a matrix of boxes *)

  val tabularl : ?dx:Num.t -> ?dy:Num.t -> Picture.t list list -> t list list
    (** similar to [tabular], but using lists instead of arrays *)

  val tabulari : 
    ?dx:Num.t -> ?dy:Num.t -> int -> int -> 
    (int -> int -> Picture.t) -> t array array
    (** similar to [tabular], but using a matrix defined with a function *)

end

and Transform : sig

  (** Transformations are an important way to modify objects in Mlpost.
      Objects can be scaled, shifted, rotated, etc, and any combination of
      these transformations is possible. Currently, transformations can be
      applied to Pictures, Pens and Paths. *)  

  type t'
    (** The abstract type of a single transformation *)

  val scaled : Num.t -> t'
    (** Scale an object by a constant factor.
      @param scale a scaling function to be applied to each float;
      see {!Num.t} for scaling functions for usual units. This makes only sense
      when the object to be transformed is given in "bp" units *)
  val rotated : float -> t'
    (** Rotate an object by an angle given in degrees *)
  val shifted : Point.t -> t'
    (** Shift an object with respect to a point *)
  val slanted : Num.t -> t'
    (** Slant an object: the point [(x,y)] becomes [(x+ay,y)], with slanting
        factor [a] *)
  val xscaled : Num.t -> t'
    (** Scale an object by a constant factor, but only in the [x] direction *)
  val yscaled : Num.t -> t'
    (** Scale an object by a constant factor, but only in the [y] direction *)
  val zscaled : Point.t -> t'
    (** Zscaled multiplies points of the object by the given point, using
        "complex" multiplication: [(x,y) * (a,b) = (ax - by, bx + ay)];
        its effect is to rotate and scale so as to map [(1,0)] into [(a,b)] *)
  val reflect : Point.t -> Point.t -> t'
    (** Reflect an object with respect to the line that goes through the two
        given points *)
  val rotate_around : Point.t -> float -> t'
    (** Rotate an object by an angle given in degrees, around a given point *)

  type t = t' list
    (** A transformation is a list of single transformations *)

  val id : t
    (** The identity transformation  *)
end

and Picture : sig

  (** Pictures are a powerful way to reuse and modify parts of a figure *)

  type t
    (** The abstract type of pictures *)

  val make : Command.t -> t
    (** Make a picture from a drawing command *)

  val tex : string -> t
   (** Take a string in Latex format and transform it into a picture *)

(*
  val currentpicture : t
    (* Corresponds to the picture that has been drawn so far and can be used in
     * commands to manipulate it *)

*)

  val transform : Transform.t -> t -> t
    (** Apply a transformation to a picture *)

  val bbox : t -> Path.t
    (** Get the bounding box of a picture (with default padding, as
	in MetaPost) *)

  val corner_bbox : ?dx:Num.t -> ?dy:Num.t -> t -> Path.t
    (** Get the bounding box of a picture, according to its corners
        and supplied padding [dx] and [dy]. *)

  val center : t -> Point.t -> t
    (** Place a picture centered at some point *)

  val place_up_left : t -> Point.t -> t
    (** Place a picture with its upper left corner at some point *)

  val place_up_right : t -> Point.t -> t
    (** Place a picture with its upper right corner at some point *)

  val place_bot_left : t -> Point.t -> t
    (** Place a picture with its bottom left corner at some point *)

  val place_bot_right : t -> Point.t -> t
    (** Place a picture with its bottom right corner at some point *)

  val beside : t -> t -> t
    (** [beside p1 p2] returns a picture in which [p2] is placed right to [p1] *)

  val below : t -> t -> t
    (** [below p1 p2] returns a picture in which [p2] is placed below [p1] *)

  (** {2 Special points of the bounding box of a picture} *)

  val ulcorner : t -> Point.t
  val llcorner : t -> Point.t
  val urcorner : t -> Point.t
  val lrcorner : t -> Point.t

  val clip : t -> Path.t -> t
  (** [clip pic path] limits [pic] to the cyclic path [path]; all elements 
   *   outside of [path] are cut off. *)

  (** {2 Dimensions} *)

  val width : t -> Num.t
  val height : t -> Num.t

  (** Predefined Transformations *)

  val scale : Num.t -> t -> t
  val rotate : float -> t -> t
  val shift : Point.t -> t -> t
  val yscale : Num.t -> t -> t
  val xscale : Num.t -> t -> t

end

and Command : sig

  (** General Commands to build figures *)

  type t
      (** The abstract commands type *)

  type figure = t list
      (** A figure is a list of commands *)

(*
  val logo : figure
    (** The Mlpost logo. *)

*)
  (** {2 Drawing Commands} *)

  val draw : ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t -> Path.t -> t
    (** Draw a path 
	@param color the color of the path; default is black
	@param pen the pen used to draw the path; default is {!Pen.default}
	@param dashed if given, the path is drawn using that dash_style. *)

  val draw_arrow : ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t -> Path.t -> t
    (** Draw a path with an arrow head; the optional arguments 
	are the same as for {!draw} *)

  val fill : ?color:Color.t -> Path.t -> t
    (** Fill a contour given by a closed path 
	@param color the color used to fill the area; default is black *)

  val draw_box : ?fill:Color.t -> ?boxed:bool -> Box.t -> t
    (** Draw a box 
	@param fill the color used to fill the box 
	@param boxed if set, the box border is drawn (default is [true]) *)

  val draw_pic : Picture.t -> t
    (** draws a picture *) 

  (** {2 Manipulating Commands} *)

  val nop : t
    (** A command that has no effect *)

  val append : t -> t -> t
    (** Append two commands to form a compound command *)

  val (++) : t -> t -> t
    (** Abbreviation for [append] *)

  val seq : t list -> t
    (** Group a list of commands to a single command *)

  val iter : int -> int -> (int -> t) -> t
    (** [iter m n f] builds a command that corresponds to the sequence
	of commands [f m; f (m+1); ... ; f(n)] *)

  val iterl : ('a -> t) -> 'a list -> t
    (** [iterl f l] builds a command that corresponds to the sequence
	of commands [f x1; f x2; ... ; f xn] for [l = [x1;x2;...;xn]] *)

  (** {2 Labels} *)

  (** Positions; useful to place labels *)
  type position =
      | Pcenter
      | Pleft
      | Pright
      | Ptop
      | Pbot
      | Pupleft
      | Pupright
      | Plowleft
      | Plowright

  (** [label ~pos:Pleft pic p] puts picture [pic] at the left of point [p] *)
  val label : ?pos:position -> Picture.t -> Point.t -> t

  (** Works like [label], but puts a dot at point [p] as well *)
  val dotlabel : ?pos:position -> Picture.t -> Point.t -> t

end

(** {2 Helpers and high-level drawing commands} *)

module Helpers : sig
  val dotlabels :
    ?pos:Command.position -> string list -> Point.t list -> Command.t list
  val draw_simple_arrow :
    ?color:Color.t -> ?pen:Pen.t -> 
    ?style:Path.joint -> ?outd:Path.direction -> ?ind:Path.direction ->
    Point.t -> Point.t -> Command.t
  val draw_label_arrow :
    ?color:Color.t -> ?pen:Pen.t ->
    ?style:Path.joint -> ?outd:Path.direction -> ?ind:Path.direction ->
    ?pos:Command.position -> Picture.t -> 
    Point.t -> Point.t -> Command.t
  val box_path :
    style:PrimPath.joint ->
    outd:PrimPath.direction ->
    ind:PrimPath.direction -> Box.t -> Box.t -> Path.t
  val box_arrow :
    ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t ->
    ?style:Path.joint -> ?outd:Path.direction -> ?ind:Path.direction -> 
    Box.t -> Box.t -> Command.t
  val box_line :
    ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t ->
    ?style:Path.joint -> ?outd:Path.direction -> ?ind:Path.direction -> 
    Box.t -> Box.t -> Command.t
  val box_label_arrow :
    ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t ->
    ?style:Path.joint -> ?outd:Path.direction -> ?ind:Path.direction ->
    ?pos:Command.position -> Picture.t -> 
    Box.t -> Box.t -> Command.t
  val box_label_line :
    ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t ->
    ?style:Path.joint -> ?outd:Path.direction -> ?ind:Path.direction ->
    ?pos:Command.position -> Picture.t -> 
    Box.t -> Box.t -> Command.t

end

module Tree : sig

  (** This module provides high-level means for creating and drawing Trees *)

  type t
    (** The abstract type of trees *)

  (** {2 Creation} *)
    
  (** The style for tree nodes *) 
  type node_style = Circle | Rect

  val leaf : ?style:node_style -> ?fill:Color.t -> string -> t
    (** [leaf label] creates a leaf with label [label]. 
	@param style a [node_style] describing how the leaf should be drawn
	@param fill if present, this color is used to fill the leaf *)

  val node : ?style:node_style -> ?fill:Color.t -> string -> t list -> t
    (** [node label children] creates a node with label [label] and a list
	of children [children]. 
	Optional arguments are the same as in [leaf]. *)

  val bin  : ?style:node_style -> ?fill:Color.t -> string -> t -> t -> t
    (** [bin label l r] creates a binary node with label [label] and 
	children [l] and [r].
	Optional arguments are the same as in [leaf]. *)

  (** Variants to create trees with pictures at nodes *)
  module Pic : sig
    val leaf : ?style:node_style -> ?fill:Color.t -> Picture.t -> t
    val node : ?style:node_style -> ?fill:Color.t -> Picture.t -> t list -> t
    val bin  : ?style:node_style -> ?fill:Color.t -> Picture.t -> t -> t -> t
  end

  (** {2 Drawing} *)

  (** The style of arrows between nodes *)
  type arrow_style = 
      Directed     (** edges are directed and an 
		       arrow is drawn at the end of an edge *)
    | Undirected   (** edges are undirected and no arrow is drawn *)

  (** There are several styles available for edges *)
  type edge_style = 
      Straight 	  (** edges are straight lines between nodes *)
    | Curve 	  (** edges are curved lines between nodes *)
    | Square 	  (** edges are straight lines and 
		      branch out from the sides of nodes *)
    | HalfSquare  (** edges are straight lines and 
		      branch out from below nodes *)

  val draw : 
    ?scale:(float -> Num.t) -> 
    ?node_style:node_style -> ?arrow_style:arrow_style -> 
    ?edge_style:edge_style ->
    ?boxed:bool -> ?fill:Color.t -> ?stroke:Color.t -> ?pen:Pen.t ->
    ?ls:Num.t -> ?nw:Num.t -> ?cs:Num.t -> 
    t -> Command.t
    (** Default scale is [Num.cm].
	Default node_style is [Circle].
	Default arrow_style is [Directed].
	Default edge_style is [Straight].

	Drawing parameters are:
	- [ls] (level sep): vertical distance between levels.
        The default value is 1.0. A negative value draws the tree upward.
	- [nw] (node width): width of one node. The default value is 0.5.
	- [cs] (children sep): horizontal distance between siblings.
        The default value is 0.2.
    *)

end

module Diag : sig

  (** Diagrams. *)

  (** This module permits to create diagrams in a very simple and yet quite
   flexible fashion. It permits to specify content, form and color of nodes as
   well as color, form and labels of arrows between nodes. Nodes have to be 
   placed by hand, though *)

  (** {2 Creation} *)

  type node
    (** The abstract type of nodes *)

  val node : ?fill:Color.t -> float -> float -> string -> node
    (** Construct a node at a given position with a given content in Latex
        format *)
  val pic_node : ?fill:Color.t -> float -> float -> Picture.t -> node
    (** Construct a node at a given position with a given picture in it *)

  type t
    (** The abstract type of diagrams *)

  val create : node list -> t
    (** Create a diagram that consists of the given nodes *)

  type dir = Up | Down | Left | Right | Angle of float

  val arrow : 
    t -> ?lab:string -> ?pos:Command.position -> 
    ?head:bool -> ?dashed:Dash.t -> ?outd:dir -> ?ind:dir -> 
    node -> node -> unit
    (** [arrow d n1 n2] adds an arrow between n1 and n2 in the diagram d, by
        side effect.
	@param lab The label of the arrow, in Latex format
	@param pos The position of the label, relative to the arrow
	@param head If true, the arrow has a head. Otherwise, it's just a line.
	@param outd The outgoing direction of the arrow
	@param ind The ingoing direction of the arrow *)

  (** {2 Drawing} *)

  type node_style = Circle of Num.t | Rect

  val draw : 
    ?scale:(float -> Num.t) -> ?style:node_style -> 
    ?boxed:bool -> ?fill:Color.t -> ?stroke:Color.t -> ?pen:Pen.t ->
    t -> Command.t
    (** Draws the diagram.
        @param scale The distance between nodes; default is 40 bp
	@param style The style of nodes: circular or rectangular 
	(default is circular)
	@param boxed The border is drawn if set (default is true)
	@param fill The color to fill nodes
	@param stroke The color to draw arrows
	@param pen The pen used for arrows *)
end


module Plot : sig

  (** Plots. *)

  (** This module helps drawing grids and plotting functions. *)

  type skeleton
    (** The abstract skeleton for grids, axes and functions *)

  val mk_skeleton : int -> int -> Num.t -> Num.t -> skeleton
    (** [mk_skeleton w h dx dy] builds a skeleton of width [w] and height [h],
	each cell being [dx] units wide and [dy] units high. *)

  type labels = (int -> Picture.t) option
  type ticks =  (Num.t * Pen.t) option
	
  type drawing = Stepwise | Normal
	
  val draw_grid : ?hdash:(int -> Dash.t) ->
                  ?vdash:(int -> Dash.t) ->
                  ?hpen:(int -> Pen.t) ->
                  ?vpen:(int -> Pen.t) -> skeleton -> Command.t

  val draw_axes : ?hpen:Pen.t -> ?vpen:Pen.t -> ?hlabel:labels -> 
                  ?vlabel:labels -> ?ticks:ticks -> ?closed:bool -> 
                  ?hcaption:Picture.t -> ?vcaption:Picture.t ->
                    skeleton -> Command.t

  val draw_func : ?pen:Pen.t -> ?drawing:drawing -> 
                  ?style:Path.joint -> ?dashed:Dash.t ->
                  ?label:(Picture.t * Command.position * int) ->
                    (int -> float) -> skeleton -> Command.t
end


module Shapes : sig

(** Various Basic Geometric Shapes *)

  val rounded_rect_path : Num.t -> Num.t -> Num.t -> Num.t -> Path.t

  val rounded_rect : 
    ?fill:Color.t -> ?stroke:Color.t -> ?thickness:float ->
    Num.t -> Num.t -> Num.t -> Num.t -> Picture.t
    (** [rounded_rect w h rx ry] draws a rectangle of width [w] and
	height [h] with rounded corners. The rounded corners are arcs
	of an ellipse of radii [rx] and [ry]. [rx] (resp. [ry]) should
	be positive and smaller than [w/2] (resp. [h/2]).
	@param fill the color with which to fill the rectangle ;
	  if no color is provided, the rectangle is not filled.
	@param stroke the color with which the rectangle's outline
	  shall be drawn ; default is black.
	@param thickness the thickness of the pen used to draw
	  the outline ; 1. is default
    *)
      
  val rectangle :
    ?fill:Color.t -> ?stroke:Color.t -> ?thickness:float ->
    Num.t -> Num.t -> Picture.t
    (** [rectangle w h] draws a rectangle of width [w] and height [h].
	@param fill the color with which to fill the rectangle ;
	  if no color is provided, the rectangle is not filled.
	@param stroke the color with which the rectangle's outline
	  shall be drawn ; default is black.
	@param thickness the thickness of the pen used to draw
	  the outline ; 1. is default
    *)

  val ellipse :
    ?fill:Color.t -> ?stroke:Color.t -> ?thickness:float ->
    Num.t -> Num.t -> Picture.t
    (** [ellipse rx ry] draws an ellipse of great axis [rx] and small axis [ry].
	The ellipse is centered on the origin and aligned with the x axis.
	@param fill the colod with which to fill the ellipse ; if no color
	  is provided, it is not filled.
	@param stroke the color with which the ellipse's outline shall be
	  drawn ; default is black.
	@param thickness the thickness of the pen used to draw
	  the outline ; 1. is default
    *)

  val arc_ellipse :
    ?fill:Color.t -> ?stroke:Color.t -> ?thickness:float -> ?close:bool ->
    Num.t -> Num.t -> float -> float -> Picture.t
    (** [arc_ellipse rx ry th1 th2] draws an arc of the ellipse 
	of great axis [rx] and small axis [ry] starting at angle [th1] and
	ending at angle [th2] (in radians).
	The ellipse is centered on the origin and aligned with the x axis.
	@param fill the colod with which to fill the ellipse ; if no color
	  is provided, it is not filled.
	@param stroke the color with which the ellipse's outline shall be
	  drawn ; default is black.
	@param thickness the thickness of the pen used to draw
	  the outline ; 1. is default
	@param close if true, the extremities of the arc are joined to 
	the origin by straight lines, thus closing path. If [fill] is provided,
	then [close] will be true by default ; otherwise it is false.
    *)

end

(** {2 Metapost generation} *)

(* Misc does not appear in the documentation *)
(**/**)
module Misc : sig
  val write_to_file : string -> (out_channel -> 'a) -> unit
  val write_to_formatted_file : string -> (Format.formatter -> 'a) -> unit
  val print_option :
    string ->
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a option -> unit
  val print_list :
    ('a -> unit -> 'b) -> ('a -> 'c -> unit) -> 'a -> 'c list -> unit
  val space : Format.formatter -> unit -> unit
  val comma : Format.formatter -> unit -> unit

  val fold_from_to : ('a -> int -> 'a) -> 'a -> int -> int -> 'a
  (** [fold_from_to f acc i j] is equivalent to 
   *   [List.fold_left f acc [i; i +1; .. j] ],
   *  where i <= j *)
end
(**/**)

module Metapost : sig

  val generate_mp :
    string ->
    ?prelude:(Format.formatter -> unit -> unit) ->
    (int * Command.t list) list -> unit

  val emit : string -> Command.t list -> unit
  val dump : ?prelude:string -> ?pdf:bool -> string -> unit
    (** [dump ?prelude ?pdf f] builds a Metapost file [f.mp] for all figures,
	then runs Metapost on it, and renames figure files according to the
	names specified to [emit]. The file suffix is [.mps] if [pdf] is
	set, and [.1] otherwise. *)

  val dump_tex : ?prelude:string -> string -> unit
    (** [dump_tex ?prelude f] builds a LaTeX file [f.tex] for all the figures,
	using LaTeX prelude [prelude] if given. *)

  val slideshow : Command.figure list -> int -> (int * Command.figure) list
    (** takes a list of figures and returns a list of figures of exactly the
        same size (the size of the biggest figure). Shared objects are 
        hopefully placed at the same absolute location across figures. The 
        resulting figures are numbered with consecutive increasing integers, 
        starting with the given value. *)

end

module Generate : sig
  val generate_tex : string -> string -> string -> (int * 'a) list -> unit
end

