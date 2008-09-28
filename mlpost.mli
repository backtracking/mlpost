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

  (** Numerics are intended to be lengths in some unit. In addition, values of
    type {!Num.t} may actually be unknown to Mlpost. This is why there is no
    function that gives back a [float]. *)

  type t
      (** The Mlpost numeric type is an abstract datatype *)
      
  (** {2 Conversion functions} *)
  (** The base unit in Mlpost are bp. The following functions 
      permit to specify values in other common units *)

  val bp : float -> t

  val pt : float -> t
  (** pt are PostScript points. This is the same unit as the pt unit in Latex *)

  val cm : float -> t
  val mm : float -> t
  val inch : float -> t

  (** {2 Useful operations on Nums} *)
  val addn : t -> t -> t
  val subn : t -> t -> t
  val multn : t -> t -> t
  val multf : float -> t -> t
  val divf : t -> float -> t
  val neg : t -> t
  val divn : t -> t -> t
  val maxn : t -> t -> t
  val minn : t -> t -> t

  val gmean : t -> t -> t
  (** the geometric mean of two nums : sqrt(a * a + b * b) *)

  (** {3 Infix operators}  *)

  module Infix : sig
    val (+/) : t -> t -> t
    (** alias for {!Num.addn} *)
     
    val (-/) : t -> t -> t
    (** alias for {!Num.subn} *)

    val ( */) : t -> t -> t
    (** alias for {!Num.multn} *)

    val (//) : t -> t -> t
    (** alias for {!Num.divn} *)

    val ( *./): float -> t -> t
    (** alias for {!Num.multf} *)

    val (/./): t -> float -> t
    (** alias for {!Num.divf} *)

  end

  (** {2 Useful constants and functions} *)

  val zero : t
  val one : t
  val two : t
  (** Shortcuts for [bp 0.], [bp 1.] and [bp 2.]. *)

  val pi : float
  (** 3 .14159 *)
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

  val xpart : t -> Num.t
  (** [xpart p] is the x coordinate of point [p] *)

  val ypart : t -> Num.t
  (** [ypart p] is the y coordinate of point [p] *)

  (** {2 Operations on points} *)
    
  val transform : Transform.t -> t -> t
  (** Apply a transformation to a point *)

  val segment : float -> t -> t -> t
  (** [segment f p1 p2] is the point [(1-f)p1 + fp2]. Stated otherwise, if
     [p1] is at [0.] and [p2] is at [1.], return the point that lies at [f] *)

  val add : t -> t -> t
  val shift : t -> t -> t
  (** Sum two points *)
  
  val sub : t -> t -> t
  (** Substract two points *)
  
  val mult : Num.t -> t -> t
  val scale : Num.t -> t -> t
  (** Multiply a point by a scalar *)
  
  val rotate : float -> t -> t
  (** Rotate a point by an angle in degrees *)

  (** [rotate_around p1 f p2] rotates [p2] around [p1] by an angle [f] 
      in degrees *)
  val rotate_around : t -> float -> t -> t

  (** Scales the X coordinate of a point by a scalar *)
  val xscale : Num.t -> t -> t

  (** Scales the Y coordinate of a point by a scalar *)
  val yscale : Num.t -> t -> t

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
      {ul {- [vec p] defines a direction by a point (interpreted as a vector)}
      {- [curl f] changes the curling factor of the extremity of a path; 
      higher curling factor means flatter curves}
      {- [noDir] means no particular direction} } *)
  type direction

  val vec : Point.t -> direction
  val curl : float -> direction
  val noDir : direction

  (** A [knot] is the basic element of a path, and is simply a point 
      with an incoming and outgoing direction constraint *)
  type knot

  (** Build a knot from a point; the optional arguments are the incoming directions *)
  val knotp :
    ?l:direction -> ?r:direction -> Point.t -> knot

  val knotlist : (direction * Point.t * direction) list -> knot list
    

  (** A joint is the connection between two knots in a path. It is either
      {ul {- [jLine] for a straight line}
      {- [jCurve] for a spline curve}
      {- [jCurveNoInflex] to avoid inflexion points}
      {- [jTension f1 f2] to specify "tension" on the joint; [jCurve] uses a default
      tension of 1. Higher tension means less "wild" curves}
      {- [jControls p1 p2] to explicitely specify control points}} *)
  type joint 
  val jLine : joint
  val jCurve : joint
  val jCurveNoInflex : joint
  val jTension : float -> float -> joint
  val jControls : Point.t -> Point.t -> joint

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
      given "in control points": [0.] means the first control point,
      [1.] the second and so on; intermediate values are accepted. *)
  val point : float -> t -> Point.t

  (** [direction f p] returns the direction of the tangent at [point f p]. *)
  val direction : float -> t -> Point.t

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
  (** Shortcuts for transformations of Paths *)

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
    (** The default pen; it corresponds to 
      [Pen.scale (Num.bp 0.5) (Pen.circle ())] *)
  val circle : ?tr:Transform.t -> unit -> t
    (** A circular pen of diameter 1 bp *)
  val square : ?tr:Transform.t -> unit -> t
    (** A pen in form of a square, of length 1 bp *)
  val from_path : Path.t -> t
    (** Construct a pen from a closed path *)

  val scale : Num.t -> t -> t
  val rotate : float -> t -> t
  val shift : Point.t -> t -> t
  val yscale : Num.t -> t -> t
  val xscale : Num.t -> t -> t
  (** Shortcuts for transformations of pens *)
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

  type on_off 

  val on : Num.t -> on_off
  val off : Num.t -> on_off

  val pattern : on_off list -> t
    (** This function, together with the type [on_off]  permits to construct
     custom dash patterns, by giving a list of [on] / [off] constructors, with 
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

  (** {3 base colors} *)

  val white : t
  val black : t
  val red : t
  val blue : t
  val green : t
  val cyan : t
  val yellow : t
  val magenta : t
    
  (** {3 lighter colors} *)

  val lightred : t
  val lightblue : t
  val lightgreen : t
  val lightcyan : t
  val lightyellow : t
  val lightmagenta : t

  (** {3 grays} *)

  val gray : float -> t
  val lightgray : t
  val mediumgray : t
  val darkgray : t

  (** {3 additional colors} *)

  val orange : t
  val purple : t

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

  val empty : t
    (** the empty box *)

  type style = Rect | Circle | Ellipse | RoundRect | Patatoid

  type 'a box_creator = 
    ?dx:Num.t -> ?dy:Num.t -> ?name:string -> 
    ?stroke:Color.t option -> ?fill:Color.t -> 'a -> t
    (** All functions used to create boxes take the following optional
	parameters : [dx] (resp. [dy]) is the horizontal
	(resp. vertical) padding between the box border and its
	contents ; [name], if present, is associated with the box and
	can be used to retrieve it using [get] ; [stroke] is the color
	used to draw the outline of the box ; when equal to [None],
	the outline will not be drawn ; [fill], if present, is the
	color used to fill the box.  
    *)

  val pic : ?style:style -> Picture.t box_creator
    (** [pic p] creates a new box containing the picture [p] *)

  val tex : ?style:style -> string box_creator
    (** [tex s] creates a new box containing the LaTeX string [s] *)

  val box : ?style:style -> t box_creator
    (** [box b] creates a new box containing the box [b] *)

  val circle : Picture.t box_creator
    (** [circle pic] creates a circle box containing the picture
    [pic]. Optional padding is given by arguments [dx] and [dy]; 
    default is 2bp. *)

  val ellipse : Picture.t box_creator
    (** [ellipse p pic] creates a elliptic box containing the picture
	[pic]. Optional padding is given by arguments [dx] and [dy]; 
	default is 2bp *)

  val rect :  Picture.t box_creator
    (** [rect p pic] creates a rectangular box containing the picture
	[pic]. Optional padding is given by arguments [dx] and [dy]; 
	default is 2bp. *)

  val round_rect : Picture.t box_creator
    (** [round_rect p pic] creates a rectangular box containing the picture 
	[pic], with rounded corners. Optional padding is given by [dx] 
	and [dy]; default is 2bp *)

  val patatoid : Picture.t box_creator
    (** [patatoid p pic] creates an undefined, vaguely rectangular box 
	containing the picture [pic]. It may happen that the content 
	overlaps with the box. *)

(***
  val round_rect_gen : ?dx:Num.t -> ?dy:Num.t -> ?rx:Num.t -> ?ry:Num.t -> 
    Point.t -> Picture.t -> t
    (** [round_rect_gen p pic] creates a rectangular box of center [p] and 
	of contents [pic], with rounded corners of radii [rx] and [ry]. 
	Optional padding is given by [dx] and [dy] ; default is 2bp *)
***)

  (** Get the bounding path of a box *)
  val bpath : t -> Path.t

  (** {2 Special points on a box} *)

  val ctr : t -> Point.t
  val north : t -> Point.t
  val south : t -> Point.t
  val west  : t -> Point.t
  val east  : t -> Point.t 
  val north_west : t -> Point.t
  val south_west : t -> Point.t
  val north_east : t -> Point.t
  val south_east : t -> Point.t

  (** {2 POS compliance} *)

  type repr = t
  val v : t -> repr
  val ctr : t -> Point.t
  (** return the center of the object *)
  val height : t -> Num.t
  (** return the height of the object *)
  val width : t -> Num.t
  (** return the width of the object *)
  val shift : Point.t -> repr -> repr
  (** [shift pt x] shifts the object [x] about the point [pt]  *)
  val center : Point.t -> t -> repr
  (** [center pt x] centers the object [x] at the point [pt]  *)

  val draw : ?debug:bool -> t -> Command.t
    (** Draws a box 
	@param debug if set to to true, the bounding
	path and the center of the box are drawn as well, default is false
    *)

  (** {2 Boxes alignment} *)

  val hbox : ?padding:Num.t -> ?pos:Command.vposition -> ?style:style -> 
    t list box_creator
      (** aligns the given boxes horizontally and returns a box containing
	  these boxes as sub-components. 
	  @param padding horizontal padding used to separate the boxes; 
	  defaults to 0
	  @param pos used to determine the way boxes are aligned; defaults to
	  [`Center]
      *)

  val vbox : ?padding:Num.t -> ?pos:Command.hposition -> ?style:style -> 
    t list box_creator
      (** aligns the given boxes vertically and returns a box containing
	  these boxes as sub-components. 
	  @param padding vertical padding used to separate the boxes
	  @param pos used to determine the way boxes are aligned
      *)

  val tabular : 
    ?hpadding:Num.t -> ?vpadding:Num.t -> ?pos:Command.position ->
    t array array -> t
    (** aligns the given boxes both vertically and horizontally and returns
	a box containing all these boxes (with rows as first sub-components,
	and then individual boxes as sub-components of each row). 
	Columns (resp. rows) are separated by [hpadding] (resp. [vpadding]);
	both default to 0.
	Alignment within columns and rows is controlled using [pos]. 
	The arrays for rows must have the same lengths; otherwise
	[Invalid_argument] is raised. *)

  val tabularl : ?hpadding:Num.t -> ?vpadding:Num.t -> ?pos:Command.position ->
    t list list -> t
    (** similar to [tabular], but using lists instead of arrays *)

  val tabulari : 
    ?hpadding:Num.t -> ?vpadding:Num.t -> ?pos:Command.position ->
    int -> int -> (int -> int -> t) -> t 
    (** similar to [tabular], but using a matrix defined with a function *)

  val hblock : ?pos:Command.vposition -> t list -> t
    (** [hblock bl] aligns the boxes of [bl] horizontally and surround
	them with new rectangular boxes of the same height; all these new
	boxes are packed together into the returned box. *)

  val vblock : ?pos:Command.hposition -> t list -> t
    (** similar to [hblock], with vertical alignment *)

  (** {2 Sub-boxes accessors} *)

  val nth : int -> t -> t
    (** [nth i b] returns the [i]-th sub-box of [b]. The first sub-box
	has index 0. Raises [Invalid_argument] if there is no such sub-box. *)

  val get : string -> t -> t
    (** [get n b] returns the sub-box of [b] of name [n], if any, and
	raises [Invalid_argument] otherwise. The behavior is not specified
	if [b] contains several sub-boxes with name [n]. *)

  val elts : t -> t array
    (** [elts b] returns the sub-boxes of [b]; returns the empty array for
        the empty box or a box containing a picture. *)

  (** {2 Box properties} *)

  val get_fill : t -> Color.t option
  val set_fill : Color.t -> t -> t

  val get_stroke : t -> Color.t option
  val set_stroke : Color.t -> t -> t
  val clear_stroke : t -> t

  (** {2 Misc.} *)

  val cpath :
    ?style:Path.joint ->
    ?outd:Path.direction ->
    ?ind:Path.direction -> t -> t -> Path.t
    (** the path that connects 2 boxes and stops at the box boundaries *) 
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

  type repr = t

  val make : Command.t -> t
    (** Make a picture from a drawing command *)

  val tex : string -> t
   (** Take a string in Latex format and transform it into a picture *)

(*
  val currentpicture : t
    (* Corresponds to the picture that has been drawn so far and can be used in
      commands to manipulate it *)

*)

  val transform : Transform.t -> t -> t
    (** Apply a transformation to a picture *)

  val bbox : t -> Path.t
    (** Get the bounding box of a picture (with default padding, as
	in MetaPost) *)

  val corner_bbox : ?dx:Num.t -> ?dy:Num.t -> t -> Path.t
    (** Get the bounding box of a picture, according to its corners
        and supplied padding [dx] and [dy]. *)

  val center :  Point.t -> t -> t
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
  val ctr : t -> Point.t

  val clip : t -> Path.t -> t
  (** [clip pic path] limits [pic] to the cyclic path [path]; all elements 
      outside of [path] are cut off. *)

  (** {2 Dimensions} *)

  val width : t -> Num.t
  val height : t -> Num.t

  (** Predefined Transformations *)

  val scale : Num.t -> t -> t
  val rotate : float -> t -> t
  val shift : Point.t -> t -> t
  val yscale : Num.t -> t -> t
  val xscale : Num.t -> t -> t
  val spin : float -> t -> t

  val v : t -> repr

end

and Arrow : sig
  (** The Beginning of a module for building arrows. Actually, an arrow is just
    a path. Use {!Command.draw_arrow} to draw arrows. *)

  val simple : ?style:Path.joint -> ?outd:Path.direction -> 
               ?ind:Path.direction -> Point.t -> Point.t -> Path.t
  (** A simple arrow between two points. You can choose ingoing and outgoing
    directions. *)

  (** {2 Heads} *)

  type head = Point.t -> Point.t -> Command.t
  (** A head is simply a function which takes a point and a normalized
    direction and return a command which draws the head. *)

  val no_head : head
  (** The empty head, meaning there is no head at all. *)

  val simple_head :
    ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t -> ?angle:float ->
    ?size:Num.t -> head
  (** A simple head with two straight lines.
  @param color the color of the head; default is black
  @param pen the pen used to draw the head; default is {!Pen.default}
  @param dashed if given, the head is drawn using that dash_style
  @param angle the angle between the two lines in degrees, default is 60 degrees
  @param size the length of the two lines, default is 4bp *)

  (** {2 Drawing} *)

  val draw : 
    ?style:Path.joint -> ?outd:Path.direction -> ?ind:Path.direction ->
    ?foot:head ->
    ?head:head ->
    Point.t -> Point.t -> Command.t
  (** Draw a complex arrow between two points.
  @param style the joint style used for all joints in the path
  @param outd the direction of the beginning of the arrow, for curved arrows
  @param ind the direction of the end of the arrow, for curved arrows
  @param foot the style used for the arrow head at the beginning of the arrow
  (default is {!no_head})
  @param head the style used for the arrow head at the end of the arrow
  (default is {!simple_head}) *)

  val draw_thick :
    ?style:Path.joint ->
    ?boxed:bool ->
   ?line_color:Color.t ->
    ?fill_color:Color.t ->
    ?outd:Path.direction ->
    ?ind:Path.direction ->
    ?width:Num.t ->
    ?head_length:Num.t ->
    ?head_width:Num.t -> Point.t -> Point.t -> Command.t
end

and ExtArrow : sig
  (** Draw simple or complex arrows. *)

  (** To draw an arrow, choose your arrow [kind], then call the [draw] function
      (giving the path that the arrow will follow) or the [draw2] function
      (giving the starting and ending points of the arrow). If your favorite
      arrow [kind] does not exist, use the tools from this module to build your
      own! *)

  (** {2 Bodies} *)

  type body
    (** The abstract type for arrow bodies *)

  (** {3 Building Your Own Body} *)

  (** Start from the empty body [body_empty] and add features to it using
      [add_line]. *)

  val body_empty: body
    (** The invisible body. *)

  val add_line: ?dashed: Dash.t -> ?color: Color.t -> ?pen: Pen.t ->
    ?from_point: float -> ?to_point: float -> ?dist: Num.t -> body -> body
    (** Add a line to a body. The line will be parallel to the path used
        to draw the arrow.
        @param dashed the dash style used to draw the line (default is plain)
        @param color the color of the line (default is black)
        @param pen the pen used to draw the line (default is {!Pen.default})
        @param from_point from [0.] (foot of the arrow) to [1.] (head of the
          arrow), the line will start from this point
        @param to_point from [0.] (foot of the arrow) to [1.] (head of the
          arrow), the line will end at this point
        @param dist the distance between the path of the arrow and this line
          (may be negative) *)

  (** {3 Built-in Bodies} *)

  val body_simple: body
    (** A simple body with one line. *)

  val body_double: body
    (** A body with two parallel lines. *)

  (** {2 Heads} *)

  type head = Point.t -> Point.t -> Command.t * Path.t
    (** If [h] is a head, [h p d] returns [c, p] where [c] is a command that can
        be used to draw the head at point [p] with direction [d], and [p] is a
        path that can be used to cut the arrow lines. [d] is normalized before
        being given to the function. *)

  val head_classic : ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t ->
    ?angle:float -> ?size:Num.t -> head
  (** A simple head with two straight lines.
      @param color the color of the head; default is black
      @param pen the pen used to draw the head; default is {!Pen.default}
      @param dashed if given, the head is drawn using that dash_style
      @param angle the angle between the two lines in degrees, default is 60
        degrees
      @param size the length of the two lines, default is 4bp *)

  (** {2 Arrow Kinds} *)

  type kind
    (** The abstract type for arrow kinds *)

  (** {3 Buildind Your Own Kind} *)

  (** Start from your favorite body, build an empty kind from it using
      [kind_empty], and then add arrow heads to it. *)

  val add_head: ?head: head -> kind -> kind
    (** Add a head at the end of the arrow.
        @param head the kind of head to add (default is {!head_classic}) *)

  val add_foot: ?head: head -> kind -> kind
    (** Add a foot (an inverted head) at the beginning of the arrow.
        @param head the kind of head to add (default is {!head_classic}) *)

  val add_belt: ?clip: bool -> ?rev: bool -> ?point: float -> ?head: head ->
    kind -> kind
    (** Add an arrow head at any point of an arrow.
        @param clip if [true], the arrow lines will be clipped after the belt
          (or before if the [rev] is [true]) (default is [false])
        @param rev if [true], the head will be drawn in the opposite direction
          (default is [false])
        @param point the point where to draw the arrow ([0.] for the beginning,
          and [1.] for the end, or any number in-between) (default is [0.5])
        @param head the kind of head to add (default is {!head_classic}) *)

  (** {3 Built-in Kinds} *)

  val simple: kind
    (** A simple arrow with one line and two straight lines for the head. *)

  val double: kind
    (** An arrow with two parallel lines and two straight lines for the head.
        Can be used for logical implications, for instance. *)

  (** {2 Drawing Arrows} *)

  val draw: ?kind: kind -> Path.t -> Command.t
    (** Draw an arrow following the given path.
        @param kind the kind of arrow (default is {!simple}) *)

  val draw2: ?kind: kind -> ?outd: Path.direction -> ?ind: Path.direction ->
    Point.t -> Point.t -> Command.t
    (** Use [draw2 a b] to draw an arrow from [a] to [b].
        @param kind the kind of arrow (default is {!simple})
        @param outd the outgoing direction, at the beginning of the arrow
        @param ind the ingoing direction, at the end of the arrow *)
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
  type hposition = [`Center | `Left | `Right]
  type vposition = [`Center | `Top | `Bot]
  type position = [
  | hposition 
  | vposition 
  | `Upleft
  | `Upright
  | `Lowleft
  | `Lowright
  ]

  (** [label ~pos:`Left pic p] puts picture [pic] at the left of point [p] *)
  val label : ?pos:position -> Picture.t -> Point.t -> t

  (** Works like [label], but puts a dot at point [p] as well *)
  val dotlabel : ?pos:position -> Picture.t -> Point.t -> t

end

module  Pos : sig
  (** This module consists of several functors for generic placement of objects.
    Instantiations with the {!Picture} module exist in other places of Mlpost.
   *)

  (** {2 Placing requirements } *)

  module type POS =
  sig
    type t
    (** the type of objects that can be positioned. *)

    type repr
    (** the type of objects once they have been placed. Often this will be the
      same type as [t]. *)

    val ctr : t -> Point.t
    (** return the center of the object *)

    val height : t -> Num.t
    (** return the height of the object *)

    val width : t -> Num.t
    (** return the width of the object *)

    val shift : Point.t -> repr -> repr
    (** [shift pt x] shifts the object [x] about the point [pt]  *)

    val center : Point.t -> t -> repr
    (** [center pt x] centers the object [x] at the point [pt]  *)

    val v : t -> repr
    (** get the "raw" object back *)
  end
  (** The signature [POS] describes the requirements for positionnable objects
   *)

  (** {2 Alignment of sequences} *)

  module type SEQ =
  sig
    module P : POS
    (** The input module of signature {!POS} *)

    type 'a seq
    (** The container sequence type *)

    include POS with type repr = P.repr seq
    (** A sequence can also be positioned. *)

    val horizontal :
      ?padding:Num.t -> ?pos:Command.vposition -> P.t seq -> t
    (** Align the input objects horizontally and return the sequence of their
      representations. *)

    val vertical :
      ?padding:Num.t -> ?pos:Command.hposition -> P.t seq -> t
    (** Align the input objects vertically and return the sequence of their
      representations. *)

    val tabular :
      ?hpadding:Num.t -> ?vpadding:Num.t -> ?pos:Command.position -> 
      P.t seq seq -> t seq
    (** Align the input objects in a table and return the table of their
      representations. *)

  end
  (** This signature describes the output type of the {!List_} and {!Array_}
   functors.  *)

  module List_ : 
    functor (P : POS) -> SEQ with type 'a seq = 'a list and module P = P 
  (** Use this functor to align lists of objects *)

  module Array_ : 
    functor (P : POS) -> SEQ with type 'a seq = 'a array and module P = P 
  (** Use this functor to align arrays of objects *)

  (** {2 Tree placement } *)

  (** The type of trees *)
  type 'a tree = N of 'a * 'a tree list

  module type TREE =
  sig
    module P : POS
    (** The input module of signature {!POS} *)

    include POS with type repr = P.repr tree
    (** Positioned trees can be repositioned using one of the other functors. *)

    val place : ?dx:Num.t -> ?dy:Num.t -> P.t tree -> t
    (** Position a tree. *)

  end
  (** The output signature of the {!Tree} functor.  *)

  module Tree : functor (P : POS) -> TREE with module P = P
  (** Use this functor to position trees.  *)

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
(***
  val hboxjoin : 
    ?color:Color.t -> ?pen:Pen.t -> ?dashed:Dash.t ->
    ?dx:Num.t -> ?dy:Num.t -> ?pos:Command.position -> ?spacing:Num.t -> 
     Picture.t list -> Command.t
***)

end


module Tree : sig

  (** This module provides high-level means for creating and drawing Trees *)

  type t
    (** The abstract type of trees *)

  (** {2 Creation} *)
  val leaf : Box.t -> t
    (** [leaf label] creates a leaf with label [label]. 
	@param style a [node_style] describing how the leaf should be drawn
	@param fill if present, this color is used to fill the leaf *)

  val node : Box.t -> t list -> t
    (** [node label children] creates a node with label [label] and a list
	of children [children]. 
	Optional arguments are the same as in [leaf]. *)

  val bin  : Box.t -> t -> t -> t
    (** [bin label l r] creates a binary node with label [label] and 
	children [l] and [r].
	Optional arguments are the same as in [leaf]. *)

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
    ?arrow_style:arrow_style -> 
    ?edge_style:edge_style ->
    ?stroke:Color.t -> ?pen:Pen.t ->
    ?ls:Num.t -> ?cs:Num.t -> 
    t -> Command.t
    (** Default arrow_style is [Directed].
	Default edge_style is [Straight].

	Drawing parameters are:
	- [ls] (level sep): vertical distance between levels.
        The default value is 1.0. A negative value draws the tree upward.
	- [cs] (children sep): horizontal distance between siblings.
        The default value is 0.2.
    *)

  val set_fill : Color.t -> t -> t

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

  type node_style = Picture.t -> Box.t
  (** The type for node styles; It corresponds to the type of the box
    creation functions in the {!Box} module *)

  val node : ?style:node_style -> ?fill:Color.t -> ?boxed:bool ->
                float -> float -> string -> node
    (** Construct a node at a given position with a given content in Latex
        format and a box style *)

  val pic_node : ?style:node_style -> ?fill:Color.t -> ?boxed:bool ->
                   float -> float -> Picture.t -> node
    (** Construct a node at a given position with a given picture in it *)

  type t
    (** The abstract type of diagrams *)

  val create : node list -> t
    (** Create a diagram that consists of the given nodes *)

  type dir = Up | Down | Left | Right | Angle of float

  val arrow : 
    t -> ?lab:string -> 
    ?line_width:Num.t ->
    ?boxed:bool ->
    ?line_color:Color.t ->
    ?fill_color:Color.t ->
    ?pos:Command.position ->  
    ?head:bool -> ?dashed:Dash.t -> ?outd:dir -> ?ind:dir -> 
    node -> node -> unit
    (** [arrow d n1 n2] adds an arrow between n1 and n2 in the diagram d, by
        side effect.
	@param lab The label of the arrow, in Latex format
	@param pos The position of the label, relative to the arrow
	@param line_width Draws a thick arrow of that width, if present (experimental)
	@param head If true, the arrow has a head. Otherwise, it's just a line.
	@param outd The outgoing direction of the arrow
	@param ind The ingoing direction of the arrow *)

  (** {2 Drawing} *)

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

  type labels = int -> Num.t -> Picture.t option
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

  type t
  val bpath : t -> Path.t

  val rounded_rect_path : Num.t -> Num.t -> Num.t -> Num.t -> t

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

(*
  val arc_ellipse :
    ?fill:Color.t -> ?stroke:Color.t -> ?thickness:float -> ?close:bool ->
    Num.t -> Num.t -> float -> float -> Picture.t
*)
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

  (* POS compliance *)
  type repr = t
  val v : t -> repr
  val ctr : t -> Point.t
  val height : t -> Num.t
  val width : t -> Num.t
  val shift : Point.t -> repr -> repr
  val center : Point.t -> t -> repr

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
      [List.fold_left f acc [i; i +1; .. j] ],
     where i <= j *)
end
(**/**)

module Metapost : sig

  val generate_mp :
    string ->
    ?prelude:string ->
    ?eps:bool ->
    (int * Command.t list) list -> unit

  val emit : string -> Command.t list -> unit
  val dump : ?prelude:string -> ?pdf:bool -> ?eps:bool -> string -> unit
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

