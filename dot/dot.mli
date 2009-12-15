open Mlpost

module type Box =
sig
  type abstract
  type concret
  val width : abstract -> Num.t
  val height : abstract -> Num.t
  val center : Point.t -> abstract -> concret
end

module Make (B : Box) :
sig
  type node      
  type edge

  val mknode : B.abstract -> node

  val mkedge : node -> node -> edge

  val mkedges : (node * node) list -> edge list

  val place : node list -> edge list -> B.concret list * Path.t list
end
