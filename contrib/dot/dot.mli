open Mlpost

module type Box =
sig
  type abstract
  type concrete
  val width : abstract -> Num.t
  val height : abstract -> Num.t
  val center : Point.t -> abstract -> concrete
end

module Make (B : Box) :
sig
  type node      
  type edge = node * node

  val mknode : B.abstract -> node

  val mkedge : node -> node -> edge

  val mkedges : (node * node) list -> edge list

  val place : 
    ?orient:[`TB|`LR|`BT|`RL] -> 
    node list -> edge list -> B.concrete list * Path.t list
end
