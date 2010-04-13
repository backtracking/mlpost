(** Place figures, boxes or boxlikes with graphviz *)
open Mlpost

module Dot :
sig
  module Make (B : Signature.Boxlike) :
  sig
    type node      
    type edge = node * node

    val mknode : B.t -> node
      (** creates a node from a boxlike *)

    val mkedge : node -> node -> edge
      (** creates an edge between two nodes *)

    val mkedges : (node * node) list -> edge list

    val place : 
      ?orient:[`TB|`LR|`BT|`RL] -> 
      node list -> edge list -> B.t list * Path.t list
      (** place ~orient nodes edges returns the list of all the
      boxlike in nodes placed by dot and the list of path of xedges
      which link the box *)
  end
end
