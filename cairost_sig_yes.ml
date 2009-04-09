
module type S = sig
  val emit_cairo : Cairo.t -> Command.t -> unit
end
