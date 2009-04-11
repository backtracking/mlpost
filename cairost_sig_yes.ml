
module type S = sig
  val emit_cairo : (float -> float -> Cairo.t) -> Command.t -> unit
end
