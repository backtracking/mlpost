
open Format

type t =
  | Evenly of float option
  | Withdots of float option

let evenly = Evenly None
let evenly_scaled s = Evenly (Some s)
let withdots = Withdots None
let withdots_scaled s = Withdots (Some s)

let print fmt = function
  | Evenly None -> fprintf fmt "evenly"
  | Evenly (Some s) -> fprintf fmt "evenly scaled %a" Num.print_float s
  | Withdots None -> fprintf fmt "withdots"
  | Withdots (Some s) -> fprintf fmt "withdots scaled %a" Num.print_float s


