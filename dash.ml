
open Types

type on_off = Types.on_off = On of Num.t | Off of Num.t

type t = Types.dash

let evenly = DEvenly
let withdots = DWithdots
let scaled s d = DScaled (s, d)
let shifted s d = DShifted (s, d)
let pattern l = DPattern l

