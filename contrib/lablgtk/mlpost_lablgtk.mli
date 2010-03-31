(** {1} Abstract lablgtk in order to display Mlpost figures inside a very
    simple interface *)

(** {2} Interfaces *)
val create_text : ?label:string -> (unit -> string) -> (string -> unit) -> unit
  (** create_text ~label get set add to the interface window a text
      input. get must return the initial value, set is called each
      times the value of the text input is changed. *)

val create_option : ?label:string -> (string * (unit ->  unit)) list -> unit
  (** create_option ~label value_list add to the interface window a
      radio menu item. value_list is a pair of one of the choice and
      the callback function used when this choice is selected. *)


(** {2} Required function *)

val set_fig : (unit -> Mlpost.Command.t) -> unit
  (** set_fig get_fig set the function which return the Mlpost figure
      to display. get_fig is called each times the window must be
      refreshed. If the value of one of the interfaces is changed, the
      displayed figure is refreshed.*)


val main : unit -> unit
  (** Start the main loop. During the main loop some interfaces can be
      added and the set_fig can be called *)
