(** Use Mlpost figures inside gtk interface *) 

(** widget gtk for displaying mlpost figure *)
class mlpost_fig : 
  ?width:int -> ?height:int -> ?packing:(GObj.widget -> unit) -> 
  ?show:bool -> unit ->
object 
  inherit GObj.widget
  val obj : Gtk.widget Gtk.obj
  method fig : Mlpost.Command.t
  method set_fig : Mlpost.Command.t -> unit
end

module Interface :
sig
  (** {1 Abstract lablgtk in order to display Mlpost figures inside a very
      simple interface} *)

  type interface

  val new_interface : 
    ?width:int -> ?height:int -> ?title:string -> unit -> interface

  (** {2 Interfaces} *)
  val create_text : 
    interface -> ?label:string -> string -> (string -> unit) -> unit
    (** create_text ~label get set add to the interface window a text
        input. get must return the initial value, set is called each
        times the value of the text input is changed. *)

  val create_option : 
    interface ->
    ?label:string -> (string * (unit ->  unit)) list -> unit
    (** create_option ~label value_list add to the interface window a
        radio menu item. value_list is a pair of one of the choice and
        the callback function used when this choice is selected. *)


  val remove_fig : interface -> (unit -> Mlpost.Command.t) -> unit

  (** {2 Required function} *)
  val add_fig : 
    interface -> 
    ?width:int -> ?height:int -> 
    ?title:string -> (unit -> Mlpost.Command.t) -> unit
    (** set_fig get_fig set the function which return the Mlpost figure
        to display. get_fig is called each times the window must be
        refreshed. If the value of one of the interfaces is changed, the
        displayed figure is refreshed.*)


  val main : interface -> unit
    (** Start the main loop. During the main loop some interfaces can be
        added and the set_fig can be called *)
end
