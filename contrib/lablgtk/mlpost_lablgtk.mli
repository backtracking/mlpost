(** Use Mlpost figures inside gtk interface *) 

(** widget gtk to display one mlpost figure *)
class mlpost_fig : 
  ?width:int -> ?height:int -> ?packing:(GObj.widget -> unit) -> 
  ?show:bool -> unit ->
object 
  inherit GObj.widget
  val obj : Gtk.widget Gtk.obj
  method fig : Mlpost.Command.t
    (** The displayed figure *)
  method set_fig : Mlpost.Command.t -> unit
    (** Sets the figure to display. This function doesn't refresh
       the widget. *)
end

module Interface :
sig
  (** {1 Abstract lablgtk in order to display Mlpost figures inside a very
      simple interface} *)

  type interface
    (** An interface is composed by one control window and by some
        display window *)

  val new_interface : 
    ?width:int -> ?height:int -> ?title:string -> unit -> interface
    (** create a new interface with an empty control window *)
  (** {2 Interfaces} *)

  val create_text : 
    interface -> ?label:string -> string -> (string -> unit) -> unit
    (** [create_text ~label get set] adds to the control window a text
        input. [get] is the initial value, [set] is called each
        times the value of the text input is changed. *)

  val create_option : 
    interface ->
    ?label:string -> (string * (unit ->  unit)) list -> unit
    (** [create_option ~label value_list] adds to the control window a
        radio menu item. [value_list] is a pair of one of the choice and
        the callback function used when this choice is selected. *)


  val remove_fig : interface -> (unit -> Mlpost.Command.t) -> unit
    (** [remove_fig gen_fig] removes a display window created by
        [add_fig gen_fig] *)


    (** {2 Required function} *)
    (** functions needed to see one mlpost figure : *)

  val add_fig : 
    interface -> 
    ?width:int -> ?height:int -> 
    ?title:string -> (unit -> Mlpost.Command.t) -> unit
    (** [add_fig get_fig] add a new display window. [get_fig] is
        called each times the window must be refreshed. If the value
        of one of the interfaces is changed, the displayed figure is
        refreshed.*)


  val main : interface -> unit
    (** Start the main loop. During the main loop some texts or
        options can be added and {!add_fig} can be called *)
end
