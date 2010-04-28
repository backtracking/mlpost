val specials : bool ref

type multi_page_pic = {pic :Cairo.t;
            new_page : unit -> unit;
             x_origin : float;
             y_origin : float
           }

module Cairo_device :
  Dviinterp.dev with type arg = multi_page_pic with type cooked = unit
