IFDEF CAIRO THEN
  type matrix = Cairo.matrix =
    { xx : float; yx : float; xy : float; yy : float; x0 : float; y0 : float; }

  type point = Cairo.point = 
      {x : float;
       y : float}
ELSE
  type matrix = 
    { xx : float; yx : float; xy : float; yy : float; x0 : float; y0 : float; }

  type point = {x : float; y : float}
END

