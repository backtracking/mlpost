beginfig(1)
    pair A, B, C;
    A:=(0,0); B:=(1cm,0); C:=(0,1cm);
    draw A--B--C;
endfig;

beginfig(2)
    pair A, B, C;
    A:=(0,0); B:=(1cm,0); C:=(0,1cm);
    draw A--B--C--cycle;
  
endfig;

beginfig(4)
    pair A;
    A:=(0,0); B:=(1cm,0); C:=(0,1cm);
    draw A withpen pencircle scaled 4bp;
endfig;

beginfig(5)
  pair A, B, C;
  A:=(0,0); B:=(1cm,0); C:=(0,1cm);
  draw A--B--C--cycle;
  draw A withpen pencircle scaled 4bp;
  draw B withpen pencircle scaled 4bp;
  draw C withpen pencircle scaled 4bp;
endfig;

beginfig(12)
    pair A, B, C;
    A:=(0,0); B:=(1cm,0); C:=(0,1cm);
    draw A--B withpen pencircle scaled 2bp withcolor .8white;
    draw B--C withpen pencircle scaled 2bp withcolor .6white;
    draw C--A withpen pencircle scaled 2bp withcolor .4white;
endfig;

beginfig(149)
  def couleur(expr x) =
    if x>.5: (2*(1-x)) [red, blue];
    else:    (1-2x) [blue, red]
    fi
  enddef;
  for i=0 step .5 until 360:
    draw 2cm* (sind(2*i), cosd(3*i))
    withpen pencircle scaled 2bp
    withcolor couleur(i/360);
  endfor;
endfig;