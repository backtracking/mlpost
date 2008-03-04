input boxes;

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

beginfig(20)
    pair A, B, C;
    A:=(0,0); B:=(1cm,0); C:=(0,1cm);
    fill A--B--C--cycle withcolor .8 white;
endfig;

beginfig(21)
    pair A, B, C;
    A:=(0,0); B:=(1cm,0); C:=(0,1cm);
    fill A--B--C--cycle withcolor .8 white;
    draw A--B--C--cycle;  
endfig;

beginfig(22)
  pair A, B, C;
  A:=(0,0); B:=(1cm,0); C:=(0,1cm);
  fill A--B--C--cycle withcolor .8 white;
  draw A--B--C--cycle withpen pencircle scaled 2bp;
endfig;

beginfig(23)
  pair A, B, C;
  A:=(0,0); B:=(1cm,0); C:=(0,1cm);
  draw A--B--C--cycle withpen pencircle scaled 2bp;
  fill A--B--C--cycle withcolor .8 white;
endfig;

beginfig(130)
  transform T;
  u:=1cm;
  z0=(0,0); z1=(2u,0); z3 = z1 rotated 90; z2 = z1+z3;
  z0 transformed T = .1[z0,z1];
  z1 transformed T = .1[z1,z2];
  z2 transformed T = .1[z2,z3];
  path p;
  p = z0--z1--z2--z3--cycle;
  for i=0 upto 50:
    fill p withcolor .8*white;
    p := p transformed T;
    fill p withcolor white;
    p := p transformed T;
  endfor;
endfig;

beginfig(140)
  for i=1 step -.01 until 0:
    fill fullcircle scaled (i*2cm) withcolor i*white;
  endfor;
  draw fullcircle scaled 2cm withpen pencircle scaled 2bp;
endfig;

% beginfig(141)
%   u:=5mm;
%   path p;
%   p = (0,0) .. (-1,1) .. (2,0) .. (0,-3) .. cycle;
%   p := p shifted (-1,0);
%   for i=1 step -.01 until 0:
%     fill p scaled (i*u) withcolor i*white;
%   endfor;
%   draw p scaled u withpen pencircle scaled 2bp;
% endfig;

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

beginfig(195)
  n:=8;
  u:=5mm;
  for i=0 upto n-1:
    for j=0 upto n-1:
      if odd(i+j):
        for k=0 step u/5 until 4/5 u:
          if odd(k*5/u):
            fill ( (k,0)--(u,u-k)--(u,u-k-u/5)--
                   (k+u/5,0)--cycle )
                 shifted (i*u,j*u) withcolor .8white;
          else:
            fill ( (0,k)--(u-k,u)--(u-k-u/5,u)--
                   (0,k+u/5)--cycle )
                 shifted (i*u,j*u) withcolor .8white;
          fi;
        endfor;
      fi;
    endfor;
  endfor;
  for i=0 upto n:
    draw (0,i*u)--(n*u,i*u);
    draw (i*u,0)--(i*u,n*u);
  endfor;
endfig;

beginfig(267)
  def drawredboxed(text t) =
    forsuffixes a=t:
      fill bpath a withcolor .5[red,white];
    endfor;
    drawboxed(t);
  enddef;

  circleit.a(btex D\'ebut etex);
  a.c = (0,0);
  a.dx = a.dy;
  circleit.b(btex Fin etex);
  b.c = (2cm,0);
  b.dx = b.dy;
  drawredboxed(a,b);
  drawarrow  a.c {dir 45} .. b.c {dir -45}
  cutbefore bpath.a cutafter bpath.b;
  drawarrow  b.c {dir -135} .. a.c {dir 135}
  cutbefore bpath.b cutafter bpath.a;
  
endfig;
