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
