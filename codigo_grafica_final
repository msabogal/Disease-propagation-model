set multiplot
set origin 0,0
set grid
set size 1,1
set key at 40, 25000
plot 'expansion.dat' w lp title 'Enfermos','' u 1:4 w lp title'Muertos', '' u 1:3 w lp title 'Inmunes'
set origin 0.525, 0.2
set size 0.45, 0.45
unset grid
unset key
plot 'expansion.dat' u 1:2 w lp, '' u 1:4 w lp
unset multiplot

