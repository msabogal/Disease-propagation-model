set multiplot
set title 'Expansión de la enfermedad; 1^{os} y 2^{os} vecinos (0.75,0.4); 1^{a} y 2^{a} inmunización (0.33,0.9)' font ",14" 
set xlabel 'Numero de días (iteración)' font ",13" 
set ylabel 'Numero de personas' font ",13" 
set tics font "Helvetica,10"
set origin 0,0
set grid
set size 1,1
set key at 54.5, 25000 font ",12" box on
plot 'expansion.dat' w lp title 'Enfermos','' u 1:4 w lp title'Muertos', '' u 1:3 w lp title 'Inmunes'
unset title
unset xlabel 
unset ylabel
set origin 0.525, 0.18
set size 0.45, 0.45
set tics font "Helvetica,8"
unset grid
unset key
plot 'expansion.dat' u 1:2 w lp, '' u 1:4 w lp
unset multiplot

