set terminal png
set grid
set key off

set xlabel 'Temp'
set ylabel 'Magnetization'
set title 'Magnetization'
set output 'MagVsT.png'
plot 'MagVsT.txt'
