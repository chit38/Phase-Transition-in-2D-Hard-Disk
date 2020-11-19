set terminal wxt size 350,262 enhanced font 'Verdana,10' persist
set title "Beta*p/rho Vs rho"
set nokey
set grid
set xlabel "x"
set ylabel "y"
m="hist.txt"
plot m using 1:2 with linespoints