set term png
set output "/Users/mike/Dropbox/Oxford/Thesis/wordcount.png"
# set term x11
# set out
set xdata time
set timefmt "%Y-%m-%d"
set xrange ["2014-02-24":"2014-04-06"]
set yrange [0:32000]
set style data lines

set xlabel "Date"
set ylabel "Word count"

plot "/Users/mike/Dropbox/Oxford/Thesis/wordcount" using 1:2 notitle, \
     f(x) = 30000 f(x) notitle
