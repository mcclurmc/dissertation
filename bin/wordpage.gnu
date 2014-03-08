set term png
set output "/Users/mike/Dropbox/Oxford/Thesis/wordpage.png"
# set term x11
# set out
set style data lines

set xdata time
set timefmt "%Y-%m-%d"
set xrange ["2014-02-24":"2014-04-06"]
set yrange [0:32000]
set y2range [0:80]

set xlabel "Date"
set ylabel "Word count"
set y2label "Page count"
set y2tics
set ytics nomirror

plot "/Users/mike/Dropbox/Oxford/Thesis/wordpage" using 1:2 title "Words" axes x1y1, \
     "/Users/mike/Dropbox/Oxford/Thesis/wordpage" using 1:3 title "Pages" axes x1y2

