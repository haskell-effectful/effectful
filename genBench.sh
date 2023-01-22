#!/bin/sh

cabal run bench -- --pattern '$2 == "countdown" && $3 == "1000"' --svg bench_countdown_1000.svg
cabal run bench -- --pattern '$2 == "countdown" && $3 == "2000"' --svg bench_countdown_2000.svg
cabal run bench -- --pattern '$2 == "countdown" && $3 == "3000"' --svg bench_countdown_3000.svg
cabal run bench -- --pattern '$2 == "filesize"  && $3 == "1000"' --svg bench_filesize_1000.svg
cabal run bench -- --pattern '$2 == "filesize"  && $3 == "2000"' --svg bench_filesize_2000.svg
cabal run bench -- --pattern '$2 == "filesize"  && $3 == "3000"' --svg bench_filesize_3000.svg

convert bench_countdown_1000.svg bench_countdown_1000.png
convert bench_countdown_2000.svg bench_countdown_2000.png
convert bench_countdown_3000.svg bench_countdown_3000.png
convert bench_filesize_1000.svg  bench_filesize_1000.png
convert bench_filesize_2000.svg  bench_filesize_2000.png
convert bench_filesize_3000.svg  bench_filesize_3000.png

rm bench_countdown_1000.svg
rm bench_countdown_2000.svg
rm bench_countdown_3000.svg
rm bench_filesize_1000.svg
rm bench_filesize_2000.svg
rm bench_filesize_3000.svg
