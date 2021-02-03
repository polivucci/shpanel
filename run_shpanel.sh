#!/bin/bash

cd ./geometry/

matlab -nosplash -nodesktop -nojvm -r "Lancio_BSpline; exit;" 

cd ../

./shpanel

gnuplot plot.p
