#!/bin/bash

cd ./geometry/

# matlab -nosplash -nodesktop -nojvm -r "Lancio_BSpline; exit;" 
octave --no-gui Lancio_BSpline.m

cd ../

./shpanel
