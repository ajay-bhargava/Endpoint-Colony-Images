#!/bin/bash

echo "Changing Directory..."
cd $HOME/working/Ajay/Thesis/Experiments/Subclone-Size-Distribution/src/

echo "R Loading Clonal Statistics Caller"
Rscript execute.R --save

echo "Finished, see ../../notebooks/ for data."
