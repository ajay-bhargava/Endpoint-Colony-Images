#!/bin/bash

echo "Changing Directory..."
cd $HOME/working/Ajay/Thesis/Experiments/Endpoint-Colony-Images/analysis/size-distribution-labeling-days/src/

echo "R Loading Clonal Statistics Caller"
Rscript run-script.R --save

echo "Finished, see ../../notebooks/ for data."
