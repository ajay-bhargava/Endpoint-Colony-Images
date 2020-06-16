#!/bin/bash

echo "Changing Directory..."
cd $HOME/working/Ajay/Thesis/Experiments/Endpoint-Colony-Images/analysis/in-vitro-edu-analysis/src/

echo "R Loading Clonal Statistics Caller"
Rscript run-script.R --save

echo "Finished, see ../../notebooks/ for data."
