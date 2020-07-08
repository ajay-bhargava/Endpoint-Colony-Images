#!/bin/bash

echo "Changing Directory..."
cd $HOME/working/Ajay/Thesis/Experiments/Endpoint-Colony-Images/analysis/therapy-resistant-subclones/src/

echo "R Loading Clonal Statistics Caller"
Rscript Run-Scripts.R --save

echo "Finished, see ../../notebooks/ for data."
