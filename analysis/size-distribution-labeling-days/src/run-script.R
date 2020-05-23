# Set the algorithm in the parent folder of your analysis algorithm
# Runscript Set Build -> 23/05/20

setwd('/camp/home/bhargaa/working/Ajay/Thesis/Experiments/Endpoint-Colony-Images/analysis/size-distribution-labeling-days/')

# Call the folder path of your data

folder.path <- c('../../shared-assets/size-distribution-labeling-days/processed-acquisitions')

# Call the function

source('./src/R-analysis-scripts/clonal-statistics-caller.R')

# Invoke the function (takes some time to run)

df <- clonal.statistics(folder.path)

# Save the Data in the notebooks folder.

saveRDS(df, "../../shared-assets/size-distribution-labeling-days/R-Output/Size-Distribution-Labeling-Days-Dataset.rds")
