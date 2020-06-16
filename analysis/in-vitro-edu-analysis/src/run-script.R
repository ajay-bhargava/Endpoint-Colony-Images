# Set the algorithm in the parent folder of your analysis algorithm
# Runscript Set Build -> 06/15/2020

setwd('/camp/home/bhargaa/working/Ajay/Thesis/Experiments/Endpoint-Colony-Images/analysis/in-vitro-edu-analysis')

# Call the folder path of your data

folder.path <- c('../../shared-assets/edu-analysis-control-images/processed-acqusitions/')

# Call the function

source('./src/R-analysis-scripts/in-vitro-edu-stats-caller.R')

# Invoke the function (takes some time to run)

df <- edu.stats.caller(folder.path)

# Save the Data in the notebooks folder.

saveRDS(df, "/camp/home/bhargaa/working/Ajay/Thesis/Experiments/Endpoint-Colony-Images/shared-assets/edu-analysis-control-images/R-Output/EdU-Analysis-Output.rds")
