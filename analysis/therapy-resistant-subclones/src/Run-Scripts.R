# Set the algorithm in the parent folder of your analysis algorithm

setwd('/camp/home/bhargaa/working/Ajay/Thesis/Experiments/Endpoint-Colony-Images/analysis/therapy-resistant-subclones/')

# Call the folder path of your data

folder.path <- c('../../shared-assets/processed-acquisitions-2/')

# Call the function

source('./src/R-analysis-scripts/clonal-statistics-caller.R')
source('./src/R-analysis-scripts/in-vitro-edu-stats-caller.R')

# Invoke the function (takes some time to run)

df <- clonal.statistics(folder.path)
df2 <- edu.stats.caller(folder.path)
#df3 <- spatial.hedgemony(folder.path)

# Save the Data in the notebooks folder.

saveRDS(df, "../../shared-assets/motility-modulator-data-output/Distribution-Dataset.rds")
saveRDS(df2, "../../shared-assets/motility-modulator-data-output/EdU-Coordinates-Dataset.rds")
#saveRDS(df3, "../../shared-assets/motility-modulator-data-output/Spatial-hedgemony-Dataset.rds")
