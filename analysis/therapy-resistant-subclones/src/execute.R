# Set the algorithm in the parent folder of your analysis algorithm

setwd('/camp/home/bhargaa/working/Ajay/Thesis/Experiments/Subclone-Size-Distribution/')

# Call the folder path of your data

folder.path <- c('./data/processed-acqusitions/')

# Call the function

source('./src/R-analysis-scripts/clonal-statistics-caller.R')

# Invoke the function (takes some time to run)

df <- clonal.statistics(folder.path)

# Save the Data in the notebooks folder.

saveRDS(df, "./data/R-outputs/Distribution-Dataset.rds")
