edu.stats.caller <- function(folder.path){
  # EXP087 - Colony Clone Size Distribution Calculator
  # By Ajay Bhargava
  # 08/02/20
  # Function takes a folder path and computes from previously segmented data the clonal statistics table
  options(warn=-1)
  source('./src/R-analysis-scripts/colony-coordinate-preprocessor.R')

  colony.coordinate.preprocessor(folder.path)


  return (data)
}
