edu.stats.caller <- function(folder.path){
  # EXP087 - Colony Clone Size Distribution Calculator
  # By Ajay Bhargava
  # 08/02/20
  # Function takes a folder path and computes from previously segmented data the clonal statistics table
  options(warn=-1)
  source('./src/R-analysis-scripts/colony-coordinate-preprocessor.R')
  source('./src/R-analysis-scripts/EdU-Distances-Function.R')
  source('./src/R-analysis-scripts/colony-area-centroid-analysis.R')
  source('./src/R-analysis-scripts/perimeter-growth-boundary.R')

  #colony.coordinate.preprocessor(folder.path)
  df1 <- colony.area.centroid.analysis(folder.path)
  df2 <- edu.distance.boundary(folder.path)
  df3 <- perimeter.growth.boundary(folder.path)
  data <- inner_join(df1, df2)

  return (inner_join(data, df3))
}
