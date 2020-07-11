edu.stats.caller <- function(folder.path){
  # Function takes a folder path and computes from previously segmented data the clonal statistics table
  options(warn=-1)
  source('./src/R-analysis-scripts/spatial-hedgemony-calculator.R')
  source('./src/R-analysis-scripts/colony-area-centroid-analysis.R')
  source('./src/R-analysis-scripts/perimeter-growth-boundary.R')

  #colony.coordinate.preprocessor(folder.path)
  df1 <- colony.area.centroid.analysis(folder.path)
  df2 <- spatial.hedgemony.calculator(folder.path)
  df3 <- perimeter.growth.boundary(folder.path)
  data <- inner_join(df1, df2)

  return (inner_join(data, df3))
}
