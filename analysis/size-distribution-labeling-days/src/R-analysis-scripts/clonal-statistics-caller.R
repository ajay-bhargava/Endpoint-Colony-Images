clonal.statistics <- function(folder.path){
  # EXP087 - Colony Clone Size Distribution Calculator
  # By Ajay Bhargava
  # 23/05/20
  # Function takes a folder path and computes from previously segmented data the clonal statistics table

  options(warn=-1)
  source('./analysis/size-distribution-labeling-days/src/R-analysis-scripts/colony-area-analysis.R')
  source('./analysis/size-distribution-labeling-days/src/R-analysis-scripts/fp-distance-from-boundary-analysis.R')
  source('./analysis/size-distribution-labeling-days/src/R-analysis-scripts/fp-area-analysis.R')

  df1 <- colony.area.analysis(folder.path)
  df2 <- fp.distance.from.boundary.analysis(folder.path)
  df3 <- fp.area.analysis(folder.path)

  data <- inner_join(df1, df2)
  data <- inner_join(data, df3)

  return (data)
}
