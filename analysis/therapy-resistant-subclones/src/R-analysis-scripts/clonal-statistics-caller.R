clonal.statistics <- function(folder.path){
  # EXP087 - Colony Clone Size Distribution Calculator
  # By Ajay Bhargava
  # 08/02/20
  # Function takes a folder path and computes from previously segmented data the clonal statistics table
  options(warn=-1)
  source('./src/R-analysis-scripts/colony-coordinate-preprocessor.R')
  source('./src/R-analysis-scripts/colony-area-analysis.R')
  source('./src/R-analysis-scripts/fp-distance-from-boundary-analysis.R')
  source('./src/R-analysis-scripts/fp-edu-per-subclone.R')
  source('./src/R-analysis-scripts/fp-area-analysis.R')
  source('./src/R-analysis-scripts/mean-edu-distance-boundary.R')
  source('./src/R-analysis-scripts/perimeter-growth-boundary.R')

  colony.coordinate.preprocessor(folder.path)
  df1 <- colony.area.analysis(folder.path)
  df2 <- fp.distance.from.boundary.analysis(folder.path)
  df3 <- fp.edu.per.subclone(folder.path)
  df4 <- fp.area.analysis(folder.path)
  df5 <- mean.edu.distance.boundary(folder.path)
  df6 <- perimeter.growth.boundary(folder.path)

  data <- inner_join(df1, df2)
  data <- inner_join(data, df3)
  data <- inner_join(data, df4)
  data <- inner_join(data, df5)
  data <- inner_join(data, df6)

  return (data)
}
