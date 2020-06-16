spatial.overlay.calculator <- function(edu.list, fp.coordinates, fp.color, fp.id){
  # cbind the fp.coordinates
  library(sp)
  shape.coordinates <- cbind(fp.coordinates$X, fp.coordinates$Y)
  shape.spatial.polygons <- SpatialPolygons(list(Polygons(list(Polygon(shape.coordinates)), 1)))
  # Promote individual fp.coordinates (to Polygon, then SpatialPolygon, then SpatialPolygons)
  edu.per.subclone <- over(shape.spatial.polygons, edu.list, returnList = TRUE)
  data.edu.per.subclone <- length(edu.per.subclone$`1`)
  return (data.frame(Subclone.ID = fp.id, Subclone.Color = fp.color, EdU.per.Subclone = data.edu.per.subclone))
}
