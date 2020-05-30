remove.null <- function(x) {
   x <- Filter(Negate(is.null), x)
   lapply(x, function(x) if (is.list(x)) rm.null(x) else x)
}
