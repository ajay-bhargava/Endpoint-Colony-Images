### R, contour graph, EXP32_Colony_Distribution				        ###
### Ajay Bhargava													###
###	28/04/2018														###

rm(list=ls())

#Load image processing toolkit
library("EBImage")
library("tidyverse")
library("gridExtra")

path <- "/Users/bhargaa/Desktop"

files <- list.files(path, pattern = "*.tif", full.names = TRUE, recursive = TRUE)

img <- list()

for (x in 1:length(files)){
	img[[x]] <- readImage(files[[x]])
}

#Image ID


plot <- list()

for (i in 1:length(files)){

	#Counter

	x = i

	#Segmentation

	normalize <- normalize(img[[x]])

	#display(normalize(normalize), title = "Normalize")

	opening <- closing(normalize, makeBrush(1, shape='box'))

	#display(normalize(opening), title = "Opening Mask")

	label <- bwlabel(normalize(thresh(opening, w=10, h=5, offset=0.1)))

	#display(colorLabels(label))

	#Finish


	#Extraction of objects

	ft <- as.tibble(computeFeatures.moment(label, img[[x]]))

	data <- tibble(x = ft$m.cx, y=ft$m.cy)

	data_raster <- data %>% transmute(x_R = x + 2523/2, y_R = 2523/2 - y)

	#Plotting Data

	plot <- ggplot(data.raster, aes(x=x_R, y=y_R)) + geom_hex(bins = 150) + theme(legend.title=element_blank()) + coord_fixed(ratio = 1)
	plot <- plot + scale_fill_distiller(palette= "Spectral", direction=1)
	plot <- plot + theme_void() + theme(plot.background = element_rect(fill = "transparent", colour = NA)) + guides(fill = guide_colorbar(barwidth = 0.25, barheight = 2)) + guides(fill = guide_colorbar(ticks = FALSE))
	plot <- plot + annotate("path", x=2523+1261*cos(seq(0,2*pi,length.out=100)), y=0+1261*sin(seq(0,2*pi,length.out=100)))
}

final <- do.call("grid.arrange", c(plot, ncol=10))

ggsave("topology_map.png", plot = final, device = "png", scale = 1, width = 30, height = 11, units = c("in"), dpi = 320, path=path_out)


plot <- ggplot(data.raster, aes(x=x_R, y=y_R)) + geom_hex(bins = 150) + theme(legend.title=element_blank()) + coord_fixed(ratio = 1)
plot <- plot + scale_fill_distiller(palette= "Spectral", direction=1)
plot <- plot + theme_void() + theme(plot.background = element_rect(fill = "transparent", colour = NA)) + guides(fill = guide_colorbar(barwidth = 0.25, barheight = 2)) + guides(fill = guide_colorbar(ticks = FALSE))
# plot <- plot + annotate("path", x=2523+1261*cos(seq(0,2*pi,length.out=100)), y=0+1261*sin(seq(0,2*pi,length.out=100)))
