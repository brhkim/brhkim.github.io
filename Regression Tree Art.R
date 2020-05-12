################################################################################
#  
#   Name: Regression Tree Art.R
#   Author: Brian Heseung Kim (@brhkim)
#   Date: 20-05-11
#   
#   Purpose: Replicate the artistic style of Dimitris Ladopoulos (@DimitrisLad)
#   by algorithmically "compressing" art, similar to here: 
#
#   https://www.thisiscolossal.com/2018/01/painting-algorithms-dimitris-ladopoulos/
#   
#   Uses regression trees to partition out regions of the image for each
#   color channel and then recompiles the image alongside the regression partitions
#   with help from Grant McDermott's parttree package (@grant_mcdermott).
#
################################################################################

library(tidyverse)
library(rpart)
library(imager)
library(magick)

# Uncomment next two lines if you need to install either remotes package or parttree package
# install.packages("remotes")
# remotes::install_github("grantmcdermott/parttree")
library(parttree)

# Import original image. Can take local files or direct URLs
# as long as in jpeg or png
img <- load.image("https://www.edvardmunch.org/images/paintings/the-scream.jpg")

# Get it into data frame
imgdf <- img %>% as.data.frame()

# Get image dimensions
ymax <- max(imgdf$y)
xmax <- max(imgdf$x)

if (ymax>xmax) {
  imgheight <- 10
  imgwidth <- 10 * (xmax/ymax)
} else {
  imgwidth <- 10
  imgheight <- 10 * (ymax/xmax)
}

# Round color values to ease up work for decision tree
imgdf$value <- imgdf$value %>% round(digits=2)

# Save an image of the original with new dimensions
imgoutput <- imgdf %>% as.cimg() %>% image_read()

ggplot() +
  annotation_raster(imgoutput, ymin=-Inf, ymax=Inf, xmin=-Inf, xmax=Inf, interpolate=TRUE) + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title=element_blank())

ggsave("treecompress_a.jpg", width=imgwidth, height=imgheight)

# Filter out separate dataframes for each color channel
imgdf1 <- imgdf %>% filter(cc==1)
imgdf2 <- imgdf %>% filter(cc==2)
imgdf3 <- imgdf %>% filter(cc==3)

# Get mean color channel values for coloring regression partitions later
color1 <- rgb(mean(imgdf1$value), mean(imgdf2$value), mean(imgdf3$value))
color2 <- rgb(mean(imgdf1$value) + 0.25*sd(imgdf1$value), mean(imgdf2$value) + 0.25*sd(imgdf2$value), mean(imgdf3$value) + 0.25*sd(imgdf3$value))
color3 <- rgb(mean(imgdf1$value) - 0.25*sd(imgdf1$value), mean(imgdf2$value) - 0.25*sd(imgdf2$value), mean(imgdf3$value) + 0.25*sd(imgdf3$value))

# Start creating regression tree for each color channel
# Futz with the CP; larger amounts leads to larger regions and greater compression (e.g. 0.0001).
# Lower amounts lead to smaller regions and lesser compression (e.g. 0.000001.
controls <- rpart.control(minsplit = 5, cp = 0.00001, 
                          maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 5,
                          surrogatestyle = 0, maxdepth = 30)

imgtree1 <- rpart(value ~ x + y, data=imgdf1, control=controls)
imgtree2 <- rpart(value ~ x + y, data=imgdf2, control=controls)
imgtree3 <- rpart(value ~ x + y, data=imgdf3, control=controls)

# Create color predictions
imgdf1$predictions <- predict(imgtree1, imgdf1)
imgdf2$predictions <- predict(imgtree2, imgdf2)
imgdf3$predictions <- predict(imgtree3, imgdf3)

# Put it all back together with correct column headers
imgoutputdf <- rbind(imgdf1, imgdf2, imgdf3)
imgoutputdf <- select(imgoutputdf, c(-value))
imgoutputdf <- rename(imgoutputdf, value = predictions)

imgoutput <- imgoutputdf %>% as.cimg() %>% image_read()

# Create image output with prediction boundaries visible
if (unique(as.character(imgtree1$frame[imgtree1$frame$var != "<leaf>", ]$var))[1]=="x") {
  ggplot() +
    annotation_raster(imgoutput, ymin=-Inf, ymax=Inf, xmin=-Inf, xmax=Inf, interpolate=TRUE) + 
    geom_parttree(data = imgtree1, color=color1, alpha = 0.00001)  +
    geom_parttree(data = imgtree2, color=color2, alpha = 0.00001)  +
    geom_parttree(data = imgtree3, color=color3, alpha = 0.00001)  +
    scale_x_continuous(limits=c(0, max(imgoutputdf$x)), expand=c(0,0)) +
    scale_y_reverse(limits=c(max(imgoutputdf$y), 0), expand=c(0,0)) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title=element_blank())
} else {
  # Note: Parttree has unexpected behavior if first var value in tree$frame$var is "y"
  # so catch and flip coordinates as necessary
  ggplot() +
    annotation_raster(imgoutput, ymin=-Inf, ymax=Inf, xmin=-Inf, xmax=Inf, interpolate=TRUE) + 
    geom_parttree(data = imgtree1, color=color1, alpha = 0.00001)  +
    geom_parttree(data = imgtree2, color=color2, alpha = 0.00001)  +
    geom_parttree(data = imgtree3, color=color3, alpha = 0.00001)  +
    coord_flip() + 
    scale_x_reverse(limits=c(max(imgoutputdf$y), 0), expand=c(0,0)) +
    scale_y_continuous(limits=c(0, max(imgoutputdf$x)), expand=c(0,0)) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title=element_blank())
}

ggsave("treecompress_b.jpg", width=imgwidth, height=imgheight)


# Create image output without prediction boundaries visible
ggplot() +
  annotation_raster(imgoutput, ymin=-Inf, ymax=Inf, xmin=-Inf, xmax=Inf, interpolate=TRUE) + 
  theme_minimal() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title=element_blank())

ggsave("treecompress_c.jpg", width=imgwidth, height=imgheight)

