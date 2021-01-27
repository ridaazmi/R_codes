###############Classification methods using R####################
##############Geoinfo4all.com ####################################
##############################Rida Azmi & AMAR Hicham ###########
####################### Package installation #####################
pkgs <- c("ggplot2","rgdal","rgeos","sf", "tidyverse", "classInt", "viridis", "readxl","cowplot")
install.packages(pkgs)
#######################declaration of libraries #####################
library(ggplot2)
library(rgdal) 
library(rgeos)
library(sf)
library(tidyverse)
library(classInt) 
library(viridis)
library(readxl)
library(cowplot) # for plot_grid function

###################Import SHP file with attribute data##################
#reading shapefile of Al Haouz Data
al_haouz <- st_read("F:/WORDPRESS/Classification Methods/Article3-Data-txt_modif/Data-commune.shp")

###################visualization of data suing ggplot package ##################

#Plotting the SHP file using ggplot package
ggplot() + 
  geom_sf(data = al_haouz, size = 1, color = "black", fill = "cyan1") + 
  ggtitle("Al Haouz province, Morocco") + 
  coord_sf()

###########Histogram of the population data
hist (al_haouz$Population,  breaks=5)

###########Manual classification #################
bk_manual_clas <- c(20000,5000, 7000, 10000, 15000)
bk_manual_clas

## palette type : BrBG PiYG PRGn PuOr RdBu RdGy RdYlBu RdYlGn
ggplot() +
  geom_sf(data = al_haouz,
          aes(fill = cut(Population, bk_manual_clas, include.lowest = T,dig.lab=10)),
          alpha = 0.8, colour = 'white', size = 0.3) +
  scale_fill_brewer(palette = "BrBG", name = "Population") +
  labs(x = NULL, y = NULL,
       title = paste("Al Haouz population Map 2014 with manual classification"),
       subtitle = "Source: HCP 2014",
       caption = "Contains HCP data ©  copyright and database right (2014)")

##################function to plot the population data using different classification methods 

plot.class <- function(X,type.clss) # X is the number of class and type.clss is classification method 
{
  classes <- classIntervals(al_haouz$Population, n = X, style = type.clss,dig.lab=10)
  bk <- classes$brks
  ggplot() +
    geom_sf(data = al_haouz,
            aes(fill = cut(Population, bk, include.lowest = T,dig.lab=10)),
            alpha = 0.8, colour = 'white', size = 0.3) +
    scale_fill_brewer(palette = "BrBG", name = "Population") +
    labs(x = NULL, y = NULL,
         title = paste("Al Haouz population Map 2014 with",type.clss," classification"),
         subtitle = "Source: HCP 2014",
         caption = "Contains HCP data ©  copyright and database right (2014)")
}

#########Call of the function plot.class

p1 <- plot.class(5,'equal')
p2 <-plot.class(5,'sd')
p3 <-plot.class(5,'jenks')
p4 <-plot.class(5,'quantile')

##############plot the 4 maps in one figure
dev.new()
plot_grid(p1, p2, p3, p4, cols=2)
