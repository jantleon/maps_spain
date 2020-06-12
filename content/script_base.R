#0. Prework.

#Install required packages / load if already installed.

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(geojsonio)){
  install.packages("geojsonio")
  library(geojsonio)
}

if(!require(broom)){
  install.packages("broom")
  library(broom)
}

if(!require(mapproj)){
  install.packages("mapproj")
  library(mapproj)
}

if(!require(grid)){
  install.packages("grid")
  library(grid)
}

#Load calibri font.
windowsFonts(Calibri=windowsFont("Calibri"))
update_geom_defaults("text", list(colour = "grey20", family = "Calibri"))

#Colour palette used in the visualization.
palette <- c("#a5d8db", "#d0df99", "#fff59b", "#fbcb8c")

#Create customized theme for graphics.
theme_excel <- function(...) {
  theme_minimal(base_family = "Calibri") +
    theme(legend.position = c(0.94, 0.15),
          axis.line = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.text.align = 0,
          plot.margin = unit(c(.5,.5,0,.5), "cm"),
          panel.spacing = unit(c(2,0.2,.2,0.2), "cm"),
          plot.title=element_text(size=15, hjust = 0.5),
          plot.subtitle = element_text(size=12, hjust = 0.5))
}

## CCAA
# 1. Data load.

#Read geojson (spanish autonomous communities with borders)
spdf_ccaa <- geojson_read("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/spain-communities.geojson", what = "sp")

#Tidy geojson to data frame.
spdf_fortified_ccaa <- tidy(spdf_ccaa, region = "cod_ccaa")

#Read data to plot into maps (DVGD data).
data_ccaa  <- read_delim("content/data/data_ccaa.csv", delim = ";")

#Join tables (geojson + data + centroid).
spdf_fortified_ccaa$id <- as.numeric(spdf_fortified_ccaa$id)
data_ccaa$id <- as.numeric(data_ccaa$id)

data_plot_ccaa <- spdf_fortified_ccaa %>%
  left_join(data_ccaa, by = "id")

#Centroid allow us to plot each data label in the center of the autonomous community.
centroid_ccaa <- read_csv("content/centroids/centroids_ccaa.csv")

data_plot_ccaa <- data_plot_ccaa %>% 
  left_join(centroid_ccaa, by = c("ccaa" = "slug"))

#2. Data transformation. 

#Mutate Canarias (by default is too far from the peninsula)
data_plot_ccaa <- data_plot_ccaa %>%
  mutate(lat_c = ifelse(lat <35, lat + 7.3, lat),
         long_c = ifelse(long <(-13), long + 4, long))

#Canaries line.
canaries_line <- data.frame(long = c(-15, -8.5, -8.5),
                            lat = c(37, 37, 35))

#Create function map_ccaa to make code cleaner.
map_ccaa <- function(variable, title, subtitle, fuente, total, outputfilename) {
#Get quantiles, minimum and maximum to divide data into discrete groups.
quantiles <- unname(quantile(data_ccaa[[variable]], probs = c(.25,.5,.75), na.rm = TRUE))
minVal <- min(data_ccaa[[variable]], na.rm = T)
maxVal <- max(data_ccaa[[variable]], na.rm = T)
brks_ccaa <- c(minVal, quantiles, maxVal)

#Create group labels.
labels <- c()

for(i in 1:length(brks_ccaa)){
  labels <- c(labels,round(brks_ccaa[i + 1], 0))
}

labels <- labels[1:length(labels)-1]

data_plot_ccaa$brks_ccaa <- cut(data_plot_ccaa[[variable]], 
                       breaks = brks_ccaa, 
                       include.lowest = TRUE, 
                       labels = labels)

brks_scale <- levels(data_plot_ccaa$brks_ccaa)
labels_scale <- rev(c(paste("Hasta", levels(data_plot_ccaa$brks_ccaa)[1]), paste("De", as.numeric(levels(data_plot_ccaa$brks_ccaa)[1])+1,"a", levels(data_plot_ccaa$brks_ccaa)[2]), paste("De", as.numeric(levels(data_plot_ccaa$brks_ccaa)[2])+1,"a", levels(data_plot_ccaa$brks_ccaa)[3]), paste("Mas de", levels(data_plot_ccaa$brks_ccaa)[3])))

#3. Data visualization
options(scipen=999)

label = textGrob(label = total, x = .95, y = 0.97, 
                 just = c("right", "top"),
                 gp=gpar(fontfamily='Calibri', size = 3, fontface = "bold", color = "grey"))

ggplot(data_plot_ccaa)+
  geom_polygon(aes(x=long_c, y = lat_c, group = group, fill=brks_ccaa), color = "grey", size = 0.1) +
  geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL), color = "grey") +
  geom_text(aes(x=longlab, y=latlab, label=.data[[variable]]), size = 4, color = "grey40") +
  annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    scale_fill_manual(
    values = palette, 
    breaks = brks_scale,
    name = "",
    drop = FALSE,
    labels = rev(labels_scale))+
  labs(title = title,
       subtitle = subtitle,
       caption = fuente) +
  theme_excel() +
  ggsave(outputfilename, height = 5.8, width = 8.6)}

## PROVINCIAS
# 1. Data load.

#Read geojson (spanish autonomous communities with borders)
spdf_prov <- geojson_read("https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/spain-provinces.geojson", what = "sp")

#Tidy geojson to data frame.
spdf_fortified_prov <- tidy(spdf_prov, region = "cod_prov")

#Read data to plot into maps.
data_prov  <- read_delim("content/data/data_prov.csv", delim = ";")

#Join tables (geojson + data + centroid).
spdf_fortified_prov$id <- as.numeric(spdf_fortified_prov$id)
data_prov$id <- as.numeric(data_prov$id)

data_plot_prov <- spdf_fortified_prov %>%
  left_join(data_prov, by = "id")

#Centroid allow us to plot each data label in the center of the autonomous community.
centroid_prov <- read_csv("content/centroids/centroids_prov.csv")

data_plot_prov <- data_plot_prov %>% 
  left_join(centroid_prov, by = c("prov" = "slug"))

#2. Data transformation. 

#Mutate Canarias (by default is too far from the peninsula)
data_plot_prov <- data_plot_prov %>%
  mutate(lat_c = ifelse(lat.x <35, lat.x + 7.3, lat.x),
         long_c = ifelse(long <(-13), long + 4, long))

#Canaries line.
canaries_line <- data.frame(long = c(-15, -8.5, -8.5),
                            lat = c(37, 37, 35))

#Create function map_prov to make code cleaner.
map_prov <- function(variable, title, subtitle, fuente, total, outputfilename) {
  #Get quantiles, minimum and maximum to divide data into discrete groups.
  quantiles <- unname(quantile(data_prov[[variable]], probs = c(.25,.5,.75), na.rm = TRUE))
  minVal <- min(data_prov[[variable]], na.rm = T)
  maxVal <- max(data_prov[[variable]], na.rm = T)
  brks_prov <- c(minVal, quantiles, maxVal)
  
  #Create group labels.
  labels <- c()
  
  for(i in 1:length(brks_prov)){
    labels <- c(labels,round(brks_prov[i + 1], 0))
  }
  
  labels <- labels[1:length(labels)-1]
  
  data_plot_prov$brks_prov <- cut(data_plot_prov[[variable]], 
                                  breaks = brks_prov, 
                                  include.lowest = TRUE, 
                                  labels = labels)
  
  brks_scale <- levels(data_plot_prov$brks_prov)
  labels_scale <- rev(c(paste("Hasta", levels(data_plot_prov$brks_prov)[1]), paste("De", as.numeric(levels(data_plot_prov$brks_prov)[1])+1,"a", levels(data_plot_prov$brks_prov)[2]), paste("De", as.numeric(levels(data_plot_prov$brks_prov)[2])+1,"a", levels(data_plot_prov$brks_prov)[3]), paste("Mas de", levels(data_plot_prov$brks_prov)[3])))
  
  #3. Data visualization
  options(scipen=999)
  
  label = textGrob(label = total, x = .95, y = 0.97, 
                   just = c("right", "top"),
                   gp=gpar(fontfamily='Calibri', size = 3, fontface = "bold", color = "grey"))
  
  ggplot(data_plot_prov)+
    geom_polygon(aes(x=long_c, y = lat_c, group = group, fill=brks_prov), color = "grey", size = 0.1) +
    geom_path(data = canaries_line, aes(x=long, y = lat, group = NULL), color = "grey") +
    geom_text(aes(x=lon, y=lat.y, label=.data[[variable]]), size = 4, color = "grey40") +
    annotation_custom(label, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    scale_fill_manual(
      values = palette, 
      breaks = brks_scale,
      name = "",
      drop = FALSE,
      labels = rev(labels_scale))+
    labs(title = title,
         subtitle = subtitle,
         caption = fuente) +
    theme_excel() +
    ggsave(outputfilename, height = 5.8, width = 8.6)}