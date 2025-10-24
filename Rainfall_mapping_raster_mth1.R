# VISUALIZATION

# load packages
library(tidyverse)
library(lubridate)
library(plotly)
#library(data.table)


# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('D:/gyc/2025/20230922_Water_Profile_WP/Analysis/RF/CHIRPS/viz_month_Labuan')


# data information

reg_name_full <- "Labuan"
reg_name_short <- "labuan"


# year range

yr_min <- 1981
yr_max <- 2024


#########################
# MAPPING

library(sf) # processing spatial vector data
library(sp) # another vector data package necessary for continuity
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
#library(rgdal) # read shapefile
library(mapproj)
library(tmap) # animation

library(gridExtra)

library(viridis)
library(RColorBrewer)
library(scales)

# And a lot of different packages to test their interpolation functions
#library(gstat)  # inverse distance weighted, Kriging
#library(fields) # Thin Plate Spline
#library(automap)# Automatic approach to Kriging


# import monthly raster tif
dir <- "D:/gyc/2025/20230922_Water_Profile_WP/Analysis/RF/CHIRPS/Labuan_Monthly_Averages"
tiffiles <- list.files(paste0(dir), pattern = ".tif$", full.names = T)

tif_stack <- stack(tiffiles)

# check format
class(tif_stack)


# plot to check
plot(tif_stack)






# map country and coordinate data

## shapefile

boundary_shp <- st_read("D:/gyc/2025/20230922_Water_Profile_WP/GIS/shp_PLab/Labuan_boundary.shp")




## basin

#all_basin_shp <- st_read("D:/GIS_data/DATA_SG_BASIN/Jica_subbasin/jsb_sm5.shp")

#basin_shp <- subset(all_basin_shp, BASINNAME %in% c("Klang", "Langat"))


# check projection

#crs(basin_shp)

crs(boundary_shp)


### change projection to WGS84 (original Kertau)
#basin_shp2 <-st_transform(basin_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
boundary_shp2 <-st_transform(boundary_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))



#crs(basin_shp2)

crs(boundary_shp2)





###############
# LAYOUT MAP


# check max min values
min(tif_stack)
max(tif_stack)


# plot

map <- ggplot() + 
  #geom_sf(data = pm_shp2, fill = "grey", alpha = 0.3, colour = "white") +
  #geom_sf(data = sel_shp2, fill = "orange", alpha = 0.3, colour = "white") +
  #geom_sf(data = basin_sel_shp3, fill = NA, alpha = 0.3, colour = "steelblue3") +
  #geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "steelblue3") +
  geom_sf(data = boundary_shp2, fill = NA, alpha = 0.3, colour = "red") +
  coord_sf(xlim = c(115.1, 115.35), ylim = c(5.18, 5.4)) +
  theme_void()


map



#########################

# CLASSIFIED MAP


## set palette 
#col_pal <- brewer.pal(n = 11, name = "Spectral")
col_pal <- brewer.pal(n = 9, name = "YlGnBu")


## set break
#col_brk <- c(0, 50, 100, 150, 200, 250, 300, 400, 500, 550)
col_brk <- c(0, 50, 100, 150, 200, 250, 300, 350, 400, 430)
#col_brk <- c(0, 100, 200, 300, 400, 500, 550)

up_limit <- max(col_brk)
low_limit <- min(col_brk)


# legend set
lgc <- "lgc1"



#########
# Convert RasterStack to data frame
## to work with geom_raster


# Optional: name layers by month (helps later when faceting)
#names(tif_stack) <- 1:nlayers(tif_stack)

tif_stack_df <- rasterToPoints(tif_stack) %>% as.data.frame()

# Check column names
head(tif_stack_df)


# reshape to long format

tif_stack_df_long <- tif_stack_df %>%
  pivot_longer(
    cols = -c(x, y),          # keep coordinates
    names_to = "month",       # month name (from layer names)
    values_to = "precip"   # precipitation values
  )

str(tif_stack_df_long)


# replace values in month column
tif_stack_df_long <- tif_stack_df_long %>% 
  mutate(month = as.numeric(gsub("precipitation.", "", month)))


#########
# PLOT ONE SAMPLE
### single map - single month

# choose month
mth_name <- "November"
mth_no <- "11"


# subset 
sel_tif <- tif_stack_df_long %>% 
  filter(month == mth_no)

plot(sel_tif)


map_cl_sely <- ggplot() +
  geom_raster(data = sel_tif, 
              aes(x = x, y = y, fill = precip)) +
  scale_fill_stepsn(name = "Rainfall (mm)",
                    #n.breaks = 3, 
                    colours = col_pal,
                    breaks = col_brk,
                    values = rescale(col_brk),
                    limits = c(low_limit, up_limit)) +
  #geom_point(data = RF_y2020, aes(x = Long, y = Lat),
  #           color = 'red', size = 0.5, alpha = 0.5) +
  #geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "white", linewidth = 0.1) +
  geom_sf(data = boundary_shp2, fill = NA, alpha = 1, colour = "red3", linewidth = 0.3) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(color = "grey30", hjust = 0.5),
        legend.title = element_text(size = 12, margin = margin(r = 20, unit = "pt")), #move title to prevent overlap
        legend.position = "bottom",
        legend.text = element_text(size = 10)) +
  labs(title = paste0(reg_name_full, " Long-Term Average Monthly Rainfall"),
       subtitle = mth_name) +
  coord_sf(xlim = c(115.1, 115.35), ylim = c(5.18, 5.4)) +
  guides(fill = guide_colorbar(label.position = "bottom", title.hjust = 0.9, 
                               barwidth = unit(30, "lines"), barheight = unit(0.75, "lines")))

map_cl_sely

#print last plot to file
ggsave(paste0(reg_name_short, "_RF_mth", mth_no, "_", lgc, ".jpg"), dpi = 300,
       width = 6, height = 4, units = "in")




#########

# FACET MAP ALL MONTHS

# Convert month to factor with full month names
tif_stack_df_long$month <- factor(tif_stack_df_long$month, levels = 1:12, labels = month.name)

# Check the data to make sure the months are correctly labeled
head(tif_stack_df_long)


# plot

map_cl_sely <- ggplot() +
  geom_raster(data = tif_stack_df_long, 
              aes(x = x, y = y, fill = precip)) +
  scale_fill_stepsn(name = "Rainfall (mm)",
                    #n.breaks = 3, 
                    colours = col_pal,
                    breaks = col_brk,
                    values = rescale(col_brk),
                    limits = c(low_limit, up_limit)) +
  #geom_point(data = RF_y2020, aes(x = Long, y = Lat),
  #           color = 'red', size = 0.5, alpha = 0.5) +
  #geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "white", linewidth = 0.1) +
  geom_sf(data = boundary_shp2, fill = NA, alpha = 1, colour = "red3", linewidth = 0.3) +
  facet_wrap(~month, ncol = 4) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(color = "grey30", hjust = 0.5),
        legend.title = element_text(size = 12, margin = margin(r = 20, unit = "pt")), #move title to prevent overlap
        legend.position = "bottom",
        legend.text = element_text(size = 10)) +
  labs(title = paste0(reg_name_full, " Long-Term Average Monthly Rainfall (", yr_min, "-", yr_max,")\n")) +
  coord_sf(xlim = c(115.1, 115.35), ylim = c(5.18, 5.4)) +
  guides(fill = guide_colorbar(label.position = "bottom", title.hjust = 0.9, 
                               barwidth = unit(30, "lines"), barheight = unit(0.75, "lines")))

map_cl_sely



#print last plot to file
ggsave(paste0(reg_name_short, "_RF_mth_fct_", lgc, ".jpg"), dpi = 300,
       width = 6, height = 4, units = "in")


## widescreen
ggsave(paste0(reg_name_short, "_RF_mth_fct_ws_", lgc, ".jpg"), dpi = 400,
       width = 20, height = 11.25, units = "in")

## A3
ggsave(paste0(reg_name_short, "_RF_mth_fct_a3_", lgc, ".jpg"), dpi = 400,
       width = 20, height = 14.19, units = "in")


#################
# PRINT LAYOUT
## if needed

library(gridExtra)


# extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(map_cl_sely)


### arrange layout

facet_map <- grid.arrange(grobs = maplist, ncol = 4)

#### title font format
title = grid::textGrob(paste0(reg_name_full, " Long-Term Average Monthly Rainfall (", yr_min, "-", yr_max,")\n"), 
                       gp = grid::gpar(fontsize = 16))

facet_legend_map <- grid.arrange(facet_map, mylegend, 
                                 top = title,
                                 nrow = 2, heights = c(9, 1))


# multiple maps with single particular map
#facet_all_map <- grid.arrange(facet_map, map_rf_m12, ncol = 2)


### print last plot to file
 
## widescreen
ggsave(paste0(reg_name_short, "_RF_ltmth_ws_", lgc, "-2.jpg"), facet_legend_map, dpi = 400,
       width = 20, height = 11.25, units = "in")

## A3
ggsave(paste0(reg_name_short, "_RF_ltmth_a3_", lgc, "-2.jpg"), facet_legend_map, dpi = 400,
       width = 20, height = 14.19, units = "in")

## A3 - portrait
ggsave(paste0(reg_name_short, "_RF_ltmth_a3p_", lgc, ".jpg"), facet_legend_map, dpi = 400,
       width = 14.19, height = 20, units = "in")

## template
ggsave(paste0(reg_name_short, "_RF_ltmth_lgc_teml.jpg"), facet_legend_map, dpi = 400,
       width = 35.3, height = 24, units = "cm")
ggsave(paste0(reg_name_short, "_RF_ltmth_lgc_temp.jpg"), facet_legend_map, dpi = 400,
       width = 24.8, height = 35, units = "cm")
	   

#########################

# ANIMATION

library(animation)

a = 1

saveGIF({
  
  for (a in 1:length(maplist)){
    
    plot(maplist[[a]])
    
  }
  
}, movie.name = 'animation.gif', interval = 0.2, ani.width = 700, ani.height = 600)


