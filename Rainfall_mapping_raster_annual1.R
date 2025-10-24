# VISUALIZATION

# load packages
library(tidyverse)
library(lubridate)
library(plotly)
#library(data.table)

# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('D:/gyc/2025/20230922_Water_Profile_WP/Analysis/RF/CHIRPS/viz_Labuan_annual')


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
# library(gstat)  # inverse distance weighted, Kriging
# library(fields) # Thin Plate Spline
# library(automap)# Automatic approach to Kriging




# import monthly raster tif
dir <- "D:/gyc/2025/20230922_Water_Profile_WP/Analysis/RF/CHIRPS/Labuan_Annual_Rainfall_Maps"
tiffiles <- list.files(paste0(dir), pattern = ".tif$", full.names = T)


# convert to RasterStack
tif_stack <- stack(tiffiles)


# check format
class(tif_stack)


# plot to check
plot(tif_stack)


# Rename the layers according to the original filenames
split_result <- strsplit(tiffiles, "[/\\s_\\s.]+")
last_element <- sapply(split_result, `[`, 20)

names(tif_stack) <- last_element



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


map <- ggplot() + 
  #geom_sf(data = pm_shp2, fill = "grey", alpha = 0.3, colour = "white") +
  #geom_sf(data = sel_shp2, fill = "orange", alpha = 0.3, colour = "white") +
  #geom_sf(data = basin_sel_shp3, fill = NA, alpha = 0.3, colour = "steelblue3") +
  #geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "steelblue3") +
  geom_sf(data = boundary_shp2, fill = NA, alpha = 0.3, colour = "red") +
  #geom_point(data = RF_stn, aes(x = Long, y = Lat),
  #           color = 'black', size = 1, alpha = 0.5) +
  coord_sf(xlim = c(115.1, 115.35), ylim = c(5.18, 5.4)) +
  theme_void()


map


#########################

# CLASSIFIED MAP



## set palette 
#col_pal <- brewer.pal(n = 11, name = "Spectral")
#col_pal <- brewer.pal(n = 7, name = "YlGnBu")

## new palette n = 12
#col_pal <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#9AE0BD", "#7FCDBB", "#41B6C4", "#1D91C0", "#1D72B7", "#225EA8", "#253494", "#1A2A78", "#081D58")

## new palette n = 11
col_pal <- c("#FFFFD9", "#C7E9B4", "#9AE0BD", "#7FCDBB", "#41B6C4", "#1D91C0", "#1D72B7", "#225EA8", "#253494", "#1A2A78", "#081D58")


## set break
#col_brk <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000) #n = 9
#col_brk <- c(500, 1000, 1500, 2000, 2500, 3000, 3500, 4000) #n = 7
col_brk <- c(2000, 2250, 2500, 2750, 3000, 3250, 3500, 3750, 4000, 4250, 4500, 4750)  #lgc1, n = 11
#col_brk <- c(1000, 1250, 1500, 1750, 2000, 2250, 2500, 2750, 3000, 3250, 3500, 4000) #n=11
#col_brk <- seq(2100, 2900, by = 100) #n=8
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
    names_to = "year",       # year 
    values_to = "precip"   # precipitation values
  )

str(tif_stack_df_long)


# replace values in year column
tif_stack_df_long <- tif_stack_df_long %>% 
  mutate(year = as.numeric(gsub("X", "", year)))


#########
# PLOT ONE SAMPLE
### single map - ANNUAL AVERAGE

# import annual avg raster tif
tif_avg <- raster(paste0("D:/gyc/2025/20230922_Water_Profile_WP/Analysis/RF/CHIRPS/Labuan_CHIRPS_1981_2024.tif"))


# check format
class(tif_avg)


# plot to check
plot(tif_avg)


###
# IF select one year
# choose year
yr_no <- "2024"


# subset 
sel_tif <- tif_stack_df_long %>% 
  filter(year == yr_no)

plot(sel_tif)

###


map_cl_sely <- ggplot() +
  geom_raster(data = as.data.frame(tif_avg, xy = TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = precipitation)) +
  scale_fill_stepsn(name = "Rainfall (mm)",
                    #n.breaks = 3, 
                    colours = col_pal,
                    breaks = col_brk,
                    values = rescale(col_brk),
                    limits = c(low_limit, up_limit)) +
  #geom_point(data = as.data.frame(RF_sely$Depth_yr_avg), aes(x = RF_sely$Long, y = RF_sely$Lat),
  #           color = 'red', size = 0.5, alpha = 0.5) +
  #geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "white", linewidth = 0.5) +
  geom_sf(data = boundary_shp2, fill = NA, alpha = 1, colour = "red3", linewidth = 0.5) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(color = "grey30", hjust = 0.5),
        legend.title = element_text(size = 12, margin = margin(r = 20, unit = "pt")), #move title to prevent overlap
        legend.position = "bottom",
        legend.text = element_text(size = 10)) +
  labs(title = paste0(reg_name_full, " Average Annual Rainfall Distribution"), # yr_no for selected year
       subtitle = paste0("(", yr_min, "-", yr_max, ")")) +
  coord_sf(xlim = c(115.1, 115.35), ylim = c(5.18, 5.4)) +
  guides(fill = guide_colorbar(label.position = "bottom", title.hjust = 0.9, 
                               barwidth = unit(30, "lines"), barheight = unit(0.75, "lines")))

map_cl_sely

#print last plot to file
## selected year
ggsave(paste0(reg_name_short, "_RF_yr_", yr_no, "_", lgc, ".jpg"), dpi = 300,
       width = 6, height = 4, units = "in")

## long-term average annual rainfall
ggsave(paste0(reg_name_short, "_RF_yr_avg_", lgc, ".jpg"), dpi = 300,
       width = 6, height = 4, units = "in")


#########

# FACET MAP ALL MONTHS

# Convert month to factor with full month names
tif_stack_df_long$month <- factor(tif_stack_df_long$month, levels = 1:12, labels = month.name)

# Check the data to make sure the months are correctly labeled
head(tif_stack_df_long)


# plot

map_cl_all <- ggplot() +
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
  facet_wrap(~year, ncol = 10) +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(color = "grey30", hjust = 0.5),
        legend.title = element_text(size = 12, margin = margin(r = 20, unit = "pt")), #move title to prevent overlap
        legend.position = "bottom",
        legend.text = element_text(size = 10)) +
  labs(title = paste0(reg_name_full, " Annual Rainfall (", yr_min, "-", yr_max,")\n")) +
  coord_sf(xlim = c(115.1, 115.35), ylim = c(5.18, 5.4)) +
  guides(fill = guide_colorbar(label.position = "bottom", title.hjust = 0.9, 
                               barwidth = unit(30, "lines"), barheight = unit(0.75, "lines")))

map_cl_all


## widescreen
ggsave(paste0(reg_name_short, "_RF_yr_fct_ws_", lgc, ".jpg"), dpi = 400,
       width = 20, height = 11.25, units = "in")

## A3
ggsave(paste0(reg_name_short, "_RF_yr_fct_a3_", lgc, ".jpg"), dpi = 400,
       width = 20, height = 14.19, units = "in")



#################
# FACET MAP

library(gridExtra)


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}



mylegend_cl <- g_legend(map_cl_sely)



# arrange layout

#facet_map_cl <- grid.arrange(grobs = maplist[1:6], ncol = 3)
facet_map_cl <- grid.arrange(grobs = maplist, ncol = 10)


#### title font format
title2 = grid::textGrob(paste0(reg_name_full, " Annual Rainfall (", yr_min, "-", yr_max,")\n"),  
                        gp = grid::gpar(fontsize = 14))


# layout - all annual

facet_legend_map_cl <- grid.arrange(facet_map_cl, mylegend_cl, 
                                    top = title2, 
                                    nrow = 2, heights = c(9, 1)
                                    #ncol = 2, widths = c(9, 1)
)

#### print last plot to file
## widescreen
ggsave(paste0(reg_name_short, "_RF_annual_ws_", lgc, ".jpg"), facet_legend_map_cl, dpi = 400,
       width = 20, height = 11.25, units = "in")

## A3
ggsave(paste0(reg_name_short, "_RF_annual_a3_", lgc, ".jpg"), facet_legend_map_cl, dpi = 400,
       width = 20, height = 14.19, units = "in")



## template
ggsave(paste0(reg_name_short, "_RF_annual_lgc1_teml.jpg"), facet_legend_map_cl, dpi = 400,
       width = 35.3, height = 24, units = "cm")
ggsave(paste0(reg_name_short, "_RF_annual_lgc1_temp.jpg"), facet_legend_map_cl, dpi = 400,
       width = 24.8, height = 35, units = "cm")




# combined layout - annual & avg
facet_legend_map_cl2 <- grid.arrange(facet_map_cl, map_cl_avg, #mylegend_cl, 
                                     top = title2, 
                                     #nrow = 2, heights = c(9, 1)
                                     ncol = 2, widths = c(7, 3)
)

#### print last plot to file
ggsave(paste0(reg_name_short, "_RF_annual_avg_lgc2.jpg"), facet_legend_map_cl2, dpi = 400,
       width = 20, height = 11.25, units = "in")



#########################

# ANIMATION

library(animation)

a = 1

saveGIF({
  
  for (a in 1:length(maplist)){
    
    plot(maplist[[a]])
    
  }
  
}, movie.name = 'animation.gif', interval = 0.2, ani.width = 700, ani.height = 600)


