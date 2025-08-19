# VISUALIZATION


# load packages
library(tidyverse)


# set strings as factors to false
options(stringsAsFactors = FALSE)


#set working directory
setwd('D:/gyc/2025/20230922_Water_Profile_WP/Analysis/UMT_yield')


# data information

reg_name_full <- "Klang - Langat"
reg_name_short <- "klngt"

#########################
# MAPPING

library(sf) # processing spatial vector data
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
#library(rgdal) # read shapefile #will be retired in 2023, replace



# import data
yield_data <- read.csv("yield_results.csv", 
                     header = T, sep = ",")


# map country and coordinate data

## shapefile
pm_shp <- st_read("D:/GIS_data/JUPEM_topo/PM/Demarcation/DA0040_State_Cover_A_dis.shp")

boundary_shp <- subset(pm_shp, NAM %in% c("WILAYAH PERSEKUTUAN KUALA LUMPUR", "PUTRAJAYA"))

## basin
all_basin_shp <- st_read("D:/GIS_data/DATA_SG_BASIN/Jica_subbasin/jsb_sm5.shp")

basin_shp <- subset(all_basin_shp, BASINNAME %in% c("Klang", "Langat"))


# check projection

crs(boundary_shp)

crs(basin_shp)



### change projection to WGS84 (original Kertau)

basin_shp2 <-st_transform(basin_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

boundary_shp2 <-st_transform(boundary_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))


crs(basin_shp2)
crs(boundary_shp2)




###############
# LAYOUT MAP

map <- ggplot() + 
  geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "steelblue3") +
  geom_sf(data = boundary_shp2, fill = NA, alpha = 0.3, colour = "red") +
  #geom_point(data = RF_stn, aes(x = Long, y = Lat),
  #           color = 'black', size = 1, alpha = 0.5) +
  coord_sf(xlim = c(101.2, 102), ylim = c(2.7, 3.4)) +
  theme_void()


map


#########################
# POLYGON MAP



### set format
str(yield_data)

## rename columns
colnames(yield_data) <- c("Basin", "Year", "Yield")



# divide value into class
yield_data2 <- yield_data %>% 
  mutate(Yield_class = cut(Yield, breaks = c(900, 1000, 2000, 3000, 4000, 5000),
                           labels = c("900-1000", "1001-2000", "2001-3000", "3001-4000", "4001-5000")))


### join data to shp
yield_data_shp <- basin_shp2 %>% 
  full_join(yield_data2, by = c("BASINNAME" = "Basin")) 
  




### set colors by classification

col_class <- c(
  "4001-5000" = "#4083ee", 
  "3001-4000" = "#82c2ff", 
  "2001-3000" = "#BEF7FF", 
  "1001-2000" = "#ffb860", 
  "900-1000" = "#ee0000")




## mapping choropleth
#yield_data_shp_sel <- subset(yield_data_shp, Year %in% 2012)

map_choro <- ggplot() + 
  geom_sf(data = yield_data_shp, 
          mapping = aes(fill = as.factor(Yield_class), 
                        group = as.factor(Yield_class)), 
          color = "white", alpha = 1, show.legend = T) +
  geom_sf(data = boundary_shp2, fill = NA, alpha = 0.3, colour = "red") +
  scale_fill_manual(name = "Yield (MCM)", values = col_class) +
  coord_sf(xlim = c(101.2, 102), ylim = c(2.7, 3.4)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 10), 
        plot.caption = element_text(size = 8, color = "grey50", hjust = 0),
        plot.margin = margin(r = 10, l = 10),
        legend.position = "bottom") +
  annotate("text", x = 111, y = 6.5, size = 4,
           label = "Yield (MCM)") +
  #labs(title = 2012) +
  #labs(caption = "Data source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


map_choro

### print last plot to file
ggsave(paste0(reg_name_short, "_yield_single.jpg"), dpi = 300,
       width = 12, height = 5, units = "in")




#########################
# MULTIPLE MAPS 


# extract date for iteration
un_date <- sort(unique(yield_data$Year)) # make sure date is sorted
df_date <- as.data.frame(un_date)
str(df_date)
colnames(df_date) <- "Year"


# list of maps
maplist <- list()

m = 1

for (m in 1:nrow(df_date)) {
  
  sel_yr <- df_date[m, 1] # start from year 2012
  yield_data_shp_sel <- subset(yield_data_shp, Year %in% sel_yr)
  
  # plot map
  annual_yield_map <- ggplot() + 
    geom_sf(data = yield_data_shp_sel, 
            mapping = aes(fill = as.factor(Yield_class), 
                          group = as.factor(Yield_class)), 
            color = "white", alpha = 1, show.legend = T) +
    geom_sf(data = boundary_shp2, fill = NA, alpha = 0.3, colour = "black") +
    scale_fill_manual(name = "Yield", values = col_class) +
    coord_sf(xlim = c(101.2, 102), ylim = c(2.7, 3.4)) +
    theme_void() +
    theme(plot.title = element_text(family = "sans", 
                                    size = 15, hjust = 0.45, vjust = 2),
          plot.margin = margin(r = 10, l = 10),
          legend.position = "none") +
    annotate("text", x = 111, y = 6.5, size = 4,
             label = "Yield (MCM)") +
    labs(title = sel_yr) 
  
  
  maplist[[m]] <- annual_yield_map

  
}



#########################
# FACET MAP

library(gridExtra)


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}



mylegend_cl <- g_legend(map_choro)



# arrange layout

#facet_map_cl <- grid.arrange(grobs = maplist[1:6], ncol = 3)
facet_map_cl <- grid.arrange(grobs = maplist, ncol = 8)


#### title font format
title2 = grid::textGrob(paste0(reg_name_full, " Annual Yield\n"),  
                        gp = grid::gpar(fontsize = 14))


# layout - all annual

facet_legend_map_cl <- grid.arrange(facet_map_cl, mylegend_cl, 
                                    top = title2, 
                                    nrow = 2, heights = c(9, 1)
                                    #ncol = 2, widths = c(9, 1)
)

#### print last plot to file
## widescreen
ggsave(paste0(reg_name_short, "_yield2_ws.jpg"), facet_legend_map_cl, dpi = 400,
       width = 20, height = 11.25, units = "in")

## A3
ggsave(paste0(reg_name_short, "_yield2_a3.jpg"), facet_legend_map_cl, dpi = 400,
       width = 20, height = 14.19, units = "in")








