# VISUALIZATION


# load packages
library(tidyverse)


# set strings as factors to false
options(stringsAsFactors = FALSE)


#set working directory
setwd('D:/gyc/2025/20230922_Water_Profile_WP/Analysis/UMT_water_demand')


# data information

reg_name_full <- "Klang - Langat"
reg_name_short <- "klngt"

#########################
# MAPPING

library(sf) # processing spatial vector data
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
#library(rgdal) # read shapefile #will be retired in 2023, replace



# import data
wd_data <- read.csv("WD_KLPutrajaya.csv", 
                     header = T, sep = ",")


# map country and coordinate data

## shapefile
pm_shp <- st_read("D:/GIS_data/JUPEM_topo/PM/Demarcation/DA0040_State_Cover_A_dis.shp")

boundary_shp <- subset(pm_shp, NAM %in% c("WILAYAH PERSEKUTUAN KUALA LUMPUR", "PUTRAJAYA"))




## basin
#all_basin_shp <- st_read("D:/GIS_data/DATA_SG_BASIN/Jica_subbasin/jsb_sm5.shp")

#basin_shp <- subset(all_basin_shp, BASINNAME %in% c("Klang", "Langat"))


# check projection

crs(boundary_shp)

#crs(basin_shp)



### change projection to WGS84 (original Kertau)

#basin_shp2 <-st_transform(basin_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

boundary_shp2 <-st_transform(boundary_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))


#crs(basin_shp2)
crs(boundary_shp2)




###############
# LAYOUT MAP

map <- ggplot() + 
  #geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "steelblue3") +
  geom_sf(data = boundary_shp2, fill = NA, alpha = 0.3, colour = "red") +
  #geom_point(data = RF_stn, aes(x = Long, y = Lat),
  #           color = 'black', size = 1, alpha = 0.5) +
  coord_sf(xlim = c(101.62, 101.76), ylim = c(2.88, 3.24)) +
  theme_void()


map


#########################
# POLYGON MAP



### set format
str(wd_data)

## rename columns
#colnames(wd_data) <- c("Basin", "Year", "Yield")



# divide value into class
wd_data2 <- wd_data %>% 
  mutate(WD_class = cut(Value, breaks = c(1, 100, 500, 1000, 1500, 2000, 2100),
                           labels = c("1-100", "101-500", "501-1000", "1001-1500", "1501-2000", "2001-2100")))


### join data to shp
wd_data_shp <- boundary_shp2 %>% 
  full_join(wd_data2, by = c("Name" = "Location")) 
  




### set colors by classification

col_class <- c(
  "1-100" = "#4083ee", 
  "101-500" = "#6aaaf4", 
  "501-1000" = "#82c2ff", 
  "1001-1500" = "#BEF7FF", 
  "1501-2000" = "#ffb860", 
  "2001-2100" = "#ee0000")

col_class <- c(
  "1-100" = "#BEF7FF", 
  "101-500" = "#82c2ff", 
  "501-1000" = "#6aaaf4", 
  "1001-1500" = "#4083ee", 
  "1501-2000" = "#ffb860", 
  "2001-2100" = "#ee0000")



## mapping choropleth
#yield_data_shp_sel <- subset(yield_data_shp, Year %in% 2012)

map_choro <- ggplot() + 
  geom_sf(data = wd_data_shp, 
          mapping = aes(fill = as.factor(WD_class), 
                        group = as.factor(WD_class)), 
          color = "white", alpha = 1, show.legend = T) +
  #geom_sf(data = boundary_shp2, fill = NA, alpha = 0.3, colour = "red") +
  scale_fill_manual(name = "Water Demand (MLD)", values = col_class) +
  geom_sf_text(data = wd_data_shp, 							# label choropleth
               nudge_x = -0.1, nudge_y = -0.1, size = 5,
               aes(label = round(Value, digits = 0))) +
  coord_sf(xlim = c(101.60, 101.78), ylim = c(2.88, 3.24)) +
  theme_void() +
  theme(plot.title = element_text(family = "sans", 
                                  size = 15, hjust = 0.45, vjust = 2),
        legend.title = element_text(size = 18, face = "bold"), 
        text = element_text(size = 16),  #legend label
        #plot.caption = element_text(size = 15, color = "grey50", hjust = 0),
        plot.margin = margin(r = 10, l = 10),
        panel.background = element_rect(fill = "gray90",linetype = "solid",
                                        color = NA, linewidth = 0),
        legend.position = "bottom") +
  annotate("text", x = 111, y = 6.5, size = 4,
           label = "Water Demand (MLD)") +
  #labs(title = 2012) +
  #labs(caption = "Data source: DOSM") +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


map_choro

### print last plot to file
ggsave(paste0(reg_name_short, "_wd_single.jpg"), dpi = 300,
       width = 5, height = 5, units = "in")




#########################
# MULTIPLE MAPS 


# extract date for iteration
un_date <- sort(unique(wd_data2$Year)) # make sure date is sorted
df_date <- as.data.frame(un_date)
str(df_date)
colnames(df_date) <- "Year"

# year and param??


# list of maps
maplist <- list()

m = 1

for (m in 1:nrow(df_date)) {
  
  # year
  sel_yr <- df_date[m, 1] # start from year 
  wd_data_shp_sel <- subset(wd_data_shp, Year %in% sel_yr)
  
  #param
  #?
  
  # plot map
  wd_map <- ggplot() + 
    geom_sf(data = wd_data_shp_sel, 
            mapping = aes(fill = as.factor(WD_class), 
                          group = as.factor(WD_class)), 
            color = "white", alpha = 1, show.legend = T) +
    #geom_sf(data = boundary_shp2, fill = NA, alpha = 0.3, colour = "black") +
    scale_fill_manual(name = "Water Demand (MLD)", values = col_class) +
    #geom_sf_text(data = wd_data_shp_sel,  							# label choropleth
    #             nudge_x = -0.1, nudge_y = -0.15, size = 2.5,
    #             aes(label = round(Yield, digits = 0))) +
    coord_sf(xlim = c(101.60, 101.78), ylim = c(2.88, 3.24)) +
    theme_void() +
    theme(plot.title = element_text(family = "sans", 
                                    size = 15, hjust = 0.45, vjust = 2),
          plot.margin = margin(r = 10, l = 10),
          legend.position = "none") +
    #annotate("text", x = 111, y = 6.5, size = 4,
    #         label = paste0(Param, " (", Year, ")")) +
    labs(title = paste0(wd_data_shp_sel$Param, " (", sel_yr, ")")) 
  
  
  maplist[[m]] <- wd_map

  
}


#########################
# FACET MAP


wd_facet_map <- ggplot() + 
  geom_sf(data = wd_data_shp, 
          mapping = aes(fill = as.factor(WD_class), 
                        group = as.factor(WD_class)), 
          color = "white", alpha = 1, show.legend = T) +
  #geom_sf(data = boundary_shp2, fill = NA, alpha = 0.3, colour = "black") +
  scale_fill_manual(name = "Water Demand (MLD)", values = col_class) +
  #geom_sf_text(data = wd_data_shp_sel,  							# label choropleth
  #             nudge_x = -0.1, nudge_y = -0.15, size = 2.5,
  #             aes(label = round(Yield, digits = 0))) +
  facet_grid(Year ~ factor(Param, levels=c('Domestic Water Demand', 'Industrial Water Demand', 'Revenue Water ',
                                     'Non Revenue Water', 'Total Water Demand')),  # sort labels
             switch = "y", # switch default y position from right
             labeller = label_wrap_gen(width = 2, multi_line = TRUE)) +  # wrap label text
  coord_sf(xlim = c(101.60, 101.78), ylim = c(2.88, 3.24)) +
  theme_void() +
  theme(#plot.title = element_text(family = "sans", 
        #                          size = 15, hjust = 0.45, vjust = 2),
        #plot.margin = margin(r = 10, l = 10),
        strip.text.x = element_text(color = "gray30", hjust = 0.5, size = 14, 
                                    margin = margin (10, 30, 10, 30)),
        strip.text.y = element_text(color = "gray30", hjust = 0, size = 14, 
                                    margin = margin (0, 15, 0, 0)),
        strip.background.x = element_rect(fill = NA, linetype = "solid", 
                                          color = NA, linewidth = 0),
        strip.background.y = element_rect(fill = NA, linetype = "solid",
                                          color = NA, linewidth = 0),
        #panel.border = element_rect(fill = NA, # Needed to add the border
        #                            color = "gray90", linewidth = 0.5),
        panel.background = element_rect(fill = "gray90",linetype = "solid",
                                        color = NA, linewidth = 0),
        panel.spacing.x = unit(15, 'points'),
        panel.spacing.y = unit(10, 'points'),
        legend.position = "none") 

wd_facet_map


#########################
# SET LAYOUT

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
#facet_map_cl <- grid.arrange(grobs = maplist, ncol = 8)


#### title font format
title2 = grid::textGrob(paste0(reg_name_full, " Water Demand\n"),  
                        gp = grid::gpar(fontsize = 14))


# layout - all annual

facet_legend_map_cl <- grid.arrange(wd_facet_map, mylegend_cl, 
                                    top = title2, 
                                    nrow = 2, heights = c(9.5, 0.5)
                                    #ncol = 2, widths = c(9, 1)
)

#### print last plot to file
## widescreen
ggsave(paste0(reg_name_short, "_wd_ws-1.jpg"), facet_legend_map_cl, dpi = 400,
       width = 20, height = 11.25, units = "in")

## A3 - landscape
ggsave(paste0(reg_name_short, "_wd_a3l-1.jpg"), facet_legend_map_cl, dpi = 400,
       width = 20, height = 14.19, units = "in")

## A3 - portrait
ggsave(paste0(reg_name_short, "_wd_a3p-3.jpg"), facet_legend_map_cl, dpi = 400,
       width = 14.19, height = 20, units = "in")







