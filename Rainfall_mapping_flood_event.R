# VISUALIZATION

# load packages
library(tidyverse)
library(lubridate)
library(plotly)

# set strings as factors to false
options(stringsAsFactors = FALSE)

#set working directory
setwd('D:/gyc/2026/20251208_Forensik_banjir/Analysis/Spatial/Perlis')


# INPUT
area_name <- "Perlis"
a_name <- "Pls"
cnt_yr <- 347
data_source <- "JPS RHN"
source_abb <- "Aq"
font_family <- "Roboto"



#import data (daily rainfall)
RF_data = read.csv("D:/gyc/2026/20251208_Forensik_banjir/Analysis/DDF/Perlis_nov/Aq_RF_1d_Perlis_202511_full.csv", 
                   header = T, sep = ",")

#set format
str(RF_data)
#RF_data$Datetime <- as.POSIXct(RF_data$Datetime, format = "%Y-%m-%d %H:%M")
RF_data$Date <- as.Date(RF_data$Date, format = "%Y-%m-%d")


# change column names
#colnames(RF_data) <- c("Stn_no", "Stn_code", "Stn_name", "Datetime", "Depth")


#station name list
stn_list <- data.frame(unique(RF_data$Stn_no))





###########

# CHECK OUTLIERS

## hourly CHART

gg_rainfall_1h <- RF_data %>% 
  ggplot(aes(x = Date, y = Depth)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = Stn_no), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name= "Date", date_labels = "%b %d",
                   date_breaks = "1 day",
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Hourly Rainfall (mm)"),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = font_family, color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = paste0(area_name, " Hourly Rainfall (November 2025)"))

gg_rainfall_1h

ggplotly(gg_rainfall_1h, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()

#print last plot to file
ggsave(paste0(source_abb, "_RF_1h_", a_name, "_202511_scatterplot1.jpg"), dpi = 300,
       width = 8, height = 4, units = "in")


# clean data based on chart above

# RF_data2 <- RF_data %>% 
#   #filter(Depth < 100) %>% 
#   filter(Stn_no != "MOCKUPWPKL")


########
# AGGREGATE DATA TO DAILY
# ignore incomplete data
## use all data regardless of data count per day


# RF_data_day <- RF_data2 %>% 
#   mutate(Date = date(Datetime)) %>% 
#   group_by(Stn_no, Stn_name, Date) %>% 
#   summarise(Depth_day = sum(Depth, na.rm = T), cnt = sum(!is.na(Depth)))

RF_data_day <- RF_data

## daily CHART

gg_rainfall <- RF_data_day %>% 
  ggplot(aes(x = Date, y = Depth)) +
  geom_point(aes(shape = ".", alpha = 0.5, color = Stn_no), na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_date(name= "Date", date_labels = "%b %d",
               date_breaks = "1 day",
               #date_minor_breaks = "1 day",
               minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Rainfall (mm)"),
                     breaks = seq(0, 600, by = 100),
                     minor_breaks = NULL) + #y axis format
  theme(text=element_text(family = font_family, color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text.x = element_text(angle = 90, hjust = 0.5)) +
  labs(title = "Perlis Daily Rainfall (November 2025)") +
  guides(alpha = "none", shape = "none")

gg_rainfall

ggplotly(gg_rainfall, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) %>% 
  rangeslider()


#print last plot to file
ggsave(paste0(source_abb, "_RF_1d_", a_name, "_202511_scatterplot1.jpg"), dpi = 300,
       width = 8, height = 4, units = "in")


########
# CUMULATIVE HOURLY RAINFALL

## select range for cumulative rainfall
RF_data_sel <- RF_data %>% 
  filter(Datetime >= as.POSIXct("2021-12-17 00:00:00") & 
           Datetime <= as.POSIXct("2021-12-19 23:55:00"))

## calculate cumulative rainfall
RF_data_cum <- RF_data_sel %>% 
  group_by(Stn_no) %>% 
  mutate(Depth_cum = cumsum(Depth)) 



gg_RF_cum <- RF_data_cum %>% 
  ggplot(aes(x = Datetime, y = Depth_cum, group = Stn_no)) +
  geom_line(aes(color = Stn_no), alpha = 0.5, size = 0.5, na.rm = T) +
  theme_bw(base_size = 10) +
  scale_x_datetime(name = "Day", date_labels = "%d %H:00",
                   date_breaks = "12 hour",
                   expand = c(0, 0),
                   #date_minor_breaks = "1 day",
                   minor_breaks = NULL) + #x axis format
  scale_y_continuous(name= paste("Rainfall (mm)"),
                     breaks = seq(0, 600, by = 100), 
                     minor_breaks = NULL) + #y axis format
  theme(text = element_text(family = font_family, color = "grey20"),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        strip.text = element_text(size = 6),
        strip.background = element_rect(colour = "white", fill = "white"),
        axis.text.y = element_text(size = 10),
        axis.text.x = element_text(angle = 0, hjust = 0.5, size = 10)) +
  labs(title = paste0("Cumulative Rainfall from 17-19 December 2021"),
       subtitle = paste0(area_name)) +
  guides(alpha = "none", shape = "none")

gg_RF_cum

#print last plot to file
ggsave(paste0(source_abb, "_RF_2dcum_", a_name, "_20251123_plot.jpg"), dpi = 300,
       width = 10, height = 6, units = "in")

ggplotly(gg_RF_cum, #tooltip = "text",
         width = 1000, height = 500,
         dynamicTicks = TRUE) 


### write to csv
write.table(RF_data_cum, paste0(source_abb, "_RF_2dcum_", a_name, "_20211217-19.csv"), 
            sep=",", row.names = FALSE)


################################
# MAPPING

library(sf) # processing spatial vector data
library(sp) # another vector data package necessary for continuity
library(raster) # processing spatial raster data. !!!overwrites dplyr::select!!!
#library(rgdal) # read shapefile # obsolete
library(mapproj)
library(tmap) # animation

library(gridExtra)

library(viridis)
library(RColorBrewer)
library(scales)

# And a lot of different packages to test their interpolation functions
library(gstat)  # inverse distance weighted, Kriging
library(fields) # Thin Plate Spline
library(automap)# Automatic approach to Kriging


# import coordinate data
## JPS RHN station 
RF_stn = read.csv("D:/GIS_data/DATA_SG_BASIN/JPS_RHN/RF_RHN_stn_list.csv", 
                  header = T, sep = ",")

str(RF_stn)

## remove unused columns
RF_stn2 <- RF_stn[,c(2:3, 6, 8)]

## change column name
#colnames(RF_stn2)[2] <- "Stn_name"
colnames(RF_stn2) <- c("Stn_no", "Stn_name", "Lat", "Long")

str(RF_stn2)


# flood areas from JPS Perlis
banjir = read.csv("D:/gyc/2026/20251208_Forensik_banjir/Site visit/Perlis_20260115/Hot Spot Banjir Perlis 2025 - JPS Perlis.csv", 
                header = T, sep = ",")
str(banjir)

## change column name
#colnames(banjir) <- c("Date", "State", "Daerah", "Flood_area", "Lat", "Long")
## set date format
#banjir$Date <- as.Date(banjir$Date, format = "%d/%m/%Y")



# map country and coordinate data

## shapefile
pm_shp <- st_read("D:/GIS_data/JUPEM_topo/PM/Demarcation/DA0040_State_Cover_A_dis.shp")

## select state
sel_shp <- subset(pm_shp, NAM %in% c("PERLIS"))

crs(sel_shp)

## basin

all_basin_shp <- st_read("D:/GIS_data/DATA_SG_BASIN/Jica_subbasin/jsb_sm5.shp")

basin_shp <- subset(all_basin_shp, BASINNAME %in% c("Perlis"))

crs(basin_shp)


### change projection to WGS84 (original Kertau/GDM)
#pm_shp2 <-st_transform(pm_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
sel_shp2 <-st_transform(sel_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))
basin_shp2 <-st_transform(basin_shp, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))


###################
# FIND INTERSECTING BASIN 
# not used

## make sure same crs
st_crs(pm_shp2) == st_crs(basin_shp2)

## apply intersection
basin_sel_shp <- st_intersection(st_as_sf(pm_shp2), st_as_sf(basin_shp2))

## calculate area
basin_sel_shp$area <- st_area(basin_sel_shp)

## filter polygon fragments
### by area
#basin_sel_shp2 <- basin_sel_shp %>% 
#  filter(area > units::set_units(150000000, m^2))
### by state
basin_sel_shp2 <- basin_sel_shp %>% 
  filter(STATE %in% c("Selangor", "Kuala Lumpur"))

## convert to SpatialPolygonsDataFrame 
basin_sel_shp2 <- as_Spatial(basin_sel_shp2)

## dissolve polygons back to basins (merge to basins)
basin_sel_shp2 <- aggregate(basin_sel_shp2, by = "BASINNAME")


###################
# LAYOUT MAP

map <- ggplot() + 
  #geom_sf(data = pm_shp2, fill = "grey", alpha = 0.3, colour = "white") +
  geom_sf(data = sel_shp2, fill = "orange", alpha = 0.3, colour = "white") +
  geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "steelblue3") +
  #geom_sf(data = boundary_shp2, fill = NA, alpha = 0.3, colour = "red") +
  geom_point(data = RF_stn2, aes(x = Long, y = Lat),
             color = 'black', size = 1, alpha = 0.5) +
  geom_point(data = banjir, aes(x = Long, y = Lat),
             color = 'red', size = 1, alpha = 0.5) +
  coord_sf(xlim=c(100.12, 100.41), ylim=c(6.27, 6.71)) +
  theme_void()

map


###################
# INTERPOLATE 
# one day or days of interest first

#RF_data_day <- RF_data

# subset 
# RF_data_day_18 <- RF_data_cum %>% 
#   filter(Datetime >= "2021-12-18 23:00:00" & Datetime <= "2021-12-19 06:00:00") %>% 
#   group_by(Stn_name) %>% 
#   filter(Depth_cum == max(Depth_cum))

# sum 2 days rainfall
RF_data_day_18 <- RF_data_day %>% 
  filter(Date >= "2025-11-23" & Date <= "2025-11-24") %>% 
  group_by(Stn_no) %>% 
  mutate(Depth_sum = sum(Depth)) %>% 
  filter(Date == "2025-11-23")


# remove column
str(RF_data_day_18)
RF_data_day_18$Depth <- NULL
colnames(RF_data_day_18)[3] <- "Depth"

# join data
RF_day18 <- RF_data_day_18 %>% 
  merge(RF_stn2, by = "Stn_no")

# convert df to spatial
sf_day18 <- st_as_sf(RF_day18, coords = c('Long', 'Lat'), crs = 4326)
plot(sf_day18)


# create raster template
ras_interp_template <- raster(sel_shp2, res = 0.001)

# make sure same projection
crs(ras_interp_template)
crs(sf_day18)

#plot(ras_interp_template)





## Nearest Neighbour
fit_NN <- gstat::gstat( # using package {gstat} 
  formula = Depth_day ~ 1,    # The column  we are interested in
  data = as(sf_day18, "Spatial"), # using {sf} and converting to {sp}, which is expected
  nmax = 10, nmin = 3 # Number of neighboring observations used for the fit
)
d18_NN <- interpolate(ras_interp_template, fit_NN)
plot(d18_NN)
d18_NN_mask <- mask(d18_NN, mask = sel_shp2)
plot(d18_NN_mask)


# Inverse Distance Weighting
fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
  formula = Depth ~ 1,
  locations = sf_day18,
  nmax = 10, nmin = 3,
  set = list(idp = 2) # inverse distance power, adjust as needed (default: 0.5, rainfall usually 2)
)
d18_IDW <- interpolate(ras_interp_template, fit_IDW)
plot(d18_IDW)
d18_IDW_mask <- mask(d18_IDW, mask = sel_shp2)
plot(d18_IDW_mask)

class(d18_IDW_mask)

## overlay map

maprf1218 <- ggplot() +
  geom_raster(data = as.data.frame(d18_IDW_mask, xy=TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  scale_fill_gradientn(name="2-day rainfall (mm)", 
                       #colors = c("beige", "deepskyblue3", "royalblue3", "purple4"),
                       colors = c("#ffffcf", "#6fc4ad", "#1d7eb3", "#1a4998"),
                       values = rescale(c(0, 30, 60, 100)),
                       na.value = "purple",
                       limits = c(0, 500)) +
  #scale_fill_distiller(name="Rainfall (mm)", 
  #                     palette = "YlGnBu", direction = 1,
                       #na.value = "purple",
  #                     oob = scales::squish, # squish out of bound values to nearest extreme
  #                     breaks = seq(0, 60, by = 10),
  #                     limits = c(0, 60)) + # set fixed legend) +
  geom_sf(data = sel_shp2, fill = NA, colour = "white") +
  theme_void() + 
  labs(title="Cumulative rainfall from 23-24 November 2025 Rainfall Distribution") +
  coord_sf(xlim=c(100.12, 100.41), ylim=c(6.27, 6.71)) 

maprf1218

#print last plot to file
ggsave(paste0("Map_", source_abb, "_RF_2d_", a_name, "_20251123-24.jpg"), dpi = 300,
       width = 6, height = 5, units = "in")



# Thin Plate Spline Regression
fit_TPS <- fields::Tps( # using {fields}
  x = as.matrix(RF_day18[, c('Longitude', 'Latitude')]), # accepts points but expects them as matrix
  Y = RF_day18$Depth_day,  # the dependent variable
  miles = FALSE     # EPSG 25833 is based in meters
)
d18_TPS <- interpolate(ras_interp_template, fit_TPS)
plot(d18_TPS)
d18_TPS_mask <- mask(d18_TPS, mask = sel_shp2)
plot(d18_TPS_mask)



# Automatized Kriging  

## reproject to GDM 2000 PM
sf_day18_gdm <- st_transform(sf_day18, crs = 3375)
crs(sf_day18_gdm)

fit_KRIG <- automap::autoKrige(      # using {automap}
  formula = Depth_day ~ 1,                 # The interface is similar to {gstat} but
  input_data = as(sf_day18_gdm, "Spatial") # {automap} makes a lot of assumptions for you
) %>% 
  .$krige_output %>%  # the function returns a complex object with lot's of metainfo
  as.data.frame() %>% # we keep only the data we are interested in
  dplyr::select(X = x1, Y = x2, Z = var1.pred) 
d18_KRIG <- raster::rasterFromXYZ(fit_KRIG, crs = 4326) #no changes to CRS??
plot(d18_KRIG)


#############################
# CLASSIFIED MAP


RF_data_cum <- RF_day18

#check max raster value
d18_IDW_mask@data@max
#max depth
max(RF_data_cum$Depth)


## set palette 
col_pal <- brewer.pal(n = 9, name = "YlGnBu")

## set breaks (n-1) of color palette
col_brk <- c(0, 100, 150, 200, 250, 300, 350, 400, 450, 500) #cumulative legend (2-day rainfall)
col_brk <- c(0, 25, 50, 75, 100, 150, 200, 250, 300, 350) #cumulative legend (1-day rainfall)
#col_brk <- c(0, 20, 50, 100, 150, 200, 250, 300, 400, 500) #cumulative legend
#col_brk <- c(0, 20, 40, 60, 80, 100, 150, 200, 300, 400, 500) #daily legend

###replace color (first color with white)
#col_pal <- replace(col_pal, col_pal == "#FFFFD9", "#FFFFFF")


#filter flooded area
banjir_pt <- banjir
# banjir_pt <- banjir %>% 
#   filter(Date == "2021-12-18" | Date == "2021-12-17" )


  # geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "steelblue3") +
  #   geom_sf(data = sel_shp2, fill = "orange", alpha = 0.3, colour = "white") +
  # #geom_sf(data = boundary_shp2, fill = NA, alpha = 0.3, colour = "red") +
  # geom_point(data = RF_stn2, aes(x = Long, y = Lat),
  #            color = 'black', size = 1, alpha = 0.5) +
  # geom_point(data = banjir, aes(x = Long, y = Lat),
  #            color = 'red', size = 1, alpha = 0.5) +
  # coord_sf(xlim=c(100.12, 100.41), ylim=c(6.27, 6.71)) +

#plot

map_cl_day <- ggplot() +
  geom_raster(data = as.data.frame(d18_IDW_mask, xy = TRUE, na.rm = TRUE), 
              aes(x = x, y = y, fill = var1.pred)) +
  scale_fill_stepsn(name = "Cumulative rainfall (mm)",
                    #n.breaks = 3, 
                    colours = col_pal,
                    breaks = col_brk,
                    values = rescale(col_brk),
                    limits = c(0, 501)) +
  # geom_sf(data = basin_shp2, fill = NA, alpha = 0.3, colour = "sandybrown") +
  geom_sf(data = sel_shp2, colour = "black", size = 0.1, fill = NA) +
  geom_point(data = RF_day18, aes(x = Long, y = Lat),   #points for each day with data
             color = 'black', size = 0.3, alpha = 0.3) +
  geom_point(data = banjir_pt, aes(x = Long, y = Lat),  #points for kawasan banjir
             color = 'red', size = 0.5, alpha = 0.5) +
  annotate(geom = "text", x = 100.35, y = 6.4, label = "Flood location",   #legend for flooded areas
           color = "grey30", hjust = 0) +
  annotate(geom = "point", x = 100.34, y = 6.4, #label = "Flood location",   #legend for flooded areas
           color = "red", size = 1, alpha = 0.5) +
  annotate(geom = "text", x = 100.35, y = 6.38, label = "Rainfall station",   #legend for station
           color = "grey30", hjust = 0) +
  annotate(geom = "point", x = 100.34, y = 6.38, #legend for station
           color = "black", size = 1, alpha = 0.3) +
  # geom_sf_text(data = st_as_sf(basin_shp2), aes(label = BASINNAME), size = 2, colour = "sienna3") +
  theme_void() + 
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(color = "grey30", hjust = 0.5),
        legend.title = element_text(size = 10), 
        legend.position = "bottom",
        legend.text = element_text(size = 5.5)) +
  labs(title = paste0(area_name, " rainfall from 23 - 24 November 2025")) +
  coord_sf(xlim=c(100.12, 100.45), ylim=c(6.27, 6.71)) +
  guides(fill = guide_colorbar(label.position = "bottom", title.hjust = 0.9, 
                               barwidth = unit(20, "lines"), barheight = unit(0.5, "lines")))

map_cl_day

#print last plot to file
ggsave(paste0("Map_", source_abb, "_RF_2d_", a_name, "_20251123-24_lgc1_fld_stn.jpg"), dpi = 400, scale = 1.3,
       width = 5, height = 4, units = "in")


#########################
# INTERPOLATE FOR ALL DAYS

##  Use IDW

# daily rainfall data join stn data

RF_data_day_stn <- RF_data_day %>% 
  merge(RF_stn2, by = "Stn_no")

str(RF_data_day_stn)

# convert df to spatial
sf_rf_dec <- st_as_sf(RF_data_day_stn, coords = c('Long', 'Lat'), crs = 4326)
plot(sf_rf_dec)

# extract date for iteration
un_date <- sort(unique(RF_data_day_stn$Date)) # make sure date is sorted
df_date <- as.data.frame(un_date)
str(df_date)
colnames(df_date) <- "Date"


# PRODUCE INTERPOLATION RASTER AND MAP



# SEPARATE INTERPOLATION AND MAPPING

# produce interpolation raster only (without map)

interp_list = list() #for combination

i = 1

for (i in 1:(nrow(df_date))) {
  #i = 18
  date <- df_date[i,]
  
  #filter according to date
  RF_data_daily <- RF_data_day_stn %>% 
    filter(Date == date)
  
  # convert to sf
  sf_rf_daily <- st_as_sf(RF_data_daily, coords = c('Long', 'Lat'), crs = 4326)
  
  
  # Inverse Distance Weighting
  fit_IDW <- gstat::gstat( # The setup here is quite similar to NN
    formula = Depth ~ 1,
    locations = sf_rf_daily,
    nmax = 10, nmin = 3,
    set = list(idp = 2) # inverse distance power, rainfall usually 2
  )
  daily_IDW <- interpolate(ras_interp_template, fit_IDW)
  daily_IDW_mask <- mask(daily_IDW, mask = sel_shp2)
  plot(daily_IDW_mask)
  

  
  #counter <- counter + 1
  interp_list[[i]] <- daily_IDW_mask
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


#check max raster value
##manually check each raster in list
interp_list



# produce maps from interpolation raster

maplist <- list()

j = 1

for (j in 1:length(interp_list)) {
  
  date <- df_date[j,]
  
  # show points for each day
  #sel_pt <- RF_data_day_stn %>% 
  #  filter(Date == date)

  
  # plot map
  daily_IDW_map <- ggplot() +
    geom_raster(data = as.data.frame(interp_list[[j]], xy=TRUE, na.rm = TRUE), 
                aes(x = x, y = y, fill = var1.pred)) +
    scale_fill_stepsn(name = "Rainfall (mm)",
                      #n.breaks = 3, 
                      colours = col_pal,
                      breaks = col_brk,
                      values = rescale(col_brk),
                      limits = c(0, 500)) +
    # geom_sf(data = basin_sel_shp2, 
    #              fill = NA, alpha = 0.3, colour = "sandybrown") +
    geom_sf(data = sel_shp2, 
                 colour = "black", size = 0.1, fill = NA) +
    # geom_point(data = sel_pt, aes(x = Longitude, y = Latitude),    #points for each day
    #            color = 'red', size = 0.5, alpha = 0.5) +
    # geom_point(data = subset(banjir, Date == date), aes(x = Long, y = Lat),  #points for kwsn banjir
    #            color = 'red', size = 0.5, alpha = 0.5) +
    theme_void() + 
    theme(plot.title = element_text(size = 10),
          legend.position = "none") +
    labs(title = date) +
    coord_sf() 
  
  
  #counter <- counter + 1
  maplist[[j]] <- daily_IDW_map
  #rainfall_stack[[counter]] <- daily_IDW_mask
  
}


#########################
# FACET MAPPING

library(gridExtra)


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend <- g_legend(map_cl_day)


# arrange layout


####
# multiple maps with single particular map

facet_map <- grid.arrange(grobs = maplist, ncol = 7)
#facet_map <- grid.arrange(grobs = maplist[10:49], ncol = 10)


# title font format
title = grid::textGrob('Perlis November 2025 Rainfall Distribution (DID)\n', 
                       gp = grid::gpar(fontsize = 16))


# daily maps with cumulative map
facet_legend_map <- grid.arrange(facet_map, map_cl_day, #mylegend_cl, 
                                 top = title, 
                                 #nrow = 2, heights = c(9, 1)
                                 ncol = 2, widths = c(7, 3))

# daily maps only
facet_legend_map <- grid.arrange(facet_map, mylegend, #map_cl_day,  
                                 top = title, 
                                 nrow = 3, heights = c(9, 1, 0.2))


#print last plot to file
ggsave(paste0("Map_", source_abb, "_RF_1d_", a_name, "_202511_lgc1.jpg"), facet_legend_map, dpi = 400,
       width = 17, height = 10, units = "in")

## widescreen
ggsave(paste0("Map_", source_abb, "_RF_1d_", a_name, "_202511_lgc1_ws.jpg"), facet_legend_map, dpi = 400,
       width = 20, height = 11.25, units = "in")

## A3
ggsave(paste0("Map_", source_abb, "_RF_1d_", a_name, "_202511_lgc1_a3.jpg"), facet_legend_map, dpi = 400,
       width = 20, height = 14.19, units = "in")





####
# multiple maps in a layout
facet_map2 <- grid.arrange(grobs = maplist, ncol = 8) 
title2 = grid::textGrob('Kuala Lumpur-Selangor December 2021 Rainfall Distribution', 
                        gp = grid::gpar(fontsize = 16))
subtitle = grid::textGrob('JPS Rangkaian Hidrologi Nasional (RHN)\n', 
                          gp = grid::gpar(fontsize = 12))
facet_all_map <- grid.arrange(top = title2, 
                              subtitle, 
                              facet_map2, 
                              mylegend, 
                              nrow = 3, heights = c(1, 8, 1))
#ncol = 2, widths = c(7, 3))


#print last plot to file
ggsave(paste0("Map_", source_abb, "_RF_1d_", a_name, "_202112_lgc1_fld.jpg"), facet_all_map, dpi = 400,
       width = 17, height = 10, units = "in")



####
# selected (days) maps in a layout
facet_map3 <- grid.arrange(grobs = maplist[13:14], ncol = 2) 
title3 = grid::textGrob('Kuala Lumpur-Selangor 17-18 December 2021 Rainfall Distribution', 
                        gp = grid::gpar(fontsize = 16))
facet_all_map2 <- grid.arrange(top = title3, 
                              facet_map3, 
                              mylegend, 
                              nrow = 2, heights = c(8, 1))
#ncol = 2, widths = c(7, 3))


#print last plot to file
ggsave(paste0("Map_", source_abb, "_RF_1d_", a_name, "_20211217-18_lgc1_fld.jpg"), facet_all_map2, dpi = 400,
       width = 8, height = 5, units = "in")



#########################
# ANIMATION

library(animation)

a = 1

saveGIF({
  
  for (a in 1:length(maplist)){
    
    plot(maplist[[a]])
    
  }
  
}, movie.name = 'Aq_RF_1d_KLSel_202112.gif', interval = 0.5, ani.width = 700, ani.height = 600)




