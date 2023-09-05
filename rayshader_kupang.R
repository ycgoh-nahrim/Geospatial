# From rayshader tutorial
# To install the latest version from Github:
# install.packages("devtools")
devtools::install_github("tylermorganwall/rayshader")

#load package
library(rayshader)

#load a DEM tif
localtif = raster::raster("E:/Backup_main/GIS/rayshader/Kupang_dem30m.tif")


#And convert it to a matrix:
elmat = raster_to_matrix(localtif)

#We use another one of rayshader's built-in textures:
elmat %>%
  sphere_shade(texture = "desert") %>%
  plot_map()


#sphere_shade can shift the sun direction:
elmat %>%
  sphere_shade(sunangle = 45, texture = "desert") %>%
  plot_map()


#detect_water and add_water adds a water layer to the map:
elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  plot_map()



# Add image overlay

#import png
overlay_img <- png::readPNG("E:/Backup_main/GIS/rayshader/Kupang_basin_A3p_basin_clean4_adj.png")


#plot map
elmat %>%
  sphere_shade() %>%
  add_overlay(overlay_img, alphalayer = 1) %>% 
  add_shadow(ray_shade(elmat), 0.5) %>%
  plot_map()



#And here we add an ambient occlusion shadow layer, which models 
#lighting from atmospheric scattering:

elmat %>%
  sphere_shade() %>%
  add_overlay(overlay_img, alphalayer = 1) %>% 
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_map()


#passing a texture map (either external or one produced by 
#rayshader) into the plot_3d function.

elmat %>%
  sphere_shade() %>%
  add_overlay(overlay_img, alphalayer = 1) %>% 
  add_shadow(ray_shade(elmat), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, 
          zscale = 20, #Adjust the zscale down to exaggerate elevation features
          fov = 67,   #isometric. Field-of-view angle
          theta = 200, #Rotation around z-axis
          zoom = 0.3, 
          phi = 20, #Azimuth angle
          windowsize = c(1000, 800))
render_label(elmat, x = 265, y = 390, z = 800, zscale = 50,
             text = "Kg. Iboi", textsize = 2, linewidth = 3)
render_label(elmat, x = 180, y = 130, z = 800, zscale = 50,
             text = "Pekan Kupang", textsize = 2, linewidth = 3)
render_label(elmat, x = 210, y = 290, z = 800, zscale = 50,
             text = "Kg. Hangus", textsize = 2, linewidth = 3)
render_label(elmat, x = 320, y = 330, z = 800, zscale = 50,
             text = "Kg. Tiak", textsize = 2, linewidth = 3)
render_label(elmat, x = 140, y = 640, z = 4500, zscale = 50,
             textcolor = "darkgreen", linecolor = "darkgreen",
             text = "Gunung Inas", textsize = 2, linewidth = 3)
render_label(elmat, x = 260, y = 800, z = 5000, zscale = 50,
             textcolor = "darkgreen", linecolor = "darkgreen",
             text = "Gunung Bintang", textsize = 2, linewidth = 3)
render_label(elmat, x = 182, y = 190, z = 500, zscale = 50,
             textcolor = "dodgerblue4", linecolor = "dodgerblue4",
             text = "Sg. Kupang", textsize = 2, linewidth = 3)
render_label(elmat, x = 100, y = 138, z = 500, zscale = 50,
             textcolor = "dodgerblue4", linecolor = "dodgerblue4",
             text = "Sg. Ketil", textsize = 2, linewidth = 3)
Sys.sleep(0.2)
render_snapshot("E:/Backup_main/GIS/rayshader/Kupang3D_30m_label.png", 
                clear = TRUE)


#interactive map (set interactive)
render_highquality(samples = 256, 
                   interactive = FALSE,
                   lightdirection = 45, 
                   scale_text_size = 24, clear = TRUE)