get_contour_map <- function(day, foot, save.title, outdir){
  # Read data
  g.300 <- readRDS("Data/geo_data/g_300_12pm_60_23.rds")
  g.500 <- readRDS("Data/geo_data/g_500_12pm_60_23.rds")
  g.700 <- readRDS("Data/geo_data/g_700_12pm_60_23.rds")
  lat <- readRDS("Data/geo_data/lat.rds")
  lon <- readRDS("Data/geo_data/lon.rds")
  date <- readRDS("Data/geo_data/Date.rds")
  
  # Create data frame
  df <- data.frame(g.300 = g.300,
                   g.500 = g.500,
                   g.700 = g.700,
                   lat = lat,
                   lon = lon,
                   DATE = date)
  
  # Remove the 29ths of February
  df$DATE <- as.Date(df$DATE, format = "%m/%d/%Y")
  df <- subset(df, !(format(df$DATE, "%m-%d") == "02-29"))
  
  # Create data frame that will be used for scaling
  df.scale <-  subset(df, !(format(df$DATE, "%Y-%m-%d") >= "1980-01-01" & format(df$DATE, "%Y-%m-%d") <= "2010-12-31"))
  
  avg.g300 <- aggregate(g.300 ~ lat + lon, data = df.scale, FUN = mean)
  avg.g500 <- aggregate(g.500 ~ lat + lon, data = df.scale, FUN = mean)
  avg.g700 <- aggregate(g.700 ~ lat + lon, data = df.scale, FUN = mean)
  
  sd.g300 <- aggregate(g.300 ~ lat + lon, data = df.scale, FUN = sd)
  sd.g500 <- aggregate(g.500 ~ lat + lon, data = df.scale, FUN = sd)
  sd.g700 <- aggregate(g.700 ~ lat + lon, data = df.scale, FUN = sd)
  
  # Select the data only for the data in between the days inputted
  df.aux <- subset(df, format(DATE, "%m-%d-%Y") == day)
  
  # List of data frames to merge
  dfs <- list(avg.g300, avg.g500, avg.g700, 
              sd.g300, sd.g500, sd.g700,
              df.aux)
  
  # Merge the data frames
  df.complete <- Reduce(function(x, y) merge(x, y, by = c("lat", "lon")), dfs)
  
  # Copy data frame for interpolation
  df.aux.inter <- data.frame(lat = df.complete$lat,
                             lon = df.complete$lon,
                             g.300 = (df.complete$g.300-df.complete$g.300.x)/df.complete$g.300.y,
                             g.500 = (df.complete$g.500-df.complete$g.500.x)/df.complete$g.500.y,
                             g.700 = (df.complete$g.700-df.complete$g.700.x)/df.complete$g.700.y)
  
  ###########################################################################
  library(gstat)
  library(ggplot2)
  library(sp)
  
  # Create a SpatialPointsDataFrame object
  coordinates(df.aux.inter) <- c("lon", "lat")
  
  # Define projection
  proj4string(df.aux.inter) <- CRS("+proj=longlat +datum=WGS84")
  
  # Create a grid of points for interpolation
  #grid <- expand.grid(
    #lon = seq(min(df.aux.inter$lon), max(df.aux.inter$lon), length.out = 300),
    #lat = seq(min(df.aux.inter$lat), max(df.aux.inter$lat), length.out = 300)
  #)

  grid <- expand.grid(
    lon = seq(-10, 5, length.out = 300),
    lat = seq(35, 45, length.out = 300)
  )
  
  # Convert the grid to SpatialPoints
  grid_sp <- SpatialPoints(grid, proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  # Variogram calculus
  variogram.model.300 <- variogram(g.300 ~ 1, data = df.aux.inter)
  variogram.model.500 <- variogram(g.500 ~ 1, data = df.aux.inter)
  variogram.model.700 <- variogram(g.700 ~ 1, data = df.aux.inter)
  
  # Adjust a spheric variogram model
  variogram.fit.300 <- fit.variogram(variogram.model.300, model = vgm(psill = 1, model = "Sph", range = 1000))
  variogram.fit.500 <- fit.variogram(variogram.model.500, model = vgm(psill = 1, model = "Sph", range = 1000))
  variogram.fit.700 <- fit.variogram(variogram.model.700, model = vgm(psill = 1, model = "Sph", range = 1000))
  
  # Interpolation using krige
  kriging.result.300 <- krige(g.300 ~ 1, locations = df.aux.inter, newdata = grid_sp, model = variogram.fit.300)
  kriging.result.500 <- krige(g.500 ~ 1, locations = df.aux.inter, newdata = grid_sp, model = variogram.fit.500)
  kriging.result.700 <- krige(g.700 ~ 1, locations = df.aux.inter, newdata = grid_sp, model = variogram.fit.700)
  
  # Final data frame with interpolated data
  interp.df <- data.frame(g.300 = kriging.result.300$var1.pred,
                          g.500 = kriging.result.500$var1.pred,
                          g.700 = kriging.result.700$var1.pred,
                          lat = kriging.result.300$lat,
                          lon = kriging.result.300$lon)
  
  #################################################################################
  
  # Instala y carga las librerías necesarias
  #install.packages("ggplot2")
  #install.packages("maps")
  library(ggplot2)
  library(maps)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(directlabels)
  library(dplyr)
  
  spain <- ne_countries(scale = "medium", returnclass = "sf", country = "Spain")
  portugal <- ne_countries(scale = "medium", returnclass = "sf", country = "Portugal")
  france <- ne_countries(scale = "medium", returnclass = "sf", country = "France")
  morocco <- ne_countries(scale = "medium", returnclass = "sf", country = "Morocco")
  algeria <- ne_countries(scale = "medium", returnclass = "sf", country = "Algeria")
  
  iberian_peninsula <- rbind(france, spain, portugal, morocco, algeria)
  
  # Contour map
  #g1 <- ggplot() +
  #geom_sf(data = iberian_peninsula, fill = "white", color = "black") +
  #geom_contour(data = interp.df, aes(x = lon, y = lat, z = g.300, color = "g.300"), linetype = 3, size = 0.7 ) +
  #geom_contour(data = interp.df, aes(x = lon, y = lat, z = g.500, color = "g.500"), linetype = 2, size = 0.7 ) +
  #geom_contour(data = interp.df, aes(x = lon, y = lat, z = g.700, color = "g.700"), linetype = 1, size = 0.7 ) +
  #coord_sf(xlim = c(-9.2, 4.2), ylim = c(35.5, 44.5)) +
  #labs(title = foot,
  #x = "Longitude", y = "Latitude",
  #color = "Contours") +
  #scale_color_manual(values = c("g.300" = "blue3", "g.500" = "red3", "g.700" = "green3")) +
  #theme_minimal()+
  #geom_dl(data = interp.df, aes(x = lon, y = lat, z = g.300, label = ..level..), method = "smart.grid", stat = "contour", color = "blue3", size = 0.7) +
  #geom_dl(data = interp.df, aes(x = lon, y = lat, z = g.500, label = ..level..), method = "smart.grid", stat = "contour", color = "red3", size = 0.7) +
  #geom_dl(data = interp.df, aes(x = lon, y = lat, z = g.700, label = ..level..), method = "smart.grid", stat = "contour", color = "green3", size = 0.7)
  
  # Create contour map
  g1 <- ggplot() +
    geom_sf(data = iberian_peninsula, fill = "white", color = "black") +
    geom_contour(data = interp.df, aes(x = lon, y = lat, z = g.300, color = "g.300"), linetype = 3, size = 0.7) +
    geom_contour(data = interp.df, aes(x = lon, y = lat, z = g.500, color = "g.500"), linetype = 2, size = 0.7) +
    geom_contour(data = interp.df, aes(x = lon, y = lat, z = g.700, color = "g.700"), linetype = 1, size = 0.7) +
    coord_sf(xlim = c(-10, 5), ylim = c(35, 45)) + #, label_axes = "----"
    labs(title = foot,
         x = "", y = "",
         color = "") +
    scale_color_manual(values = c("g.300" = "blue3", "g.500" = "red3", "g.700" = "green3"),
			labels = c("g.300" = "G300", 'g.500'= "G500", "g.700" = "G700")) +
    theme_minimal()+
    theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15),
      axis.title.x = element_text(size = 15), 
      axis.title.y = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.text.x = element_text(size = 15, angle = 45, hjust = 1,  face = "bold")
    )
  
  # Extract data from contours
  contour_data_300 <- ggplot_build(g1)$data[[2]]
  contour_data_500 <- ggplot_build(g1)$data[[3]]
  contour_data_700 <- ggplot_build(g1)$data[[4]]
  
  # Select just one value per contour line
  #contour_data_300 <- contour_data_300 %>% group_by(level) %>% slice(4)
  #contour_data_500 <- contour_data_500 %>% group_by(level) %>% slice(4)
  #contour_data_700 <- contour_data_700 %>% group_by(level) %>% slice(4)
  
  # Add contour labels
  g1 <- g1 +
    geom_dl(data = contour_data_300, aes(x = x, y = y, label = level), method = "smart.grid", color = "blue3", size = 5) +
    geom_dl(data = contour_data_500, aes(x = x, y = y, label = level), method = "smart.grid", color = "red3", size = 5) +
    geom_dl(data = contour_data_700, aes(x = x, y = y, label = level), method = "smart.grid", color = "green3", size =5)
  
  ggsave(save.title, g1, device = "pdf", path = outdir, width = 5, height = 4)
  
}