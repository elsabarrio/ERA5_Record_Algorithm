# Clear workspace
rm(list = ls())

# Read M2 model
M2.frm <- readRDS("Results/final_models/m2.rds")

# Read geopotential data
G300 <- readRDS("Data/geo_data/g_300_12pm_60_23.rds")
G500 <- readRDS("Data/geo_data/g_500_12pm_60_23.rds")
G700 <- readRDS("Data/geo_data/g_700_12pm_60_23.rds")

# Read date
DATE <- readRDS("Data/geo_data/Date.rds")

# Read latitude and longitude
LAT <- readRDS("Data/geo_data/lat.rds")
LON <- readRDS("Data/geo_data/lon.rds")

# Create a data frame joining the variable, latitude, longitude, and date data
new.df.aux <- data.frame(g300. = G300,
                     g500. = G500,
                     g700. = G700,
                     g300..lag1 = dplyr::lag(G300, 1, 0),
                     g500..lag1 = dplyr::lag(G500, 1, 0),
                     g700..lag1 = dplyr::lag(G700, 1, 0),
                     LAT = LAT,
                     LON = LON)
new.df.aux$DATE <- as.Date(DATE, format = "%m/%d/%Y")

# Remove the 29ths of February
# new.df <- subset(new.df, !(format(new.df$DATE, "%m-%d") == "02-29"))


# Input day
#days <- c('2001-08-10','2002-08-10','2003-08-10', '2004-08-10','2005-08-10','2006-08-10',
          #'2007-08-10','2008-08-10','2009-08-10', '2010-08-10','2011-08-10','2012-08-10',
          #'2013-08-10','2014-08-10','2015-08-10', '2016-08-10','2017-08-10', '2018-08-10',
          #'2019-08-10','2020-08-10','2021-08-10', '2022-08-10','2023-08-10')

days <- c('2001-08-10','2002-08-10','2003-08-10', '2004-08-10','2005-08-10','2006-08-10',
          '2007-08-10','2008-08-10','2009-08-10', '2010-08-10','2011-08-10','2012-08-10',
          '2013-08-10','2014-08-10','2015-08-10', '2016-08-10','2017-08-10', '2018-08-10',
          '2019-08-10','2020-08-10','2021-08-10', '2022-08-10','2023-08-10', 
          '2003-08-01','2003-08-02','2003-08-03','2003-08-04','2003-08-05',
          '2023-08-22','2023-08-23','2023-08-24','2023-08-25','2023-08-26',
          '2023-08-08','2023-08-09','2023-08-10','2023-08-11','2023-08-12')

days <- c('2021-08-10','2021-08-11','2021-08-12','2021-08-13',
          '2021-08-14','2021-08-15','2021-08-16','2021-08-17')

for (day in days){
  
  # Select the data only for the data in the day selected
  #new.df <- subset(new.df, format(DATE, "%Y-%m-%d") == day)
  day.target <- as.Date(day)
  new.df <- new.df.aux[new.df.aux$DATE >= (day.target - 3) & new.df.aux$DATE <= day.target, ]
  
  # Get outter grid corners
  large.grid <- data.frame(latitude = c(max(new.df$LAT),min(new.df$LAT),max(new.df$LAT),min(new.df$LAT)),
                           longitude = c(max(new.df$LON),max(new.df$LON),min(new.df$LON),min(new.df$LON)))
  
  # Loop to get variables in corners
  for (i in 1:nrow(large.grid)) {
    print(i)
    lat <- large.grid$latitude[i]
    lon <- large.grid$longitude[i]
    if (lat > 0.5){
      lat_name<- paste0(lat,"N")
    }else{lat_name<- paste0(lat,"S")}
    if (lon > 0.5){
      lon_name<- paste0(lon,"E")
    }else{lon_name<- paste0(abs(lon),"W")}
    # Call get_atmospheric_data for each combination of altitude, latitude, and longitude
    new.df[paste0("g300.", lat_name,".",lon_name)] <- new.df$g300.[which(new.df$LAT == large.grid$latitude[i] & new.df$LON == large.grid$longitude[i])]
    new.df[paste0("g500.", lat_name,".",lon_name)] <- new.df$g500.[which(new.df$LAT == large.grid$latitude[i] & new.df$LON == large.grid$longitude[i])]
    new.df[paste0("g700.", lat_name,".",lon_name)] <- new.df$g700.[which(new.df$LAT == large.grid$latitude[i] & new.df$LON == large.grid$longitude[i])]
    
    # And add lag terms
    new.df[paste0("g300.", lat_name,".",lon_name, '.lag1')] <- new.df$g300..lag1[which(new.df$LAT == large.grid$latitude[i] & new.df$LON == large.grid$longitude[i])]
    new.df[paste0("g500.", lat_name,".",lon_name, '.lag1')] <- new.df$g500..lag1[which(new.df$LAT == large.grid$latitude[i] & new.df$LON == large.grid$longitude[i])]
    new.df[paste0("g700.", lat_name,".",lon_name, '.lag1')] <- new.df$g700..lag1[which(new.df$LAT == large.grid$latitude[i] & new.df$LON == large.grid$longitude[i])]
    
  }
  
  ###################################################################################################
  # Now we need to apply the M2 model
  
  # Set data directory
  data_dir <- "C:/Users/elsac/Documents/ERA5_Record_Algorithm/Data"
  if(!dir.exists(data_dir)) stop("data_dir not found")
  
  # Load data.frame
  global.df <- readRDS(file.path(data_dir,"global_data","global_df.rds"))
  
  # Load feature selected variables based on z-values
  outdir <- "C:/Users/elsac/Documents/ERA5_Record_Algorithm/Results/final_models"
  if(!dir.exists(outdir)) stop("outdir not found")
  fin.var <- readRDS(file.path(outdir,"final_variables.rds"))
  
  # Detect lag1 variables
  lag1idx <- grep(".lag1", fin.var)
  if(length(lag1idx)>0){
    
    # Load libraries
    if(!is.element("dplyr", row.names(installed.packages()))) install.packages("dplyr")
    library(dplyr)
    
    # Calculate lag1 variables
    for(ii in 1:length(lag1idx)){
      # Find variable
      varname <- unlist(strsplit(x = fin.var[lag1idx[ii]], split = ".lag1"))
      # Create lag1
      if(!is.element(varname, names(global.df))) stop("Lag1 variable not found in global data.frame")
      global.df[[fin.var[lag1idx[ii]]]] <- dplyr::lag(global.df[[varname]], 1, 0)
      
    }# for ii lag1 found
  }# if any lag1 found
  
  # Detect poly-2 variables
  poly2idx <- grep("poly", fin.var)
  fin.single <- fin.var
  if(length(poly2idx)>0){
    for(ii in 1:length(poly2idx)){
      fin.single[poly2idx[ii]] <- gsub(", 2[)]","",gsub("poly[(]","",fin.single[poly2idx[ii]]))
    } 
  }
  
  # Check if all final local variables are included in the global data.frame
  if(!all(fin.single %in% names(global.df))) stop("Variable name not found in global data.frame")
  
  model_file <- file.path(outdir, "m2.rds")
  
  # Set training set
  idx.train <- which(global.df$t <= 51)
  
  M2 <- glm(formula = M2.frm,
            data = global.df[idx.train,],
            family = binomial(link = "logit"))
  
  ####################################################################################
  # Predict values
  predict.aux <- predict(M2, newdata = new.df, type = 'response')
  
  # Create new data frame with predict values and lat and lon
  plot.df <- data.frame(pred = predict.aux[which(new.df$DATE == day.target)],
                        LAT = new.df$LAT[which(new.df$DATE == day.target)],
                        LON = new.df$LON[which(new.df$DATE == day.target)])
  
  ###################################################################################
  # Let's create maps
  
  # Load libraries
  library(ggplot2)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(sf)
  library(ggspatial)
  library(akima)
  library(raster)
  library(dplyr)
  library(gstat) 
  
  # Convert to sf points
  stations.sf <- st_as_sf(plot.df, coords = c("LON", "LAT"), crs = 4326)
  
  # Download Spain polygon
  world <- ne_countries(scale = "medium", returnclass = "sf")
  region <- subset(world, admin %in% c("Spain", "Portugal", "France", "Morocco", "Algeria", "Andorra"))
  spain <- subset(world, admin == "Spain")
  
  # Separate multipolygon into individual polygons
  spain_parts <- st_cast(spain, "POLYGON")
  
  # Keep only mainland (largest polygon)
  spain_mainland <- spain_parts %>% 
    mutate(area = st_area(.)) %>% 
    slice_max(area, n = 1) %>% 
    st_as_sf()
  
  # Create a regular grid over Spain
  spain_bbox <- st_bbox(spain_mainland)
  grid_res <- 0.05  # grid resolution in degrees (~5 km)
  x_seq <- seq(spain_bbox["xmin"], spain_bbox["xmax"], by = grid_res)
  y_seq <- seq(spain_bbox["ymin"], spain_bbox["ymax"], by = grid_res)
  grid <- expand.grid(x = x_seq, y = y_seq)
  grid.sf <- st_as_sf(grid, coords = c("x", "y"), crs = 4326)
  grid.sf <- st_intersection(grid.sf, spain_mainland)  # keep points only inside Spain
  
  # Kriging interpolation
  stations.sp <- as(stations.sf, "Spatial")    # convert for gstat
  grid.sp <- as(grid.sf, "Spatial")
  
  
  # Kriging interpolation
  stations.sp <- as(stations.sf, "Spatial")    # convert for gstat
  grid.sp <- as(grid.sf, "Spatial")
  
  # Fit variogram
  vgm1 <- variogram(pred ~ 1, data = stations.sp)
  fit.vgm <- fit.variogram(vgm1, model = vgm("Sph"))
  
  # Ordinary kriging
  krig_res <- krige(pred ~ 1, locations = stations.sp, newdata = grid.sp, model = fit.vgm)
  
  # Convert kriging results to data frame for ggplot
  krig.df <- as.data.frame(krig_res)
  names(krig.df)[names(krig.df) == "var1.pred"] <- "pred"
  
  # Plot with ggplot
  g.map <- ggplot() +
    geom_tile(data = krig.df, aes(x = coords.x1, y = coords.x2, fill = pred)) +
    geom_sf(data = region, fill = NA, color = "black", size = 0.6) +
    scale_fill_viridis_c(option = "inferno", name = "", limits = c(0, 0.8)) +
    coord_sf(xlim = c(-10, 5), ylim = c(35, 45), expand = FALSE) +
    labs(title = day) +
    theme_minimal() +
    xlab('') + ylab('')+
    theme(
      plot.title = element_text(size = 15, hjust = 0.5),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15),
      axis.title.x = element_text(size = 15), 
      axis.title.y = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.text.x = element_text(size = 15, angle = 45, hjust = 1,  face = "bold")
    )
  
  library(cowplot)
  
  # Extract the legend from the ggplot
  legend <- get_legend(
    g.map + theme(legend.position = "right")
  )
  
  # Remove the legend from the main plot
  g.map_nolegend <- g.map + theme(legend.position = "none")
  
  # Save the map without the legend
  ggsave(filename = file.path(paste0('C:/Users/elsac/Downloads/mapas.modelo/', day, '.png')),
         plot = g.map_nolegend, device = 'png', height = 3, width = 3, bg = 'white')
  
}


# Save the legend separately
legend.plot <- plot_grid(legend)
ggsave(filename = file.path(paste0('C:/Users/elsac/Downloads/mapas.modelo/legend.pdf')),
       plot = legend.plot, device = 'pdf', width = 1.5, height = 3
       )

