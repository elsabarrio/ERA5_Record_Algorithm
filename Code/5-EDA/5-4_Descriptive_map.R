#!/usr/bin/env Rscript

# Clear workspace
rm(list = ls())

# Set data directory
data_dir <- file.path(getwd(),"Data")
if(!dir.exists(data_dir)) stop("data_dir not found")

# Read list of observatories of interest. Based on previous work.
stations <- read.csv("Data/geo_peninsula_zones.csv")

# Do not consider (for now) more than one station from Madrid
stations$STANAME[which(is.na(stations$Zona))]
idx <- which(!is.na(stations$Zona))
stations <- stations[idx,]

# Modify Barcelona-Airport label
stations$STANAME[10] <- "FABRA OBSERVATORY                       "
stations$STANAME[25] <- "BCN/AEROPUERTO                          "
#stations$abb <- substr(stations$STANAME,1,6)

extract_first_word <- function(name) {
  pos <- regexpr("[ /-]", name)
  if (pos[1] > 0) {
    return(substr(name, 1, pos[1] - 1))
  } else {
    return(name)
  }
}

# Adapt station names 

stations$abb <- sapply(stations$STANAME, extract_first_word)
stations$abb[which(stations$abb == "A")] <- "A CORUÑA"
stations$abb[which(stations$abb == "CIUDAD")] <- "CIUDAD REAL"
stations$abb[which(stations$abb == "SAN")] <- "SAN SEBASTIAN"
stations$abb[which(stations$abb == "BCN")] <- "BARCELONA"

stations$abb <- paste0(toupper(substring(stations$abb, 1, 1)), tolower(substring(stations$abb, 2)))

stations$abb[which(stations$abb == "Ciudad real")] <- "Ciudad Real"
stations$abb[which(stations$abb == "Logrono")] <- "Logroño"
stations$abb[which(stations$abb == "A coruña")] <- "A Coruña"
stations$abb[which(stations$abb == "San sebastian")] <- "San Sebastian"
##################################################################################

# Load libraries
library(raster)
library(terra)
library(geodata)

# Define the bounding box coordinates
min_lon <- -11
min_lat <- 34
max_lon <- 6
max_lat <- 46

# Download global elevation data using geodata package
global_elevation <- geodata::elevation_global(res = 0.5, path = "Data/Raster/")

# Subset the data to your defined bounding box
srtm_data <- crop(global_elevation, extent(min_lon, max_lon, min_lat, max_lat))

# Convert raster data to a data frame
srtm_raster <- raster(srtm_data)
srtm_df <- rasterToPoints(srtm_raster)
srtm_df <- as.data.frame(srtm_df)
colnames(srtm_df) <- c("lon", "lat", "elevation")

# Check the structure of the cropped raster data
print(srtm_data)

# Remove the directory immediately after use
if (dir.exists("Data/Raster/elevation")) {
  unlink("Data/Raster/elevation", recursive = TRUE)
}

# Plot the elevation data
plot(srtm_data, col = terrain.colors(255), main = "Relief Map")

################################################################################
# Create final map
library(ggplot2)
library(ggrepel)

p <- ggplot() +
  geom_raster(data = srtm_df, aes(x = lon, y = lat, fill = elevation)) +
  scale_fill_gradientn(colours = terrain.colors(255)) +
  scale_x_continuous(breaks = seq(-10, 5, by = 1), minor_breaks = seq(-10, 5, by = 1) ) +        
  scale_y_continuous(breaks = seq(35, 45, by = 1), minor_breaks = seq(35, 45, by = 1)) +        
  labs(title = '', x = "Longitude", y = "Latitude", fill = 'Elev (m)') + # Title: "ECA&D Stations"
  coord_quickmap(xlim = c(-10, 5), ylim = c(35, 45)) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "lightblue1", colour = "lightblue1"),
    panel.grid.major = element_line(color = "lightblue3"),
    panel.grid.minor = element_line(color = "lightblue3"),
    plot.title = element_text(size = 20),         # Increase title size
    axis.title = element_text(size = 25),         # Increase axis labels size
    axis.text = element_text(size = 25),          # Increase axis values size
    legend.text = element_text(size = 20),        # Increase legend text size
    legend.title = element_text(size = 25)        # Increase legend title size
  ) +
  geom_point(data = stations, aes(x = LON, y = LAT), color = "coral4", size = 2) +
  geom_text_repel(data = stations, aes(x = LON, y = LAT, label = abb), color = "coral4", size = 6.5)

print(p)

outdir <- "Results/Exploratory/"

ggsave(filename = "ecad.stations.map.pdf", plot = p, device = "pdf", path = outdir, 
       width = 10, height = 8, bg = "white")

