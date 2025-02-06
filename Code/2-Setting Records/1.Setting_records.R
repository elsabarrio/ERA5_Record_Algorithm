# Clear workspace
rm(list = ls())

# Read the data for maximum temperatures
tx.mat <- read.csv("C:/Users/elsac/Documents/Excess.records/Data/tx.data/Tx_mat.csv", header = TRUE)

# Ensure the Date column is in Date format
tx.mat$Date <- as.Date(tx.mat$Date)

# Calculate the 95th percentile for each station using the 1991-2020 data for June, July, and August
thresholds <- apply(tx.mat[format(tx.mat$Date, "%Y") >= 1991 & format(tx.mat$Date, "%Y") <= 2020 & 
                         format(tx.mat$Date, "%m") %in% c("06", "07", "08"), -1], 2, 
                    function(x) quantile(x, 0.95, na.rm = TRUE))

# Find dates of interest
dates.idx <- which(format(tx.mat$Date, "%m") %in% c("06", "07", "08"))

# Create matrix to calculate records
A <- as.matrix(tx.mat[dates.idx, -1])

# Apply the thresholds to all June, July, and August data (all years) and convert to 1s and 0s
record.matrix<- array(as.integer(A > thresholds[col(A)]), dim = dim(A))

# Adjust the colnames so that they do not contain X before the station id
colnames(record.matrix) <- gsub(pattern = "X", replacement = "", x = colnames(tx.mat[-1]))

# Add the Date column back to the result
record.matrix$Date <- tx.mat[format(tx.mat$Date, "%m") %in% c("06", "07", "08"), "Date"]

# Reorder so that Date is the first column
record.matrix <- record.matrix[, c(ncol(record.matrix), 1:(ncol(record.matrix) - 1))]

# Reshape the record matrix
LL <- nrow(record.matrix)/length(unique(format(record.matrix$Date, "%Y")))
TT <- length(unique(format(record.matrix$Date, "%Y")))
SS <- ncol(record.matrix)-1
itx3d <- array(data = as.matrix(record.matrix[,-1]), dim = c(LL,TT,SS))

# Now save this record  matrix in the Data folder
# Create folder if it does not exist
outdir <- "C:/Users/elsac/Documents/Excess.records/Data/record.data"
# Check if the folder exists
if (!dir.exists(outdir)) {
  # Create the folder if it doesn't exist
  dir.create(outdir)
}

# Check if the file already exists
if (!file.exists(file.path(outdir,"record.matrix.csv"))) {
  # Save the record matrix
  write.csv(record.matrix, file = file.path(outdir,"record.matrix.csv"), row.names = FALSE)
}

# Read the data for stations
stations <- read.csv("C:/Users/elsac/Documents/Excess.records/Data/geo_peninsula_zones.csv")

# Check what threshold corresponds to each station
staid.threshold <- gsub(pattern = "X", replacement = "", x = names(thresholds))

# Add the thresholds to the stations data frame
stations$threshold <- round(thresholds[match(stations$STAID,staid.threshold)]/10,1)

# Now we only want to keep one station from Madrid
# Do not consider (for now) more than one station from Madrid
stations$STANAME[which(is.na(stations$Zona))]
# Get indices for the stations that have NA in the column Zona
idx <- which(!is.na(stations$Zona))
# Remove stations
stations <- stations[idx,]

# We are going to abbreaviate the names of the stations to be able to show them in map
# Create function to extract first word from a string of characters
extract_first_word <- function(name) {
  pos <- regexpr("[ /-]", name)
  if (pos[1] > 0) {
    return(substr(name, 1, pos[1] - 1))
  } else {
    return(name)
  }
}

# Create new column with abbreviated names using the function we just created
stations$abb <- sapply(stations$STANAME, extract_first_word)

# Adjust some of the names manually
stations$abb[which(stations$abb == "A")] <- "A CORUÑA"
stations$abb[which(stations$abb == "CIUDAD")] <- "CIUDAD REAL"
stations$abb[which(stations$abb == "SAN")] <- "SAN SEBASTIAN"
stations$abb[which(stations$abb == "BCN")] <- "BARCELONA"

# Set the firs letter to capital letters
stations$abb <- paste0(toupper(substring(stations$abb, 1, 1)), tolower(substring(stations$abb, 2)))

# Now we create a map of the plot with the threshold values
library(ggplot2)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(gridExtra)

# Get the map of the countries that will be shown in the map
spain <- ne_countries(scale = "medium", returnclass = "sf", country = "Spain")
portugal <- ne_countries(scale = "medium", returnclass = "sf", country = "Portugal")
france <- ne_countries(scale = "medium", returnclass = "sf", country = "France")
morocco <- ne_countries(scale = "medium", returnclass = "sf", country = "Morocco")
algeria <- ne_countries(scale = "medium", returnclass = "sf", country = "Algeria")

# Now combine them
iberian.peninsula <- rbind(france, spain, portugal, morocco, algeria)

# Plot the map with the data points and values
g.threshold <- ggplot() +
  geom_sf(data = iberian.peninsula, fill = "white", color = "black") +  # Plot Spain map
  geom_point(data = stations, aes(x = LON, y = LAT), size = 1) +
  geom_text_repel(data = stations, aes(x = LON, y = LAT, label = threshold), size = 3) +  # Plot points
  scale_color_gradient(low = "blue", high = "red") +  # Adjust color gradient for 'value'
  labs(title = "Threshold values", x = "Longitude", y = "Latitude", color = "Threshold", size = "Threshold") +
  coord_sf(xlim = c(-10, 5), ylim = c(35, 45)) +
  theme_minimal()

# Create the plot with the stations labels
g.labels <- ggplot() +
  geom_sf(data = iberian.peninsula, fill = "white", color = "black") +  # Plot Spain map
  geom_point(data = stations, aes(x = LON, y = LAT), size = 1) +
  geom_text_repel(data = stations, aes(x = LON, y = LAT, label = abb), size = 3) +  # Plot points
  scale_color_gradient(low = "blue", high = "red") +  # Adjust color gradient for 'value'
  labs(title = "Spanish Stations", x = "Longitude", y = "Latitude", color = "Threshold", size = "Threshold") +
  coord_sf(xlim = c(-10, 5), ylim = c(35, 45)) +
  theme_minimal()

# Now combine both plots in the same frame
threshold.map <- grid.arrange(g.labels, g.threshold, ncol = 2)  # Arrange side by side (2 columns)

# Save plot
ggsave(filename = "records.thresholds.map.pdf", plot = threshold.map, 
       device = "pdf", path = "C:/Users/elsac/Documents/Excess.records/Results/Exploratory",
       width = 12, height = 6)
