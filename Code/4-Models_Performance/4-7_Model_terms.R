# Clear workspace
rm(list = ls())

# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)

# Set data directory
data_dir <- file.path(getwd(),"Data")
if(!dir.exists(data_dir)) stop("data_dir not found")

# Load data.frame
global.df <- readRDS(file.path(data_dir,"global_data","global_df.rds"))

# Load feature selected variables based on z-values
outdir <- file.path(getwd(),"Results/final_models")
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

# Set training set
idx.train <- which(global.df$t <= 51)

# Define data frame
#df.aux <- global.df[idx.train,]
df.aux <- global.df[idx.train,] %>%
  mutate(across(starts_with("g"), ~ .x - mean(.x, na.rm = TRUE)))

# Read list of observatories of interest. Based on previous work.
stations <- read.csv(file.path(data_dir,"geo_peninsula_zones.csv"))
idx <- which(!is.na(stations$Zona))
stations <- stations[idx,]
# Abbreviations
stations$STANAME[10] <- "FABRA OBSERVATORY                       "
stations$STANAME[25] <- "BCN/AEROPUERTO                          "
stations$abb <- substr(stations$STANAME,1,6)

################################################################################
# Function to change poly terms for quadratic
expand_poly2 <- function(f) {
  # collapse formula into a single line
  f_str <- paste(deparse(f), collapse = " ")
  
  # regex finds poly(var,2)
  pattern <- "poly\\(([A-Za-z0-9\\.]+), *2\\)"
  
  # replace all occurrences
  f_str_new <- gsub(pattern,
                    "(\\1 + I(\\1^2))",
                    f_str)
  
  as.formula(f_str_new)
}


######################################################################################
# M1 model

# M1: Simple global model
# Define file path
model_file <- file.path(outdir, "m1.rds")
# Check if file exists
if(!file.exists(model_file)) stop("Model M1 not found!")
# File exists; read the formula
m1.frm <- readRDS(model_file)
# Update formula
m1.frm.aux <- expand_poly2(m1.frm)
# Compute the model
m1 <- glm(formula = m1.frm.aux,
          data = global.df[idx.train,],
          family = binomial(link = "logit"))

##############################################################################
# Term effects

# Select variable of interest
#var.aux <- "g500.35N.10W"
#var.aux <- "g700.45N.10W"
#var.aux <- 'g700.'
var.aux <- 'g700..lag1'

capitalize_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}

# Extract coefficient for the variable
coef.aux <- coef(m1)[var.aux]

# Get specific data
df.aux <- global.df[idx.train, var.aux]

# For lag terms, remove the first value
#df.aux <- df.aux[-1]

# Compute mean of x
x.mean <- mean(df.aux, na.rm = TRUE)

# Change sequence of x values
aux.x <- seq(
  min(df.aux, na.rm = TRUE),
  max(df.aux, na.rm = TRUE),
  by = sd(df.aux, na.rm = TRUE) / 10
)

# Linar effect
#aux.y <- coef1 * aux.x + coef2 * (aux.x^2)
aux.y.effect <- coef.aux * aux.x

# Center the effect so that mean(x) = 0
aux.y <- aux.y.effect - (coef.aux * x.mean)

# Data for ggplot
df.plot <- data.frame(
  x_centered = aux.x - x.mean,
  effect = aux.y
)

# Define data frame and variable
#df.aux$pred_fixed <- df.aux[[var.aux]] * coef.aux
#df.aux$pred_fixed <- exp(global.df[idx.train,var.aux] * coef.aux)
#df.aux$pred_fixed <- global.df[idx.train,var.aux] * coef.aux
#df.aux <- df.aux[-1,] # We need to remove the first row, only for lag terms

# Create plot
library(ggplot2)

g1 <- ggplot(df.plot, aes(x = x_centered, y = effect)) +
  # ggplot(df.aux, aes(x = .data[[var.aux]], y = pred_fixed)) +
  geom_line(color = "black") +      # semi-transparent points
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_abline(intercept = 0, slope = coef.aux, color = "red", size = 1.2) +  # linear contribution
  labs(
    x = expression(G - bar(G) ~ (m^{2} * s^{-2})),
    y = "Effect",
    title = capitalize_first(paste(var.aux))
  ) +
  theme_minimal(base_size = 14)+
  theme(    
    plot.title = element_text(size = 20),         # Increase title size
    axis.title = element_text(size = 20),         # Increase axis labels size
    axis.text = element_text(size = 20),          # Increase axis values size
    legend.text = element_text(size = 20),        # Increase legend text size
    legend.title = element_text(size = 20))


# Create folder if it does not exist
if (!dir.exists(file.path(outdir,'Terms'))) {
  dir.create(file.path(outdir,'Terms'), recursive = TRUE)
}

#geom_rug(
  #data = data.frame(
    #x_centered = df.aux -
      #mean(df.aux, na.rm = TRUE)
  #),
  #aes(x = x_centered),
  #sides = "b",
  #alpha = 0.4,
  #inherit.aes = FALSE,      # important so it doesn't try to use y
  #length = unit(0.02, "npc")  # small ticks
#) +
  
ggsave(file.path(paste0(outdir,'/Terms/M1.',var.aux,'.png')), g1, 
       width = 5, height = 3, bg = 'white')
##################################################################################
# Select variable of interest
var.aux <- "g300.35N.5E"
#var.aux <- "g700.45N.5E"
#var.aux <- 'g700.35N.5E'

# Extract coefficient for the variable
# coef.aux1 <- coef(m1)[paste0('poly(',var.aux,', 2)1')]
# coef.aux2 <- coef(m1)[paste0('poly(',var.aux,', 2)2')]
coef.aux1 <- coef(m1)[var.aux]
coef.aux2 <- coef(m1)[paste0('I(',var.aux,'^2)')]

# Get specific data
df.aux <- global.df[idx.train, var.aux]

# Compute mean of x
x.mean <- mean(df.aux, na.rm = TRUE)

# Change sequence of x values
aux.x <- seq(
  min(df.aux, na.rm = TRUE),
  max(df.aux, na.rm = TRUE),
  by = sd(df.aux, na.rm = TRUE) / 10
)

# Linar effect
aux.y.effect <- coef.aux1 * aux.x + coef.aux2 * (aux.x^2)

# Center the effect so that mean(x) = 0
aux.y <- aux.y.effect - (coef.aux1 * x.mean + coef.aux2 * (x.mean^2))

# Data for ggplot
df.plot <- data.frame(
  x_centered = aux.x - x.mean,
  effect = aux.y
)

# Poly term effects
#X <- model.matrix(m1)
#p1 <- X[, paste0("poly(", var.aux, ", 2)1")]
#p2 <- X[, paste0("poly(", var.aux, ", 2)2")]


# Compute contribution
#df.aux$pred_fixed_poly <- coef.aux1 * p1 + coef.aux2 * p2
#df.aux$pred_fixed_poly <- coef.aux1 * global.df[idx.train,var.aux] + coef.aux2 * global.df[idx.train,var.aux]^2


g2 <- ggplot(df.plot, aes(x = x_centered, y = effect)) +
  # ggplot(df.aux, aes(x = .data[[var.aux]], y = pred_fixed_poly)) +
  geom_line(color = "black") +      # semi-transparent points
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_abline(intercept = 0, slope = coef.aux, color = "red", size = 1.2) +  # linear contribution
  labs(
    x = expression(G - bar(G) ~ (m^{2} * s^{-2})),
    y = "",
    title = paste(var.aux)
  ) +
  theme_minimal(base_size = 14)

ggsave(file.path(paste0(outdir,'/Terms/M1.',var.aux,'.png')), g2, 
       width = 5, height = 3, bg = 'white')


########################################################################################
# M2 model

# M2: M1 * (LAT+LON)
# Define file path
model_file <- file.path(outdir, "m2.rds")
# Check if file exists
if(!file.exists(model_file)) stop("Model M2 not found!")
# File exists; read the formula
m2.frm <- readRDS(model_file)
# Update formula
m2.frm.aux <- expand_poly2(m2.frm)
# Compute the model
m2 <- glm(formula = m2.frm.aux,
          data = global.df[idx.train,],
          family = binomial(link = "logit"))

##################################################################################
# m2 model terms that interact with lat and lon
# Linear m2 model terms that interact with lat and lon

# Select variable of interest
var.aux <- "g700.45N.10W"
#var.aux <- 'g700.'

# Extract coefficient for the variable
coef.aux      <- coef(m2)[var.aux]
coef.lat.aux  <- coef(m2)[paste0(var.aux,':LAT')]
coef.lon.aux  <- coef(m2)[paste0(var.aux,':LON')]

# Select stations of interest
stat.aux <- c('CORUNA','ZARAGOZA','MURCIA')
stations.aux <- stations[grepl(paste(stat.aux, collapse = '|'), stations$STANAME), c('STANAME','LAT','LON')]

# Get specific data
df.aux <- global.df[idx.train, var.aux]

# Compute mean of x
x.mean <- mean(df.aux, na.rm = TRUE)

# Change sequence of x values
aux.x <- seq(
  min(df.aux, na.rm = TRUE),
  max(df.aux, na.rm = TRUE),
  by = sd(df.aux, na.rm = TRUE) / 10
)

# Data for ggplot
df.plot <- data.frame(
  x_centered = aux.x - x.mean
)

# Create variables for each station
for (i in 1:nrow(stations.aux)) {
  
  st  <- stations.aux$STANAME[i]
  #LAT <- stations.aux$LAT[i]
  LON <- stations.aux$LON[i]

  # Linar effect
  aux.y.effect <- coef.aux     * aux.x +
    #coef.lat.aux * aux.x * LAT +
    coef.lon.aux * aux.x * LON
  
  # Center the effect so that mean(x) = 0
  aux.y <- aux.y.effect - (coef.aux     * x.mean +
                             #coef.lat.aux * x.mean * LAT +
                             coef.lon.aux * x.mean * LON)
  
  # Data for ggplot
  df.plot[[paste0("pred_", st)]] <- aux.y
  
  #df.plot[[paste0("pred_", st)]] <-
    #coef.aux     * global.df[idx.train,var.aux] +
    #coef.lat.aux * global.df[idx.train,var.aux] * LAT +
    #coef.lon.aux * global.df[idx.train,var.aux] * LON
}

# Change to long format to plot
df.long <- df.plot |>
  pivot_longer(
    cols = starts_with("pred_"),
    names_to = "STANAME",
    values_to = "pred"
  ) |>
  mutate(STANAME = sub("pred_", "", STANAME)) |>
  arrange(STANAME, x_centered)

# Create plot
g3 <- ggplot(df.long, aes(x = x_centered, y = pred, color = STANAME)) +
  #geom_line(size = 1) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    x = expression(G - bar(G) ~ (m^{2} * s^{-2})),
    y = "Effect",
    title = toupper(var.aux),
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian() +
  theme(
    plot.margin = margin(5, 5, 5, 5),  # small margin so legend doesn’t get pushed out
    #legend.position = c(1.2, 1.2),   # inside, relative to panel
    #legend.justification = c("right", "top"),
    #legend.text = element_text(size = 9),
    plot.title = element_text(size = 20),         # Increase title size
    axis.title = element_text(size = 20),         # Increase axis labels size
    axis.text = element_text(size = 20),          # Increase axis values size
    #legend.text = element_text(size = 20),        # Increase legend text size
    #legend.title = element_text(size = 20)
    legend.position = 'none'
  ) 

ggsave(file.path(paste0(outdir,'/Terms/M2.',var.aux,'.png')), g3, 
       width = 5, height = 3, bg = 'white')

##################################################################################
# Poly m2 model terms that interact with lat and lon

# Select variable of interest
var.aux <- "g700.45N.5E"

# Extract coefficient for the variable
#coef.aux1      <- coef(m2)[paste0('poly(',var.aux,', 2)1')]
#coef.aux2      <- coef(m2)[paste0('poly(',var.aux,', 2)2')]
#coef.lat.aux1  <- coef(m2)[paste0('LAT:poly(',var.aux,', 2)1')]
#coef.lat.aux2  <- coef(m2)[paste0('LAT:poly(',var.aux,', 2)2')]
#coef.lon.aux1  <- coef(m2)[paste0('poly(',var.aux,', 2)1:LON')]
#coef.lon.aux2  <- coef(m2)[paste0('poly(',var.aux,', 2)2:LON')]

coef.aux1      <- coef(m2)[var.aux]
coef.aux2      <- coef(m2)[paste0('I(',var.aux,'^2)')]
coef.lat.aux1  <- coef(m2)[paste0('LAT:',var.aux)]
coef.lat.aux2  <- coef(m2)[paste0('LAT:I(',var.aux,'^2)')]
coef.lon.aux1  <- coef(m2)[paste0(var.aux,':LON')]
coef.lon.aux2  <- coef(m2)[paste0('I(',var.aux,'^2):LON')]

# Poly term effects
#X  <- model.matrix(m2)
#p1 <- X[, paste0("poly(", var.aux, ", 2)1")]
#p2 <- X[, paste0("poly(", var.aux, ", 2)2")]

# Select stations of interest
stat.aux <- c('CORUNA','ZARAGOZA','MURCIA')
stations.aux <- stations[grepl(paste(stat.aux, collapse = '|'), stations$STANAME), c('STANAME','LAT','LON')]

# Get specific data
df.aux <- global.df[idx.train, var.aux]

# Compute mean of x
x.mean <- mean(df.aux, na.rm = TRUE)

# Change sequence of x values
aux.x <- seq(
  min(df.aux, na.rm = TRUE),
  max(df.aux, na.rm = TRUE),
  by = sd(df.aux, na.rm = TRUE) / 10
)

# Data for ggplot
df.plot <- data.frame(
  x_centered = aux.x - x.mean
)

# Create variables for each station
for (i in 1:nrow(stations.aux)) {
  
  st  <- stations.aux$STANAME[i]
  LAT <- stations.aux$LAT[i]
  LON <- stations.aux$LON[i]
  
  #df.plot[[paste0("pred_", st)]] <-
    #coef.aux1     * p1 +
    #coef.aux2     * p2 +
    #coef.lat.aux1 * p1 * LAT +
    #coef.lat.aux2 * p2 * LAT +
    #coef.lon.aux1 * p1 * LON +
    #coef.lon.aux2 * p2 * LON
  
  #df.plot[[paste0("pred_", st)]] <-
    #coef.aux1     * global.df[idx.train,var.aux]   +
    #coef.aux2     * global.df[idx.train,var.aux]^2 +
    #coef.lat.aux1 * global.df[idx.train,var.aux]   * LAT +
    #coef.lat.aux2 * global.df[idx.train,var.aux]^2 * LAT +
    #coef.lon.aux1 * global.df[idx.train,var.aux]   * LON +
    #coef.lon.aux2 * global.df[idx.train,var.aux]^2 * LON
  
  # Linar effect
  aux.y.effect <- 
    coef.aux1     * aux.x   +
    coef.aux2     * aux.x^2 +
    coef.lat.aux1 * aux.x   * LAT +
    coef.lat.aux2 * aux.x^2 * LAT +
    coef.lon.aux1 * aux.x   * LON +
    coef.lon.aux2 * aux.x^2 * LON
  
  # Center the effect so that mean(x) = 0
  aux.y <- aux.y.effect - (coef.aux1     * x.mean   +
                             coef.aux2     * x.mean^2 +
                             coef.lat.aux1 * x.mean   * LAT +
                             coef.lat.aux2 * x.mean^2 * LAT +
                             coef.lon.aux1 * x.mean   * LON +
                             coef.lon.aux2 * x.mean^2 * LON)
  
  # Data for ggplot
  df.plot[[paste0("pred_", st)]] <- aux.y
}

colnames(df.plot)[2] <- "pred_ZARAGOZA                                "
# Change to long format to plot
df.long <- df.plot |>
  pivot_longer(
    cols = starts_with("pred_"),
    names_to = "STANAME",
    values_to = "pred"
  ) |>
  mutate(STANAME = sub("pred_", "", STANAME)) |>
  arrange(STANAME, x_centered)

# Create plot
g4 <- ggplot(df.long, aes(x = x_centered, y = pred, color = STANAME)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  #geom_point() +
  labs(
    x = expression(G - bar(G) ~ (m^{2} * s^{-2})),
    y = "Effect",
    title = toupper(var.aux),
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian() +
  theme(
    plot.margin = margin(5, 5, 5, 5),  # small margin so legend doesn’t get pushed out
    legend.position = c(1.2, 1.2),   # inside, relative to panel
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 15),
    legend.title = element_text(size = 15),
    plot.title = element_text(size = 20),         # Increase title size
    axis.title = element_text(size = 20),         # Increase axis labels size
    axis.text = element_text(size = 20),          # Increase axis values size
    #legend.key.size = unit(0.4, "lines")
  ) 

ggsave(file.path(paste0(outdir,'/Terms/M2.',var.aux,'.png')), g4, 
       width = 5, height = 3, bg = 'white')


################################################################################
# MAPS for M2 model terms
################################################################################
# LAT and LON
var.aux <- 'Intercept'

coef.aux      <- coef(m2)['(Intercept)']
coef.lat.aux  <- coef(m2)['LAT']
coef.lon.aux  <- coef(m2)['LON']

# Get unique stations
stations.aux <- unique(df.aux[, c('STAID', 'LAT', 'LON')])
stations.aux$pred <- NA

# Loop over all stations
for (i in 1:nrow(stations.aux)) {
  st  <- stations.aux$STAID[i]
  LAT <- stations.aux$LAT[i]
  LON <- stations.aux$LON[i]
  
  # Create a new column for the predicted value for this station
  stations.aux$pred[i] <- 
    coef.aux           +
    coef.lat.aux * LAT +
    coef.lon.aux * LON
}

################################################################################
# Linear terms

# Select variable of interest
#var.aux <- "g700.45N.10W"
var.aux <- 'g700.'

# Extract coefficient for the variable
coef.aux      <- coef(m2)[var.aux]
#coef.lat.aux  <- coef(m2)[paste0(var.aux,':LAT')]
coef.lon.aux  <- coef(m2)[paste0(var.aux,':LON')]


# Get unique stations
stations.aux <- unique(df.aux[, c('STAID', 'LAT', 'LON')])
stations.aux$pred <- NA

# Calculate the mean of the variable
mean.var <- mean(global.df[idx.train,var.aux])

# Loop over all stations
for (i in 1:nrow(stations.aux)) {
  st  <- stations.aux$STAID[i]
  #LAT <- stations.aux$LAT[i]
  LON <- stations.aux$LON[i]
  
  # Create a new column for the predicted value for this station
  stations.aux$pred[i] <- 
    coef.aux           +
    #coef.lat.aux * LAT +
    coef.lon.aux * LON
}

################################################################################
# Poly terms

# Select variable of interest
# Select variable of interest
var.aux <- "g700.45N.5E"

# Extract coefficient for the variable
#coef.aux1      <- coef(m2)[paste0('poly(',var.aux,', 2)1')]
#coef.aux2      <- coef(m2)[paste0('poly(',var.aux,', 2)2')]
#coef.lat.aux1  <- coef(m2)[paste0('LAT:poly(',var.aux,', 2)1')]
#coef.lat.aux2  <- coef(m2)[paste0('LAT:poly(',var.aux,', 2)2')]
#coef.lon.aux1  <- coef(m2)[paste0('poly(',var.aux,', 2)1:LON')]
#coef.lon.aux2  <- coef(m2)[paste0('poly(',var.aux,', 2)2:LON')]

coef.aux1      <- coef(m2)[var.aux]
coef.aux2      <- coef(m2)[paste0('I(',var.aux,'^2)')]
coef.lat.aux1  <- coef(m2)[paste0('LAT:',var.aux)]
coef.lat.aux2  <- coef(m2)[paste0('LAT:I(',var.aux,'^2)')]
coef.lon.aux1  <- coef(m2)[paste0(var.aux,':LON')]
coef.lon.aux2  <- coef(m2)[paste0('I(',var.aux,'^2):LON')]

# Get unique stations
stations.aux <- unique(df.aux[, c('STAID', 'LAT', 'LON')])
stations.aux$pred <- NA

# Calculate the mean of the variable
mean.var <- mean(global.df[idx.train,var.aux])
mean.var.2 <- mean(global.df[idx.train,var.aux]^2)
# Loop over all stations
for (i in 1:nrow(stations.aux)) {
  st  <- stations.aux$STAID[i]
  LAT <- stations.aux$LAT[i]
  LON <- stations.aux$LON[i]
  
  # Create a new column for the predicted value for this station
  stations.aux$pred[i] <- 
    coef.aux1           * mean.var   +
    coef.aux2           * mean.var.2 +
    coef.lat.aux1 * LAT * mean.var   +
    coef.lat.aux2 * LAT * mean.var.2 +
    coef.lon.aux1 * LON * mean.var   +
    coef.lon.aux2 * LON * mean.var.2 
}

################################################################################

library(sf)
library(gstat)
library(raster)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(gstat) 

# Convert to sf points
stations.sf <- st_as_sf(stations.aux, coords = c("LON", "LAT"), crs = 4326)

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
  scale_fill_viridis_c(option = "viridis", name = "") +
  coord_sf(xlim = c(-10, 5), ylim = c(35, 45), expand = FALSE) +
  labs(title = var.aux) +
  theme_minimal() +
  xlab('') + ylab('')

print(g.map)

ggsave(file.path(paste0(outdir,'/Terms/M2.',var.aux,'.map.png')), g.map, 
       width = 4, height = 3, bg = 'white')

################################################################################
# FOR A SPECIFIC DAY
# Linear terms

# Select variable of interest
var.aux <- "g700.45N.10W"

# Extract coefficient for the variable
coef.aux      <- coef(m2)[var.aux]
coef.lat.aux  <- coef(m2)[paste0(var.aux,':LAT')]
coef.lon.aux  <- coef(m2)[paste0(var.aux,':LON')]

# Copy of the data
df.plot <- df.aux

# Get unique stations
stations.aux <- unique(df.plot[, c('STAID', 'LAT', 'LON')])

# Loop over all stations
for (i in 1:nrow(stations.aux)) {
  st  <- stations.aux$STAID[i]
  LAT <- stations.aux$LAT[i]
  LON <- stations.aux$LON[i]
  
  # Create a new column for the predicted value for this station
  df.plot[[paste0("pred_", st)]] <- 
    coef.aux     * df.plot[[var.aux]] +
    coef.lat.aux * df.plot[[var.aux]] * LAT +
    coef.lon.aux * df.plot[[var.aux]] * LON
}

################################################################################
# Poly terms

# Select variable of interest
# Select variable of interest
var.aux <- "g700.45N.5E"

# Extract coefficient for the variable
coef.aux1      <- coef(m2)[paste0('poly(',var.aux,', 2)1')]
coef.aux2      <- coef(m2)[paste0('poly(',var.aux,', 2)2')]
coef.lat.aux1  <- coef(m2)[paste0('LAT:poly(',var.aux,', 2)1')]
coef.lat.aux2  <- coef(m2)[paste0('LAT:poly(',var.aux,', 2)2')]
coef.lon.aux1  <- coef(m2)[paste0('poly(',var.aux,', 2)1:LON')]
coef.lon.aux2  <- coef(m2)[paste0('poly(',var.aux,', 2)2:LON')]

# Poly term effects
X  <- model.matrix(m2)
p1 <- X[, paste0("poly(", var.aux, ", 2)1")]
p2 <- X[, paste0("poly(", var.aux, ", 2)2")]

# Copy of the data
df.plot <- df.aux

# Get unique stations
stations.aux <- unique(df.plot[, c('STAID', 'LAT', 'LON')])

# Loop over all stations
for (i in 1:nrow(stations.aux)) {
  st  <- stations.aux$STAID[i]
  LAT <- stations.aux$LAT[i]
  LON <- stations.aux$LON[i]
  
  # Create a new column for the predicted value for this station
  df.plot[[paste0("pred_", st)]] <- 
    coef.aux1     * p1 +
    coef.aux2     * p2 +
    coef.lat.aux1 * p1 * LAT +
    coef.lat.aux2 * p2 * LAT +
    coef.lon.aux1 * p1 * LON +
    coef.lon.aux2 * p2 * LON
}

################################################################################

# Filter for 7th August 2003
t_index <- 2003 - 1960 + 1  # 44
l_index <- 30 + 31 + 7       # 68
date <- '7 Aug 2003'
date.aux <- '07_08_2003'

df.day <- df.plot[df.plot$t == t_index & df.plot$l == l_index, ]

library(sf)
library(gstat)
library(raster)
library(ggplot2)
library(rnaturalearth)
library(dplyr)
library(gstat) 

# Prepare stations with predictions
# We'll take one predicted column per station (or average if multiple, here one per station)
pred_cols <- grep("^pred_", names(df.day), value = TRUE)
plot.df <- df.day %>%
  dplyr::select(STAID, LON, LAT, all_of(pred_cols)) %>%
  tidyr::pivot_longer(cols = all_of(pred_cols), names_to = "station", values_to = "pred") %>%
  mutate(station = gsub("pred_", "", station))  # clean station names

plot.df <- plot.df[which(plot.df$STAID == plot.df$station),]

# Convert to sf points
stations.sf <- st_as_sf(plot.df, coords = c("LON", "LAT"), crs = 4326)

# Download Spain polygon
world <- ne_countries(scale = "medium", returnclass = "sf")
region <- subset(world, admin %in% c("Spain", "Portugal", "France", "Morocco", "Algeria", "Andorra"))
spain <- subset(world, admin == "Spain")


# Create a regular grid over Spain
spain_bbox <- st_bbox(spain)
grid_res <- 0.05  # grid resolution in degrees (~5 km)
x_seq <- seq(spain_bbox["xmin"], spain_bbox["xmax"], by = grid_res)
y_seq <- seq(spain_bbox["ymin"], spain_bbox["ymax"], by = grid_res)
grid <- expand.grid(x = x_seq, y = y_seq)
grid.sf <- st_as_sf(grid, coords = c("x", "y"), crs = 4326)
grid.sf <- st_intersection(grid.sf, spain)  # keep points only inside Spain

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
  scale_fill_viridis_c(option = "viridis", name = "") +
  coord_sf(xlim = c(-10, 5), ylim = c(35, 45), expand = FALSE) +
  labs(title = paste0(var.aux,' - ',date)) +
  theme_minimal() +
  xlab('') + ylab('')

print(g.map)

ggsave(file.path(paste0(outdir,'/Terms/M2.',var.aux,'.',date.aux,'.pdf')), g.map, 
       width = 4, height = 3, bg = 'white')
