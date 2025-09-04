# Clear workspace
rm(list = ls())

# Load libraries
library(ggplot2)
library(tidyr)

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
df.aux <- global.df[idx.train,]

# Read list of observatories of interest. Based on previous work.
stations <- read.csv(file.path(data_dir,"geo_peninsula_zones.csv"))
idx <- which(!is.na(stations$Zona))
stations <- stations[idx,]
# Abbreviations
stations$STANAME[10] <- "FABRA OBSERVATORY                       "
stations$STANAME[25] <- "BCN/AEROPUERTO                          "
stations$abb <- substr(stations$STANAME,1,6)

######################################################################################
# M1 model

# M1: Simple global model
# Define file path
model_file <- file.path(outdir, "m1.rds")
# Check if file exists
if(!file.exists(model_file)) stop("Model M1 not found!")
# File exists; read the formula
m1.frm <- readRDS(model_file)
# Compute the model
m1 <- glm(formula = m1.frm,
          data = global.df[idx.train,],
          family = binomial(link = "logit"))

##############################################################################
# Term effects

# Select variable of interest
#var.aux <- "g500.35N.10W"
var.aux <- "g700.45N.10W"

# Extract coefficient for the variable
coef.aux <- coef(m1)[var.aux]

# Define data frame and variable
df.aux$pred_fixed <- df.aux[[var.aux]] * coef.aux

# Create plot
library(ggplot2)

g1 <- ggplot(df.aux, aes(x = .data[[var.aux]], y = pred_fixed)) +
  geom_point(alpha = 0.5, color = "black") +      # semi-transparent points
  #geom_abline(intercept = 0, slope = coef.aux, color = "red", size = 1.2) +  # linear contribution
  labs(
    x = expression(paste('Geopotential ', m^{2}*s^{-2})),
    y = "",
    title = paste(var.aux)
  ) +
  theme_minimal(base_size = 14)

# Create folder if it does not exist
if (!dir.exists(file.path(outdir,'Terms'))) {
  dir.create(file.path(outdir,'Terms'), recursive = TRUE)
}

ggsave(file.path(paste0(outdir,'/Terms/M1.',var.aux,'.png')), g1, 
       width = 5, height = 3, bg = 'white')
##################################################################################
# Select variable of interest
#var.aux <- "g300.35N.5E"
var.aux <- "g700.45N.5E"

# Extract coefficient for the variable
coef.aux1 <- coef(m1)[paste0('poly(',var.aux,', 2)1')]
coef.aux2 <- coef(m1)[paste0('poly(',var.aux,', 2)2')]

# Poly term effects
x <- df.aux[[var.aux]]   # training values used in glm

# Create orthogonal polynomial basis with same scaling
p <- poly(x, 2)

X <- model.matrix(m2)
p1 <- X[, paste0("poly(", var.aux, ", 2)1")]
p2 <- X[, paste0("poly(", var.aux, ", 2)2")]


# Compute contribution
df.aux$pred_fixed_poly <- coef.aux1 * p1 + coef.aux2 * p2

g2 <- ggplot(df.aux, aes(x = .data[[var.aux]], y = pred_fixed_poly)) +
  geom_point(alpha = 0.5, color = "black") +      # semi-transparent points
  #geom_abline(intercept = 0, slope = coef.aux, color = "red", size = 1.2) +  # linear contribution
  labs(
    x = expression(paste('Geopotential ', m^{2}*s^{-2})),
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
# Compute the model
m2 <- glm(formula = m2.frm,
          data = global.df[idx.train,],
          family = binomial(link = "logit"))

##################################################################################
# m2 model terms that interact with lat and lon
# Poly m2 model terms that interact with lat and lon

# Select variable of interest
var.aux <- "g700.45N.10W"

# Extract coefficient for the variable
coef.aux      <- coef(m2)[var.aux]
coef.lat.aux  <- coef(m2)[paste0(var.aux,':LAT')]
coef.lon.aux  <- coef(m2)[paste0(var.aux,':LON')]

# Select stations of interest
stat.aux <- c('CORUNA','ZARAGOZA','MURCIA')
stations.aux <- stations[grepl(paste(stat.aux, collapse = '|'), stations$STANAME), c('STANAME','LAT','LON')]

df.plot <- df.aux  # make a working copy

# Create variables for each station
for (i in 1:nrow(stations.aux)) {
  
  st  <- stations.aux$STANAME[i]
  LAT <- stations.aux$LAT[i]
  LON <- stations.aux$LON[i]
  
  df.plot[[paste0("pred_", st)]] <-
    coef.aux     * df.aux[[var.aux]] +
    coef.lat.aux * df.aux[[var.aux]] * LAT +
    coef.lon.aux * df.aux[[var.aux]] * LON
}

# Change to long format to plot
df.long <- df.plot |>
  pivot_longer(
    cols = starts_with("pred_"),
    names_to = "STANAME",
    values_to = "pred"
  ) |>
  mutate(STANAME = sub("pred_", "", STANAME)) |>
  arrange(STANAME, .data[[var.aux]])

# Create plot
g3 <- ggplot(df.long, aes(x = .data[[var.aux]], y = pred, color = STANAME)) +
  #geom_line(size = 1) +
  geom_point() +
  labs(
    x = expression(paste('Geopotential ', m^{2}*s^{-2})),
    y = "",
    title = var.aux,
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian() +
  theme(
    plot.margin = margin(5, 5, 5, 5),  # small margin so legend doesn’t get pushed out
    legend.position = c(1.15, 1),   # inside, relative to panel
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 7),
    legend.title = element_text(size = 10),
    legend.key.size = unit(0.2, units = 'lines')
  ) 

ggsave(file.path(paste0(outdir,'/Terms/M2.',var.aux,'.png')), g3, 
       width = 5, height = 3, bg = 'white')

##################################################################################
# Poly m2 model terms that interact with lat and lon

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

# Select stations of interest
stat.aux <- c('CORUNA','ZARAGOZA','MURCIA')
stations.aux <- stations[grepl(paste(stat.aux, collapse = '|'), stations$STANAME), c('STANAME','LAT','LON')]

df.plot <- df.aux  # make a working copy

# Create variables for each station
for (i in 1:nrow(stations.aux)) {
  
  st  <- stations.aux$STANAME[i]
  LAT <- stations.aux$LAT[i]
  LON <- stations.aux$LON[i]
  
  df.plot[[paste0("pred_", st)]] <-
    coef.aux1     * p1 +
    coef.aux2     * p2 +
    coef.lat.aux1 * p1 * LAT +
    coef.lat.aux2 * p2 * LAT +
    coef.lon.aux1 * p1 * LON +
    coef.lon.aux2 * p2 * LON
}

# Change to long format to plot
df.long <- df.plot |>
  pivot_longer(
    cols = starts_with("pred_"),
    names_to = "STANAME",
    values_to = "pred"
  ) |>
  mutate(STANAME = sub("pred_", "", STANAME)) |>
  arrange(STANAME, .data[[var.aux]])

# Create plot
g4 <- ggplot(df.long, aes(x = .data[[var.aux]], y = pred, color = STANAME)) +
  #geom_line(size = 1) +
  geom_point() +
  labs(
    x = expression(paste('Geopotential ', m^{2}*s^{-2})),
    y = "",
    title = var.aux,
    color = ""
  ) +
  theme_minimal(base_size = 14) +
  coord_cartesian() +
  theme(
    plot.margin = margin(5, 5, 5, 5),  # small margin so legend doesn’t get pushed out
    legend.position = c(1.2, 1.2),   # inside, relative to panel
    legend.justification = c("right", "top"),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10)
    #legend.key.size = unit(0.4, "lines")
  ) 

ggsave(file.path(paste0(outdir,'/Terms/M2.',var.aux,'.png')), g4, 
       width = 5, height = 3, bg = 'white')
