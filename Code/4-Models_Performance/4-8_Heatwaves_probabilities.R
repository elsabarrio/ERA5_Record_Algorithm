# Clear workspace
rm(list = ls())

# Load libraries
library(ggplot2)
library(tidyr)
library(dplyr)
if(!is.element("pROC", row.names(installed.packages()))) install.packages("pROC")
library(pROC)
library(maps)
library(rnaturalearth)
library(rnaturalearthdata)

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
# M2 model and predictions

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

# Predict
predict.aux <- predict(object = m2,
                       newdata = global.df[-idx.train,],
                       type = "response")

# Create new data frame with predictions
predict.df <- global.df[-idx.train,]
predict.df$predict <- predict.aux
predict.df <- predict.df %>%
  dplyr::select(STAID, Ix, t, l, LAT, LON, predict)
################################################################################

# August 2023

# 22th of August 2023: itx3d[64,82,]
# 23th of August 2023: itx3d[64,83,]
# 24th of August 2023: itx3d[64,84,]
# 25th of August 2023: itx3d[64,85,]

# June 2022

# 16th of June 2022: itx3d[63,16,]
# 17th of June 2022: itx3d[63,17,]
# 18th of June 2022: itx3d[63,18,]

# July 2022

# 15th of July 2022: itx3d[63,44,]
# 16th of July 2022: itx3d[63,45,]
# 17th of July 2022: itx3d[63,46,]

# June 2017

# 17th of June 2017: itx3d[58,17,]
# 18th of June 2017: itx3d[58,18,]
# 19th of June 2017: itx3d[58,19,]

# Load countries
spain <- ne_countries(scale = "medium", returnclass = "sf", country = "Spain")
portugal <- ne_countries(scale = "medium", returnclass = "sf", country = "Portugal")
france <- ne_countries(scale = "medium", returnclass = "sf", country = "France")
morocco <- ne_countries(scale = "medium", returnclass = "sf", country = "Morocco")
algeria <- ne_countries(scale = "medium", returnclass = "sf", country = "Algeria")

# Create iberian peninsula
iberian_peninsula <- rbind(france, spain, portugal, morocco, algeria)

# 22-26 of August 2023
days.aux <- c(82,83,84,85,86)
years.aux <- c(64,64,64,64,64)
days.names <- c('22nd of August 2023', '23rd of August 2023', '24th of August 2023',
                '25th of August 2023', '26th of August 2023')
date.names <- c('22.08.23', '23.08.23', '24.08.23',
                '25.08.23', '26.08.23')

# 8-12 of august 2023
days.aux <- c(68, 69, 70, 71, 72)
years.aux <- c(64, 64, 64, 64, 64)

days.names <- c(
  '8th of August 2023',
  '9th of August 2023',
  '10th of August 2023',
  '11th of August 2023',
  '12th of August 2023'
)

date.names <- c(
  '08.08.23',
  '09.08.23',
  '10.08.23',
  '11.08.23',
  '12.08.23'
)

# 27-31 of July 2003
days.aux <- c(56,57,58,59,60)
years.aux <- c(44, 44, 44, 44, 44)

days.names <- c(
  '27th of July 2003',
  '28th of July 2003',
  '29th of July 2003',
  '30th of July 2003',
  '31st of July 2003'
)

date.names <- c(
  '27.07.03',
  '28.07.03',
  '29.07.03',
  '30.07.03',
  '31.07.03'
)

# 1-5 of august 2003
days.aux <- c(61, 62, 63, 64, 65)
years.aux <- c(44, 44, 44, 44, 44)

days.names <- c(
  '1st of August 2003',
  '2nd of August 2003',
  '3rd of August 2003',
  '4th of August 2003',
  '5th of August 2003'
)

date.names <- c(
  '01.08.03',
  '02.08.03',
  '03.08.03',
  '04.08.03',
  '05.08.03'
)


# 6-10 of august 2003
days.aux <- c(66, 67, 68, 69, 70)
years.aux <- c(44, 44, 44, 44, 44)

days.names <- c(
  '6th of August 2003',
  '7th of August 2003',
  '8th of August 2003',
  '9th of August 2003',
  '10th of August 2003'
)

date.names <- c(
  '06.08.03',
  '07.08.03',
  '08.08.03',
  '09.08.03',
  '10.08.03'
)

# 11-18 of august 2021
days.aux <- c(71,72,73,74,75,76,77,78)
years.aux <- c(62, 62, 62, 62, 62,62,62,62)

days.names <- c(
  '11th of August 2021',
  '12th of August 2021',
  '13th of August 2021',
  '14th of August 2021',
  '15th of August 2021',
  '16th of August 2021',
  '17th of August 2021',
  '18th of August 2021'
)

date.names <- c(
  '11.08.21',
  '12.08.21',
  '13.08.21',
  '14.08.21',
  '15.08.21',
  '16.08.21',
  '17.08.21',
  '18.08.21'
)

for (i in 1:length(days.aux)){
  # Get day and year
  day.aux <- days.aux[i]
  year.aux <- years.aux[i]
  
  # Select day in data frame
  plot.df <- predict.df[which(predict.df$t == year.aux & predict.df$l == day.aux),]
  
  # Create map with records on a specific day
  map.true <- ggplot() +
    geom_sf(data = iberian_peninsula, fill = "white", color = "black") +
    geom_point(data = plot.df, aes(x = LON, y = LAT, fill = factor(Ix)), size = 3, shape = 21, color = "black") +
    scale_fill_manual(values = c("0" = "white", "1" = "black")) + 
    coord_sf(xlim = c(-10, 5), ylim = c(35, 45)) +
    theme_minimal() +
    labs(title = days.names[i],
         x = "",
         y = "",
         size = 16) +
    guides(fill = guide_legend(title = ""),
           shape = guide_legend(title = "")) +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15),
      axis.title.x = element_text(size = 15), 
      axis.title.y = element_text(size = 15),
      axis.text.x = element_text(size = 15, angle = 45, hjust = 1,  face = "bold"),
      axis.text.y = element_text(size = 15)
    )
  
  # Create plot with simulated values
  map.sim <- ggplot() +
    geom_sf(data = iberian_peninsula, fill = "white", color = "black") +
    geom_point(data = plot.df, aes(x = LON, y = LAT, fill = predict), size = 3, shape = 21, color = "black") +
    #scale_fill_gradient(low = "white", high = "blue", limits = c(0,0.8)) +
    scale_fill_gradientn(
      colours = c(
        "#d73027",  # red (0)
        "#7b3294",  # purple (0.2)
        "#4575b4",  # blue (0.4)
        "#1a9850",  # green (0.6)
        "#fee08b"   # yellow (0.8)
      ),
      values = scales::rescale(c(0, 0.2, 0.4, 0.6, 0.8)),
      limits = c(0, 0.8)
    )+
    coord_sf(xlim = c(-10, 5), ylim = c(35, 45)) +
    theme_minimal() +
    labs(title = days.names[i],
         x = "",
         y = "",
         size = 16) +
    guides(fill = guide_legend(title = "Prob"),
           shape = guide_legend(title = "Prob")) +
    theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      legend.title = element_text(size = 15),
      legend.text = element_text(size = 15),
      axis.title.x = element_text(size = 15), 
      axis.title.y = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.text.x = element_text(size = 15, angle = 45, hjust = 1,  face = "bold")
    )
  
  # Save
  ggsave(file.path(getwd(),paste0("Results/final_models/Heatwaves/",date.names[i],"map.true.pdf")), plot = map.true, width = 5, height = 4, units = "in", dpi = 300, bg = "white")
  ggsave(file.path(getwd(),paste0("Results/final_models/Heatwaves/",date.names[i],"map.sim.pdf")), plot = map.sim, width = 5, height = 4, units = "in", dpi = 300, bg = "white")

  
}
