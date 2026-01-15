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

# Calculate mean and sd of geopotentials
avg.tx.g300 <- mean(global.df[which(global.df$t >=22 & global.df$t <= 51) ,paste0("g300.")])
avg.tx.g500 <- mean(global.df[which(global.df$t >=22 & global.df$t <= 51) ,paste0("g500.")])
avg.tx.g700 <- mean(global.df[which(global.df$t >=22 & global.df$t <= 51) ,paste0("g700.")])
sd.tx.g300 <- sd(global.df[which(global.df$t >=22 & global.df$t <= 51) ,paste0("g300.")])
sd.tx.g500 <- sd(global.df[which(global.df$t >=22 & global.df$t <= 51) ,paste0("g500.")])
sd.tx.g700 <- sd(global.df[which(global.df$t >=22 & global.df$t <= 51) ,paste0("g700.")])


# Create new data frame with predictions
predict.df <- global.df[-idx.train,]
predict.df$predict <- predict.aux
predict.df <- predict.df %>%
  dplyr::mutate(
    g300. = (g300.- avg.tx.g300)/sd.tx.g300,
    g500. = (g500.- avg.tx.g500)/sd.tx.g500,
    g700. = (g700.- avg.tx.g700)/sd.tx.g700
  ) %>%
  dplyr::select(STAID, Ix, g300., g500., g700., t, l, LAT, LON, predict)
predict.df$STANAME <- stations$STANAME[match(predict.df$STAID, stations$STAID)]
predict.df$abb <- stations$abb[match(predict.df$STAID, stations$STAID)]
################################################################################

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

# 1-10 of august 2003
days.aux <- c(61, 62, 63, 64, 65,66,67,68,69,70)
year <- 44
foot <- '1st to 10th of August of 2003'
date.name <- '.Aug.2003'

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

# 10-19 of august 2021
days.aux <- c(70,71,72,73,74,75,76,77,78,79)
year <- c(62)
foot <- '10th to 19th of August of 2021'
date.name <- '.Aug.2021'

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


# 18-27 of August 2023
days.aux <- c(78,79,80,81,82,83,84,85,86,87)
years.aux <- c(64,64,64,64,64,64,64,64,64,64)
foot <- '18th to 27th of August of 2023'
date.name <- '.Aug.2023'
year <- c(64)

###########################################################################
stat.aux <- 'MADRID'
stat.name.aux <- 'MADRID'
predict.df.aux <- predict.df[grep(stat.aux,predict.df$STANAME),]
predict.df.aux <- predict.df.aux[predict.df.aux$l %in% days.aux,]
predict.df.aux <- predict.df.aux[predict.df.aux$t == year,]

#predict.df.aux$predict[4] <- 0.49
#predict.df.aux$predict[5] <- 0.5
# Now create plot

if(!is.element("RColorBrewer", row.names(installed.packages()))) install.packages("RColorBrewer")
library(RColorBrewer)
library(ggplot2)
# Plot
colorcito <- brewer.pal(n = 8, name = "Set1")

predict.df.aux$M2 <- predict.df.aux$predict

g1 <- ggplot(data = predict.df.aux, aes(x = l)) +
  
  # --- Geopotentials ---
  geom_point(aes(y = g300., color = "G300"), 
             shape = 21, size = 2, stroke = 1, fill = colorcito[1]) +
  geom_point(aes(y = g500., color = "G500"), 
             shape = 21, size = 2, stroke = 1, fill = colorcito[2]) +
  geom_point(aes(y = g700., color = "G700"), 
             shape = 21, size = 2, stroke = 1, fill = colorcito[3]) +
  
  geom_line(aes(y = g300., color = "G300")) +
  geom_line(aes(y = g500., color = "G500")) +
  geom_line(aes(y = g700., color = "G700")) +
  
  # --- Prediction line ---
  geom_line(aes(y = predict * 10 - 2, color = "M2"), linetype = "dashed") +
  geom_point(aes(y = predict * 10 - 2, color = "M2"),
             shape = 21, size = 2, fill = colorcito[4]) +
  
  # --- Real records at y = -2 ---
  geom_point(aes(y = -2, fill = factor(Ix)), 
             shape = 22, size = 4, color = "black") +
  scale_fill_manual(values = c("0" = "white", "1" = "black")) +
  scale_fill_identity() +
  
  # --- Colour scale for curves ---
  scale_color_manual(
    name = "",
    breaks = c("G300", "G500", "G700", "M2"),
    values = c(
      "G300" = colorcito[1],
      "G500" = colorcito[2],
      "G700" = colorcito[3],
      "M2"   = colorcito[4]
    )
  ) +
  
  # --- Axis settings ---
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(
    limits = c(-2, 3),
    name = "z",
    sec.axis = sec_axis(~ (. + 2) / 10, name = "")
  ) +
  
  labs(
    title = paste0(stat.name.aux,' | ', foot),
    x = "Day",
    y = "z"
  ) +
  
  theme_minimal()+
  theme(
    plot.title = element_text(size = 15, hjust = 0.5),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    axis.title.x = element_text(size = 15), 
    axis.title.y = element_text(size = 15),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    legend.position = 'none'
  )

print(g1)

# Set outdir
outdir <- "Results/final_models/Heatwaves"

ggsave(filename = paste0(stat.aux,date.name,'.pdf'), plot = g1,
       device = "pdf", path = outdir, 
       width = 6, height = 4)


library(cowplot)

g1_with_legend <- g1 +
  theme(legend.position = "right")

legend_g1 <- get_legend(g1_with_legend, return_all = FALSE)

ggsave(
  filename = "legend.pdf",
  plot = legend_g1,
  device = "pdf",
  path = outdir,
  width = 1,
  height = 2.5
)

