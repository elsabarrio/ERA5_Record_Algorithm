# Clear workspace
rm(list = ls())

# Set data directory
data_dir <- file.path(getwd(),"Data")
if(!dir.exists(data_dir)) stop("data_dir not found")

# Read list of observatories of interest. Based on previous work.
stations <- read.csv("Data/geo_peninsula_zones.csv")

# Read records
itx3d <- readRDS("Data/record_data/recordvol.rds")

# Do not consider (for now) more than one station from Madrid
stations$STANAME[which(is.na(stations$Zona))]
idx <- which(!is.na(stations$Zona))
stations <- stations[idx, ]

# Modify Barcelona-Airport label
stations$STANAME[10] <- "FABRA OBSERVATORY                       "
stations$STANAME[25] <- "BCN/AEROPUERTO                          "
stations$abb <- substr(stations$STANAME,1,6)


# Set outdir
outdir.df.save <- "Results/Exploratory/"


library(dplyr)
library(ggplot2)


# August 2023

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

# Read predictions
predict.df.aux <- read.csv('Results/final_models/Predicts/preds_m2.csv')

# Define parameters of specific day
hot.day <- itx3d[64,84,]
foot <- "24th of August 2023"
predict.df <- predict.df.aux[which(predict.df.aux$t == 64 & predict.df.aux$l == 84),c(1,5)]
date.name <- "23.08.24."

#test <- merge(stations, predict.df, by = "STAID")

# Create data frame with real and simulated values
df.sim.true.aux <- data.frame(#sim = test$predict.test,
  sim = predict.df[match(stations$STAID, predict.df$STAID),2],
  true = as.integer(hot.day),
  names = stations$abb,
  lat = stations$LAT,
  lon = stations$LON)
#names = test$abb)

#df.sim.true <- df.sim.true.aux[order(stations$CoastDist),]
df.sim.true <- df.sim.true.aux[order(stations$CoastDist),]
df.sim.true$names <- factor(df.sim.true$names, levels = unique(df.sim.true$names))

# Plot real and simulated values
library(ggplot2)
library(gridExtra)

g1 <- ggplot(data = df.sim.true, aes(x = names)) +
  # Simulated values
  geom_point(aes(y = sim, fill = factor(true)), color = "black", shape = 21, size = 3, stroke = 0.5) +
  # Real values
  geom_point(aes(y = 0, fill = factor(true)), shape = 22, size = 4) +
  scale_fill_manual(values = c("white", "black")) +
  scale_fill_identity() +
  labs(x = "Stations", y = "Simulated Record",
       title = foot) +
  #ggtitle("Green models' predicitons in a hot August day of 2023") +
  ggtitle("M2 model's predictions") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        axis.ticks.x = element_blank(),
        plot.title = element_text(size = 20),         # Increase title size
        axis.title = element_text(size = 20),         # Increase axis labels size
        axis.text = element_text(size = 13)          # Increase axis values siz3
        ) +
  geom_abline(intercept = 0.2565641, slope = 0, linetype = "dashed")+
  coord_cartesian(ylim = c(0, 0.5))  # Set y-axis limits from 0 to 1


ggsave(filename = paste0(date.name,"M2.predictions.png"),plot = g1, 
device = "png", path = file.path("Results/final_models/Heatwaves/"),
width = 8, height = 6, bg = "white")
