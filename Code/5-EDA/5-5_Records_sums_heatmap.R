# Clear workspace
rm(list = ls())

# Set data directory
data_dir <- file.path(getwd(),"Data")
if(!dir.exists(data_dir)) stop("data_dir not found")

# Read list of observatories of interest. Based on previous work.
stations <- read.csv("Data/geo_peninsula_zones.csv")

# Read Tx data
tx <- read.csv(file = file.path(data_dir,"tx_data/Tx_mat.csv"))
# Reshape data
LL <- 365
TT <- nrow(tx)/LL
SS <- nrow(stations)
tx3d <- array(data = as.matrix(tx[,-1]), dim = c(LL,TT,SS))
if(sum(is.na(tx3d))>0) tx3d[is.na(tx3d)] <- -9999

# Set seasons
summer_idx <- c(152:243)

# Compute upper records (indicators) for summer data only
upp.rcrd <- function(x) c(1,as.numeric(diff(cummax(x))>0))
itx3d <- apply(X = tx3d[summer_idx,,], MARGIN = c(1,3), upp.rcrd)

# Do not consider (for now) more than one station from Madrid
stations$STANAME[which(is.na(stations$Zona))]
idx <- which(!is.na(stations$Zona))
stations <- stations[idx,]
itx3d <- itx3d[,,idx]
voldim <- dim(itx3d)

# Modify Barcelona-Airport label
stations$STANAME[10] <- "FABRA OBSERVATORY                       "
stations$STANAME[25] <- "BCN/AEROPUERTO                          "
stations$abb <- substr(stations$STANAME,1,6)

#52:64
# Sum records over all stions
sum.itx <- apply(itx3d[25:64,,], c(1,2), sum)

# Create a heatmap for records sums
library(ggplot2)
library(reshape2)

# Convert the matrix to a data frame for ggplot2
sum.itx.df <- melt(sum.itx)

# Set outdir
outdir.df.save <- "Results/Exploratory/"


library(dplyr)
library(ggplot2)

sum.itx.df <- sum.itx.df %>%
  mutate(day = as.Date(Var2 - 1, origin = "2011-06-01"))

Sys.setlocale("LC_TIME", "C")

y_labels <- c("1 Jun", "1 Jul", "1 Aug")
y_breaks <- c(1, 32, 63)

g.records.sum.aux <- ggplot(sum.itx.df, aes(Var1 + 1983, Var2, fill = value/36)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  scale_x_continuous(breaks = seq(min(sum.itx.df$Var1 + 1983), max(sum.itx.df$Var1 + 2010), by = 2)) +
  scale_y_continuous(
    breaks = y_breaks,
    labels = y_labels
  ) +
  labs(x = "", y = "", fill = "") +
  ggtitle('') + # "Record sum in last 13 years and 92 summer days"
  theme_minimal() +# Get really hot days
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 15),          # Increase axis values size
    legend.text = element_text(size = 15)        # Increase legend text size
    )

ggsave(filename = "records.heat.map.aux.pdf",plot = g.records.sum.aux, 
       device = "pdf", path = file.path(outdir.df.save),
       width = 8, height = 3.5, bg = "white")
