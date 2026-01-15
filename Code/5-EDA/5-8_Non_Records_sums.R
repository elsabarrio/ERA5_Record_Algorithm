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
dim(sum.itx)
zero.prop <- rowSums(sum.itx == 0)/92
stat.prop <- (1-1/(25:64))^36

library(ggplot2)

g1 <- ggplot(data = data.frame(x = 1984:2023), aes(x = x)) +
  geom_point(aes(y = zero.prop), color = "blue", size = 1.2) +
  geom_smooth(aes(y = zero.prop), method = "loess", se = FALSE, color = "blue", size = 1, span = 3) +  # LOESS for zero.prop
  geom_line(aes(y = stat.prop), color = "red", size = 1) +
  labs(x = "Year", y = "Proportion of non-records") +
  ylim(c(0,1))+
  scale_x_continuous(breaks = seq(1984, 2023, by = 4)) +  # Labels every 2 years
  theme_minimal()+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),          # Increase axis values size
    legend.text = element_text(size = 15)        # Increase legend text size
  )

# Set outdir
outdir.df.save <- "Results/Exploratory/Trends"

ggsave(filename = "trend_non_records_prop.pdf",plot = g1, 
       device = "pdf", path = file.path(outdir.df.save, "Plots/"),
       width = 5, height = 3.5, bg = "white")
