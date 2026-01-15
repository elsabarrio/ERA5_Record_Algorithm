# Clear workspace
rm(list = ls())

# Save graphical parameters
op <- par()

# Load libraries
if(!is.element("ggplot2", row.names(installed.packages()))) install.packages("ggplot2")
library(ggplot2)
if(!is.element("RColorBrewer", row.names(installed.packages()))) install.packages("RColorBrewer")
library(RColorBrewer)
if(!is.element("maps", row.names(installed.packages()))) install.packages("maps")
library(maps)
library(grid)
library(cowplot)
library(RColorBrewer)
library(maps)
library(car)


# Set data directory
data_dir <- "Data"
if(!dir.exists(data_dir)) stop("data_dir not found")

# Read list of observatories of interest. Based on previous work.
stations <- read.csv(file.path(data_dir,"geo_peninsula_zones.csv"))
idx <- which(!is.na(stations$Zona))
stations <- stations[idx,]

# Read Tx record data
itx3d <- readRDS(file = file.path(data_dir, "record_data", "recordvol.rds"))
voldim <- dim(itx3d)

# Data directory
out_dir <- "Results/Exploratory/Trends"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = T)

###############################################################################
# Trend
###############################################################################

# Response trends
# Compute yearly trend
trend_Ix <- apply(X = itx3d, MARGIN = 1, function(x) mean(x, na.rm = T))
# Plot inputs
trnd_df <- data.frame(Itx=trend_Ix,
                      t=1:voldim[1])
# Plot
g1a <- ggplot(data = trnd_df,
              mapping =  aes(x=t)) +
  geom_hline(yintercept=1, color = "gray") +
  geom_line(aes(y=Itx*t), color="black") +
  geom_smooth(aes(y=Itx*t), color="black", se = F, linetype = "dashed") +
  ylim(c(0, NA)) +
  ylab(expression(t%*%hat(p)[t])) +
  xlab("t (year)") +
  scale_x_continuous(breaks = c(1, 21, 41, 61),
                     labels = c("1 (1960)", "21 (1980)", "41 (2000)", " 61 (2020)")) +
  theme_minimal() +
  theme(    
    axis.title = element_text(size = 15),         # Increase axis labels size
    axis.text = element_text(size = 15),          # Increase axis values size
  )
show(g1a)
ggsave(filename = "trend_pt_Ix.pdf",
       plot = g1a,
       device = "pdf",
       path = out_dir,
       width = 5,
       height = 4)

# Plot by geographic zone
stations$Zona <- factor(stations$Zona)
levels(stations$Zona) <- c("A","C","E","G","L","NP","SP")

# Compute centroids per zone
zone_centers <- aggregate(
  cbind(LON, LAT) ~ Zona,
  data = stations,
  FUN = mean
)

# Stations and colors by zone
outfile <- file.path(out_dir,"map_colzone.png")
png(file = outfile, width = 800, height = 600, res = 150, bg = "transparent")
# Mapa with Iberic peninsula plus Balear islands
map(regions=sov.expand(c("Spain", "Portugal")),
    #mar = c(0,0,0,0),
    xlim=c(-10,5),
    fill = FALSE)
# Set zone colors for each station
auxcol <- brewer.pal(7, "Set1")[as.integer(stations$Zona)]
# Include the stations points
points(x = stations$LON,
       y = stations$LAT,
       col = "black",
       bg = auxcol, 
       pch = 21,
       cex = 1.5)
#abline(h=c(36:44), v=c(-10:10), lty=3)
# Colors per zone (same palette)
zone_cols <- brewer.pal(7, "Set1")[as.integer(zone_centers$Zona)]
library(car)
dx <- c(A = 0.1, C = 0, E = 0, G = 0.6, L = 0.3, NP = 0.1, SP = 0)
dy <- c(A = 0.4, C = -0.5, E = 0.2, G = 0, L = 0, NP = 0.25, SP = -0.2)
text(
  zone_centers$LON + dx[zone_centers$Zona],
  zone_centers$LAT + dy[zone_centers$Zona],
  labels = zone_centers$Zona,
  col = zone_cols,
  font = 2,
  cex = 1.05
)
dev.off()

# Probability of record trends by zone (Tx)

# Compute yearly trend by group
trend_tx <- apply(X = itx3d, MARGIN = c(1,3), function(x) mean(x, na.rm = T))
# Average by zones
trend_zn <- aggregate(x = t(trend_tx), by = list(stations$Zona), FUN=mean) 
# Plot inputs
trend_df <- data.frame(p = c(t(as.matrix(trend_zn[,-1]))),
                       Zone = rep(levels(stations$Zona), each = voldim[1]),
                       t = rep(1:voldim[1],nlevels(stations$Zona)))
# Figure
g1c <- ggplot(data = trend_df,
              mapping =  aes(x=t, y=p*t, color = Zone)) +
  geom_hline(yintercept=1,
             color = "black") +
  geom_smooth(method = "loess",
              formula = "y ~ x",
              se=F) +
  coord_cartesian(ylim=c(0,4.5)) +
  theme_bw() +
  scale_color_brewer(palette = "Set1") +
  ylab(expression(t %*% hat(p)[t])) +
  xlab("t (year)") +
  scale_x_continuous(breaks = c(1, 21, 41, 61),
                     labels = c("1 (1960)", "21 (1980)", "41 (2000)", " 61 (2020)"))+  
  theme_minimal() +
  theme(    
    axis.title = element_text(size = 15),         # Increase axis labels size
    axis.text = element_text(size = 15),          # Increase axis values size
    legend.position = 'none'
  )
show(g1c)
# Save
ggsave(filename = "trend_pt_Ix_zone.pdf",
       plot = g1c,
       device = "pdf",
       path = out_dir,
       width = 5,
       height = 4)

library(cowplot)
library(png)
library(grid)
map_img  <- readPNG(file.path(out_dir,"map_colzone.png"))
map_grob <- rasterGrob(map_img, interpolate = TRUE)

final_plot <- ggdraw(g1c) +
  draw_plot(
    map_grob,
    x = 0,   # left
    y = 0.25,   # top
    width = 0.9,
    height = 0.9
  )

final_plot
ggsave(filename = "trend_pt_Ix_zone_map.pdf",
       plot = final_plot,
       device = "pdf",
       path = out_dir,
       width = 5,
       height = 4)
###############################################################################
# Persistence
###############################################################################

# Read Tx data
tx <- read.csv(file = file.path(data_dir,"tx_data","Tx_mat.csv"))
# Reshape data
LL <- 365
TT <- nrow(tx)/LL
SS <- nrow(stations)
tx3d <- array(data = as.matrix(tx[,-1]), dim = c(LL,TT,SS))
if(sum(is.na(tx3d))>0) tx3d[is.na(tx3d)] <- -9999 # Remove NAs

# Compute upper records (indicators) for summer data only
upp.rcrd <- function(x) c(1,as.numeric(diff(cummax(x))>0))
itx3d <- apply(X = tx3d, MARGIN = c(1,3), upp.rcrd)
# Compute lagged records
source("R/lag3d.R")
itx3d_lag1 <- lag3d(itx3d)
# Extract only JJA
summer_idx <- c(152:243)
itx3d <- itx3d[,summer_idx,]
itx3d_lag1 <- itx3d_lag1[,summer_idx,]
voldim <- dim(itx3d)

# Concurrence
# Load 3d volume functions
source("R/or3d.R")
# Persistence
or_df <- or3d(itx3d,itx3d_lag1)

# Plot
g2a <- ggplot(data = or_df[-1,],
              mapping =  aes(x=t, y=log(OR))) +
  geom_line() +
  stat_smooth(se = F, color="black", linetype = "dashed") +
  ylab(expression(LOR[t])) +
  xlab("t (year)") +
  scale_x_continuous(breaks = c(1, 21, 41, 61),
                     labels = c("1 (1960)", "21 (1980)", "41 (2000)", " 61 (2020)")) +
  theme_minimal() +
  theme(    
    axis.title = element_text(size = 15),         # Increase axis labels size
    axis.text = element_text(size = 15),          # Increase axis values size
    legend.position = 'none'
  )
show(g2a)
# Save
ggsave(filename = "trend_LOR.pdf",
       plot = g2a,
       device = "pdf",
       path = out_dir,
       width = 5,
       height = 4)
