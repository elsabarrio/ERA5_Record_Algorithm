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
  theme_bw()
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

# Stations and colors by zone
outfile <- file.path(out_dir,"map_colzone.pdf")
pdf(file = outfile, width = 4, height = 4)
# Mapa with Iberic peninsula plus Balear islands
map(regions=sov.expand(c("Spain", "Portugal")),
    #mar = c(0,0,0,0),
    xlim=c(-10,5))
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
                     labels = c("1 (1960)", "21 (1980)", "41 (2000)", " 61 (2020)"))
show(g1c)
# Save
ggsave(filename = "trend_pt_Ix_zone.pdf",
       plot = g1c,
       device = "pdf",
       path = out_dir,
       width = 5,
       height = 4)



# Load libraries
if(!is.element("RecordTest", row.names(installed.packages()))) install.packages("RecordTest")
library(RecordTest)
if(!is.element("ggplot2", row.names(installed.packages()))) install.packages("ggplot2")
library(ggplot2)
if(!is.element("patchwork", row.names(installed.packages()))) install.packages("patchwork")
library(patchwork)

# Set data directory
data_dir <- file.path(getwd(),"Data")
if(!dir.exists(data_dir)) stop("data_dir not found")

# Data directory
outdir <- paste0(getwd(),"/Results/Exploratory/Trends/")
if(!dir.exists(outdir)) dir.create(outdir, recursive = T)

# Set data directory
data_dir <- file.path(getwd(),"Data")
if(!dir.exists(data_dir)) stop("data_dir not found")

# Read list of observatories of interest. Based on previous work.
stations <- read.csv("Data/geo_peninsula_zones.csv")

# Read Tx data
tx <- read.csv(file = file.path(data_dir,"Tx_mat.csv"))
tn <- read.csv(file = file.path(data_dir,"Tn_mat.csv"))
# Reshape data
LL <- 365
TT <- nrow(tx)/LL
SS <- nrow(stations)
tx3d <- array(data = as.matrix(tx[,-1]), dim = c(LL,TT,SS))

# Set inputs
summer_idx <- c(152:243)
geoalt <- c("700","500","300")

# Set stations indices
si_idx <- c(9, 10, 16, 2)

# For each station
for(ss in 1:length(si_idx)){
  
  # Read geopotential variables in nearest grid point for four cities
  bin_df <- read.csv(paste0(data_dir,"/Stations_dfs_complete/",stations$STAID[si_idx[ss]],".df.csv"))
  
  # Weighting function
  #wfunc <- function(t) ifelse(t == 1, 0, sqrt(t^2 / (t-1)))
  
  # Find nearest grid point
  npoint <- paste0(round(stations$LAT[si_idx[ss]]),"N.",
                   abs(round(stations$LON[si_idx[ss]])),
                   ifelse(stations$LON[si_idx[ss]]>0,"E","W"))
  
  # Reshape
  #corners <- c("45N.10W","45N.5E","35N.10W","35N.5E")
  #sta_idx <- c(14,10,33,15) # Coruña, Fabra, Huelva, Murcia
  nday <- length(table(bin_df$l))
  nt <- length(table(bin_df$t))
  
  # Apply record test N.plot
  # Get observed temperatures from selected station
  y <- t(tx3d[summer_idx,,si_idx[ss]])
  
  # Save number of records
  point_df <- data.frame(Tx=Nmean.record(y, "upper"))
  point_df$G700 <- point_df$G500 <- point_df$G300 <- 0
  cp_vec <- vector("numeric", 4)
  nt_vec <- vector("numeric", 4)
  cp_vec[1] <- change.point(X = series_uncor(y), record = "upper")$estimate
  nt_vec[1] <- N.test(X = series_uncor(y), record = "upper", alternative = "greater")$statistic
  N.plot(X = series_uncor(y), record = c(1,0,0,0))
  for(ii in 1:length(geoalt)){
    # Reshape data
    y <- t(matrix(bin_df[[paste0("g.",geoalt[ii],".",npoint)]],
                  nrow = nday))
    point_df[[paste0("G",geoalt[ii])]] <- Nmean.record(y, "upper")
    cp_vec[1+ii] <- change.point(X = series_uncor(y), record = "upper")$estimate
    nt_vec[1+ii] <- N.test(X = series_uncor(y), record = "upper", alternative = "greater")$statistic
    N.plot(X = series_uncor(y), record = c(1,0,0,0))
  }
  
  # Plot
  point_df$t <- 1:nrow(point_df)
  point_df$Nt <- cumsum(1/point_df$t)
  # Subtitle
  plot_subt <- paste0("G-",npoint," vs. ",stations$STANAME[si_idx[ss]])
  g2 <- ggplot(data = point_df, mapping = aes(x = t)) +
    geom_line(mapping = aes(y = Nt), color = "gray") +
    geom_point(mapping = aes(y = G300, color = "G300")) +
    geom_point(mapping = aes(y = G500, color = "G500")) +
    geom_point(mapping = aes(y = G700, color = "G700")) +
    geom_point(mapping = aes(y = Tx, color = "Tx")) +
    #geom_vline(xintercept=cp_vec[4], color ="red") +
    #geom_vline(xintercept=cp_vec[3], color ="chartreuse3") +
    #geom_vline(xintercept=cp_vec[2], color ="blue") +
    #geom_vline(xintercept=cp_vec[1], color ="black") +
    annotate("text", x=6, y=5, label = round(nt_vec[4],2), col = "blue") +
    annotate("text", x=6, y=4.6, label = round(nt_vec[3],2), col = "chartreuse3") +
    annotate("text", x=6, y=4.2, label = round(nt_vec[2],2), col = "red") +
    annotate("text", x=6, y=3.80, label = round(nt_vec[1],2), col = "black") +
    scale_color_manual(name = "",
                       breaks=c("G300",
                                "G500",
                                "G700",
                                "Tx"),
                       values=c("G300"="blue",
                                "G500"="chartreuse3",
                                "G700"="red",
                                "Tx"="black")) +
    ylab("Mean number of records") +
    labs(subtitle = plot_subt) +
    theme_bw()
  show(g2)
  # Save
  ggsave(filename = paste0("g",stations$STAID[si_idx[ss]],"_records.pdf"),
         width = 5,
         height = 3,
         plot = g2,
         device = "pdf",
         path = outdir)
  
}