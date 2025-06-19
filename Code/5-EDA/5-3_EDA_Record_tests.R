#!/usr/bin/Rscript

# Clear workspace
rm(list = ls())

# Save graphical parameters
op <- par()

# Load libraries
if(!is.element("RecordTest", row.names(installed.packages()))) install.packages("RecordTest")
library(RecordTest)
if(!is.element("ggplot2", row.names(installed.packages()))) install.packages("ggplot2")
library(ggplot2)
if(!is.element("patchwork", row.names(installed.packages()))) install.packages("patchwork")
library(patchwork)

# Set data directory
data_dir <- "Data"
if(!dir.exists(data_dir)) stop("data_dir not found")

# Data directory
out_dir <- "Results/Exploratory/Tests"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = T)

# Read list of observatories of interest. Based on previous work.
stations <- read.csv(file.path(data_dir,"geo_peninsula_zones.csv"))
idx <- which(!is.na(stations$Zona))
stations <- stations[idx,]
# Set "corner" stations indices
si_idx <- c(14,10,32,15) # Coruña, Fabra, Huelva, Murcia
# Relabel
stations$STANAME[si_idx[1]] <- "A CORUÑA"
stations$STANAME[si_idx[2]] <- "BARCELONA-FABRA"
stations$STANAME[si_idx[3]] <- "HUELVA"
stations$STANAME[si_idx[4]] <- "MURCIA"
show(stations$STANAME[si_idx])
# Set corner grid points
corners <- c("45N.10W","45N.5E","35N.10W","35N.5E")

# Read Tx data
tx <- read.csv(file = file.path(data_dir, "tx_data","Tx_mat.csv"))
tx$Date <- as.Date(tx$Date)
idx.d <- which(as.numeric(format(tx$Date,"%m")) >=6 &
                 as.numeric(format(tx$Date,"%m")) <= 8)
tx <- tx[idx.d,]
# Reshape data
LL <- 92
TT <- nrow(tx)/LL
SS <- nrow(stations)
tx3d <- array(data = as.matrix(tx[,-1]), dim = c(LL,TT,SS))

# Read grid data
geoalt <- c("700","500","300")
g300 <- read.csv(file.path(data_dir,"grid_data","g300_grid.csv"))
g500 <- read.csv(file.path(data_dir,"grid_data","g500_grid.csv"))
g700 <- read.csv(file.path(data_dir,"grid_data","g700_grid.csv"))

set.seed(19430319) # M. Molina's DOB
# For each station
for(ss in 1:length(si_idx)){
  
  # Apply record test N.plot
  print(stations$STANAME[si_idx[ss]])
  
  # Store N.test z-statistic
  ntz_vec <- ntp_vec <- vector("numeric", 4)
  
  # Get observed temperatures from selected station
  y <- t(tx3d[,,si_idx[ss]])
  #N.plot(X = y, record = c(1,0,0,0))
  ynt <- N.test(X = y,
                record = "upper",
                alternative = "greater",
                permutation.test = TRUE,
                B = 10000)
  ntz_vec[1] <- ynt$statistic
  ntp_vec[1] <- ynt$p.value
  
  # Save number of records
  point_df <- data.frame(Tx=Nmean.record(y, "upper"))
  point_df$G700 <- point_df$G500 <- point_df$G300 <- 0
  
  # Now applied to geopotentials
  for(ii in 1:length(geoalt)){
    
    glab <- paste0("g",geoalt[ii])
    cat(paste0("..",glab))
    # Get data
    y <- t(matrix(get(glab)[[paste0(glab,".",corners[ss])]],
                  nrow = LL))
    # Calculate averaged Nt
    point_df[[paste0("G",geoalt[ii])]] <- Nmean.record(y, "upper")
    # Calculate Nt.test
    ynt <- N.test(X = y,
                  record = "upper",
                  alternative = "greater",
                  permutation.test = TRUE,
                  B = 10000)
    ntz_vec[1+ii] <- ynt$statistic
    ntp_vec[1+ii] <- ynt$p.value
  }
  
  # Plot
  point_df$t <- 1:nrow(point_df)
  point_df$Nt <- cumsum(1/point_df$t)
  # Subtitle
  plot_subt <- paste0("G-",corners[ss]," vs. ",stations$STANAME[si_idx[ss]])
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
    #annotate("text", x=55, y=2.8, label = round(ntz_vec[4],2), col = "blue") +
    #annotate("text", x=55, y=2.4, label = round(ntz_vec[3],2), col = "chartreuse3") +
    #annotate("text", x=55, y=1.9, label = round(ntz_vec[2],2), col = "red") +
    #annotate("text", x=55, y=1.5, label = round(ntz_vec[1],2), col = "black") +
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
    xlab("t (year)") +
    ylim(0.9,6.1) +
    scale_x_continuous(breaks = c(1, 21, 41, 61),
                       labels = c("1 (1960)", "21 (1980)", "41 (2000)", " 61 (2020)")) +
    labs(subtitle = plot_subt) +
    theme_bw()
  show(g2)
  # Show z-values
  show(ntz_vec)
  # Show p-values
  show(ntp_vec)
  
  # Save
  ggsave(filename = paste0("g",stations$STAID[si_idx[ss]],"_records.pdf"),
         width = 5,
         height = 3,
         plot = g2,
         device = "pdf",
         path = out_dir)
}
