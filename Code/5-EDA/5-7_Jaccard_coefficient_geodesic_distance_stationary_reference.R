# Clear workspace
rm(list = ls())

# Load libraries
if(!require(devtools)) install.packages("devtools"); library(devtools)
if(!require(geodist)) install.packages("geodist"); library(geodist)
if(!require(GGally)) install.packages("GGally"); library(GGally)
if(!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if(!require(RecordTest)) install_local(file.path("Packages","RecordTest_2.2.0.tar.gz")); library(RecordTest)

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
year_idx <- c(25:64)

# Compute upper records (indicators) for summer data only
upp.rcrd <- function(x) c(1,as.numeric(diff(cummax(x))>0))
itx3d <- apply(X = tx3d[summer_idx,,], MARGIN = c(1,3), upp.rcrd)

# Do not consider (for now) more than one station from Madrid
stations$STANAME[which(is.na(stations$Zona))]
idx <- which(!is.na(stations$Zona))
stations <- stations[idx,]
itx3d <- itx3d[year_idx,,idx]
voldim <- dim(itx3d)

# Modify Barcelona-Airport label
stations$STANAME[10] <- "FABRA OBSERVATORY                       "
stations$STANAME[25] <- "BCN/AEROPUERTO                          "


extract_first_word <- function(name) {
  pos <- regexpr("[ /-]", name)
  if (pos[1] > 0) {
    return(substr(name, 1, pos[1] - 1))
  } else {
    return(name)
  }
}

stations$abb <- sapply(stations$STANAME, extract_first_word)
stations$abb[which(stations$abb == "A")] <- "A CORUÑA"
stations$abb[which(stations$abb == "CIUDAD")] <- "CIUDAD REAL"
stations$abb[which(stations$abb == "SAN")] <- "SAN SEBASTIAN"
stations$abb[which(stations$abb == "BCN")] <- "BARCELONA"

#stations$abb <- paste0(toupper(substring(stations$abb, 1, 1)), tolower(substring(stations$abb, 2)))

#stations$abb[which(stations$abb == "A coruña")] <- "A Coruña"
#stations$abb[which(stations$abb == "Ciudad real")] <- "Ciudad Real"
#stations$abb[which(stations$abb == "San sebastian")] <- "San Sebastian"
#################################################################################

# Compute geodetic distances
dist_mat <- geodist(x = cbind(stations$LON,stations$LAT),
                    measure = "geodesic")
show(max(dist_mat))
stations$STANAME[which(dist_mat == max(dist_mat), arr.ind = T)[1,]]

################################################################################
outdir <- 'Results/Exploratory/Co_ocurrence'
# Correlation by sites
library(GGally)

test_idx <- c(25:64)

# Permute the dimensions to bring the third dimension to the front
itx3d_permuted <- aperm(itx3d, c(2,1,3))
itx2d <- matrix(itx3d_permuted, nrow = length(test_idx)*voldim[2], ncol = voldim[3])

# Define a function to calculate Jaccard Index
jaccard_index <- function(vec1, vec2) {
  intersection <- sum(vec1 & vec2)
  union <- sum(vec1 | vec2)
  return(intersection / union)
}

# Initialize Jaccard Index Matrix
jaccard.mat.itx <- matrix(0, nrow = voldim[3], ncol = voldim[3])

# Calculate Jaccard Index for each pair of stations
for (i in 1:voldim[3]) {
  for (j in 1:voldim[3]) {
    jaccard.mat.itx[i, j] <- jaccard_index(itx2d[, i], itx2d[, j])
  }
}

# Calculate correlations between record series
colnames(jaccard.mat.itx) <- stations$abb
rownames(jaccard.mat.itx) <- stations$abb

##########################################################################################
# Create simulations for stationary case 

sim<-NULL
for(j in 1:1000){
  serie1<-NULL
  serie2<-NULL
  for (i in 25:64)
  {
    serie1<-c(serie1,rbinom(92,size=1, p=1/i))
    serie2<-c(serie2,rbinom(92,size=1, p=1/i))
  }
  
  sim[j]<-jaccard_index(serie1, serie2)
}

sim.aux <- mean(sim)
band.sim <- quantile(sim, p=c(0.025, 0.975))
##############################################################################################

# Extract upper triangle
utri <- upper.tri(jaccard.mat.itx)
plot_df <- data.frame(dist=dist_mat[utri]/1000, cor=jaccard.mat.itx[utri])
# Plot it
g1 <- ggplot(data = plot_df,
             mapping =  aes(x=dist, y=cor)) +
  geom_point(alpha=0.5,shape = 16) +
  geom_smooth(se = FALSE) +
  theme_bw() +
  labs(title = "1984-2023") +
  ylab('Jaccard index') +
  xlab("Geodetic distance (km)") +
  ylim(c(0,1))+
  geom_segment(aes(x = min(dist), xend = max(dist),
                   y = sim.aux, yend = sim.aux),
               color = "red", linewidth = 0.5) +
  #geom_hline(yintercept = sim.aux, color = "red", linewidth = 1) +
  # Shaded band for the 95% interval
  annotate("rect",
           xmin = min(plot_df$dist), xmax = max(plot_df$dist),
           ymin = band.sim[1], ymax = band.sim[2],
           alpha = 0.2, fill = "grey1")+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),          # Increase axis values size
    legend.text = element_text(size = 15)        # Increase legend text size
  )
show(g1)

ggsave(filename = "jaccard-deodist-stationary-ref.pdf",
       width = 5, height = 3.5,
       plot = g1, device = "pdf", path = outdir)
