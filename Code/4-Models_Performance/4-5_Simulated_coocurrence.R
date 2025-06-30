# Clear workspace
rm(list = ls())

# Load libraries
if(!is.element("geodist", row.names(installed.packages()))) install.packages("geodist")
library(geodist)
if(!is.element("ggplot2", row.names(installed.packages()))) install.packages("ggplot2")
library(ggplot2)

# Set data directory
data_dir <- "Data"
if(!dir.exists(data_dir)) stop("data_dir not found")

# Read list of observatories of interest. Based on previous work.
stations <- read.csv(file.path(data_dir,"geo_peninsula_zones.csv"))
idx <- which(!is.na(stations$Zona))
stations <- stations[idx,]

# Read record data
itx3d <- readRDS(file.path(data_dir, "record_data", "recordvol.rds"))
LL <- dim(itx3d)[2]
TT <- dim(itx3d)[1]
SS <- dim(itx3d)[3]
if(SS != nrow(stations)) stop("Number of stations differ!")

# Compute geodesic distances
geomat <- geodist(x = cbind(stations$LON,stations$LAT),
                    measure = "geodesic")/1000

# Define a function to calculate Jaccard Index
jidx <- function(vec1, vec2) {
  inter <- sum(vec1 & vec2)
  onion <- sum(vec1 | vec2)
  return(inter / onion)
}

# Set validation test 
test_idx <- c(52:64)
test_len <- length(test_idx)

# Save observed pairwise-JI across stations
uptri <- which(upper.tri(x = geomat, diag = F),arr.ind = T)
ji_obs_df <- data.frame(dist = geomat[uptri], JI = 0)
for(ii in 1:nrow(ji_obs_df)) ji_obs_df$JI[ii] <- jidx(c(itx3d[test_idx,,uptri[ii,1]]),c(itx3d[test_idx,,uptri[ii,2]]))

# Plot it
g1 <- ggplot(data = ji_obs_df,
             mapping =  aes(x=dist, y=JI)) +
  geom_point(alpha=0.5,shape = 16) +
  geom_smooth(se = T) +
  theme_bw() +
  labs(title = "Observed pairwise Jaccard Index",
       subtitle = "2011-2023") +
  ylab('Jaccard\'s index') +
  xlab("Geodesic distance (km)")
show(g1)

# Calculate random JI under iid-stationarity
nperm <- 1000
ji_rdm_mat <- matrix(0, nrow = nrow(ji_obs_df), ncol = nperm)

for(pp in 1:nperm){
  if(pp %% 100 == 0) cat(paste0("..",pp))
  binmat <- array(data = as.numeric(NA),
                  dim = c(test_len,LL,SS))
  for(yy in 1:test_len) binmat[yy,,] <- rbinom(LL*SS,1,1/test_idx[yy])
  for(ii in 1:nrow(ji_obs_df)) ji_rdm_mat[ii,pp] <- jidx(c(binmat[,,uptri[ii,1]]),c(binmat[,,uptri[ii,2]]))
}# pp permutation

# Add average to plotting data.frame
ji_obs_df$JIsim <- apply(ji_rdm_mat,1,mean)
# Plot it
g2 <- ggplot(data = ji_obs_df,
             mapping =  aes(x=dist)) +
  geom_smooth(mapping = aes(y=JI), se = T) +
  geom_smooth(mapping = aes(y=JIsim), col = "red", se = T) +
  theme_bw() +
  labs(title = "Observed pairwise Jaccard Index",
       subtitle = "2011-2023") +
  ylab('Jaccard\'s index') +
  xlab("Geodesic distance (km)")
show(g2)
