# Clear workspace
rm(list = ls())

# Set data directory
data_dir <- "Data"
if(!dir.exists(data_dir)) stop("data_dir not found")

# Data directory
out_dir <- "Results/Exploratory/Trends"
if(!dir.exists(out_dir)) dir.create(out_dir, recursive = T)

# Read list of observatories of interest. Based on previous work.
stations <- read.csv(file.path(data_dir,"geo_peninsula_zones.csv"))
idx <- which(!is.na(stations$Zona))
stations <- stations[idx,]

# Read the data for maximum temperatures
tx.mat <- read.csv(file.path(data_dir,"tx_data","Tx_mat.csv"), header = TRUE)
tx.mat$Date <- as.Date(tx.mat$Date)

# Set seasons
summer_idx <- c(152:243)
test_idx <- 52:64
# Reshape data into volume
LL <- 365
TT <- nrow(tx.mat)/LL
SS <- nrow(stations)
tx3d <- array(data = as.matrix(tx.mat[,-1]), dim = c(LL,TT,SS))[summer_idx,,]/10
voldim <- dim(tx3d)

# Get standardized Tx trends in the test period
# Compute yearly trend
trend_ztx <- apply(X = tx3d, MARGIN = 2, function(x) mean(x, na.rm = T))
# Plot inputs
trnd_df <- data.frame(ztx = trend_ztx,
                      t = 1:voldim[2])

# Plot
g3a <- ggplot(data = trnd_df,
              mapping =  aes(x=t)) +
  #geom_hline(yintercept=0, color = "gray") +
  geom_line(aes(y=ztx), color="black") +
  geom_smooth(aes(y=ztx), color="black", se = F, linetype = "dashed") +
  #ylim(c(-1, 1)) +
  ylab(expression(T[x] * phantom(0) * (degree*C))) +
  xlab("t (year)") +
  scale_x_continuous(breaks = c(1, 21, 41, 61),
                     labels = c("1 (1960)", "21 (1980)", "41 (2000)", " 61 (2020)")) +
  theme_bw()
show(g3a)

# Save
ggsave(filename = "trend_Tx.pdf",
       plot = g3a,
       device = "pdf",
       path = out_dir,
       width = 5,
       height = 4)

### Standardized $T_x$
# EDA for the daily maximum temperature ($T_x$).

# Set average and SD for reference period 1981-2010
idx.y <- which(as.numeric(format(tx.mat$Date,"%Y")) > 1981 &
                 as.numeric(format(tx.mat$Date,"%Y")) < 2010)
idx.d <- which(as.numeric(format(tx.mat$Date,"%m")) >=6 &
                 as.numeric(format(tx.mat$Date,"%m")) <= 8)
stations$avg.Tx <- apply(X = tx.mat[intersect(idx.d,idx.y),-1],
                         MARGIN = 2,
                         FUN = function(x) mean(x, na.rm = T))/10
stations$sd.Tx <- apply(X = tx.mat[intersect(idx.d,idx.y),-1],
                        MARGIN = 2,
                        FUN = function(x) sd(x, na.rm = T))/10

# Standardize series
tx.mat[,-1] <- sweep(x = tx.mat[,-1]/10,
                     MARGIN = 2,
                     STATS = stations$avg.Tx,
                     FUN = "-")
tx.mat[,-1] <- sweep(x = tx.mat[,-1],
                     MARGIN = 2,
                     STATS = stations$sd.Tx,
                     FUN = "/")

# Reshape data into volume
tx3d <- array(data = as.matrix(tx.mat[,-1]), dim = c(LL,TT,SS))[summer_idx,,]
voldim <- dim(tx3d)

# Get standardized Tx trends in the test period
# Compute yearly trend
trend_ztx <- apply(X = tx3d, MARGIN = 2, function(x) mean(x, na.rm = T))
# Plot inputs
trnd_df <- data.frame(ztx = trend_ztx,
                      t = 1:voldim[2])

# Plot
g3b <- ggplot(data = trnd_df,
              mapping =  aes(x=t)) +
  geom_hline(yintercept=0, color = "gray") +
  geom_line(aes(y=ztx), color="black") +
  geom_smooth(aes(y=ztx), color="black", se = F, linetype = "dashed") +
  ylim(c(-1, 1)) +
  ylab(expression(T[x](z))) +
  xlab("t (year)") +
  scale_x_continuous(breaks = c(1, 21, 41, 61),
                     labels = c("1 (1960)", "21 (1980)", "41 (2000)", " 61 (2020)")) +
  theme_bw()
show(g3b)

### Standardized Geopotentials
# EDA for the daily maximum temperature ($T_x$) and geopotentials.

# Read grid data
g300 <- read.csv(file.path(data_dir,"grid_data","g300_grid.csv"))
g300$date <- as.Date(g300$date)
g500 <- read.csv(file.path(data_dir,"grid_data","g500_grid.csv"))
g500$date <- as.Date(g500$date)
g700 <- read.csv(file.path(data_dir,"grid_data","g700_grid.csv"))
g700$date <- as.Date(g700$date)

# Reference period
idx.y <- which(as.numeric(format(g300$date,"%Y")) > 1981 &
                 as.numeric(format(g300$date,"%Y")) < 2010)

# G300
g_avg <- apply(X = g300[idx.y,-1],
               MARGIN = 2,
               FUN = function(x) mean(x, na.rm = T))
g_sd <- apply(X = g300[idx.y,-1],
              MARGIN = 2,
              FUN = function(x) sd(x, na.rm = T))
# Standardize series
g300[,-1] <- sweep(x = g300[,-1],
                   MARGIN = 2,
                   STATS = g_avg,
                   FUN = "-")
g300[,-1] <- sweep(x = g300[,-1],
                   MARGIN = 2,
                   STATS = g_sd,
                   FUN = "/")
# Reshape data into volume
g3d <- array(data = as.matrix(g300[,-1]), dim = voldim)
# Compute yearly trend
trnd_df$G300 <- apply(X = g3d, MARGIN = 2, function(x) mean(x, na.rm = T))

# G500
g_avg <- apply(X = g500[idx.y,-1],
               MARGIN = 2,
               FUN = function(x) mean(x, na.rm = T))
g_sd <- apply(X = g500[idx.y,-1],
              MARGIN = 2,
              FUN = function(x) sd(x, na.rm = T))
# Standardize series
g500[,-1] <- sweep(x = g500[,-1],
                   MARGIN = 2,
                   STATS = g_avg,
                   FUN = "-")
g500[,-1] <- sweep(x = g500[,-1],
                   MARGIN = 2,
                   STATS = g_sd,
                   FUN = "/")
# Reshape data into volume
g3d <- array(data = as.matrix(g500[,-1]), dim = voldim)
# Compute yearly trend
trnd_df$G500 <- apply(X = g3d, MARGIN = 2, function(x) mean(x, na.rm = T))

# G700
g_avg <- apply(X = g700[idx.y,-1],
               MARGIN = 2,
               FUN = function(x) mean(x, na.rm = T))
g_sd <- apply(X = g700[idx.y,-1],
              MARGIN = 2,
              FUN = function(x) sd(x, na.rm = T))
# Standardize series
g700[,-1] <- sweep(x = g700[,-1],
                   MARGIN = 2,
                   STATS = g_avg,
                   FUN = "-")
g700[,-1] <- sweep(x = g700[,-1],
                   MARGIN = 2,
                   STATS = g_sd,
                   FUN = "/")
# Reshape data into volume
g3d <- array(data = as.matrix(g700[,-1]), dim = voldim)
# Compute yearly trend
trnd_df$G700 <- apply(X = g3d, MARGIN = 2, function(x) mean(x, na.rm = T))

# Reshape data.frame
# Plot inputs
avg_idx <- grep(pattern = "^t", names(trnd_df), invert = T)
trend_df <- data.frame(z = c(as.matrix(trnd_df[,avg_idx])),
                       Signal = rep(c("Tx","G300","G500","G700"), each = nrow(trnd_df)),
                       t = rep(1:nrow(trnd_df), length(avg_idx)))
# Figure
colorcito <- c("blue","green","red","black")
g3d <- ggplot(data = trend_df,
              mapping =  aes(x=t, y=z, color = Signal)) +
  geom_hline(yintercept=0,
             color = "gray") +
  geom_line() +
  theme_bw() +
  scale_color_manual(values = colorcito) +
  ylab("z") +
  xlab("t (year)") +
  scale_x_continuous(breaks = c(1, 21, 41, 61),
                     labels = c("1 (1960)", "21 (1980)", "41 (2000)", " 61 (2020)"))
show(g3d)

# Save
ggsave(filename = "trend_z_Ix_G.pdf",
       plot = g3d,
       device = "pdf",
       path = out_dir,
       width = 5,
       height = 4)

# Figure
colorcito <- c("blue","green","red","black")
g3e <- ggplot(data = trend_df,
              mapping =  aes(x=t, y=z, color = Signal)) +
  geom_hline(yintercept=0,
             color = "gray") +
  geom_smooth(method = "loess",
              formula = "y ~ x",
              se=F) +
  theme_bw() +
  scale_color_manual(values = colorcito) +
  ylab("z") +
  xlab("t (year)") +
  scale_x_continuous(breaks = c(1, 21, 41, 61),
                     labels = c("1 (1960)", "21 (1980)", "41 (2000)", " 61 (2020)"))
show(g3e)

# Save
ggsave(filename = "trend_pt_Ix_G_loess.pdf",
       plot = g3e,
       device = "pdf",
       path = out_dir,
       width = 5,
       height = 4)
