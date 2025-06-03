# Clear workspace
rm(list = ls())

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


## Save record streaks
# Set functions directory
fdir <- "R"
if(!dir.exists(fdir)) stop("Functions directory not found")
# Load auxiliary function
source(file.path(fdir,"count_ones_runs_with_days.R"))

# Apply the function for each year and station
runs_ls <- array(list(), dim = c(TT, SS))
for(tt in 1:TT){
  for(ss in 1:SS) {
    series <- itx3d[tt, ,ss]
    runs_ls[[tt,ss]] <- count_ones_runs_with_days(series)
  }# for ss station
}# for tt year
colnames(runs_ls) <- stations$STAID

# View an example of the results for the last year and the first station
print(runs_ls[[64,1]])

# Check for output directory
# Load feature selected variables based on z-values
outdir <- "Results/streaks"
if(!dir.exists(outdir)) dir.create(outdir)

# Save results
outname <- paste0("streaks_counts.rds")
if(!file.exists(file.path(outdir,outname))){
  saveRDS(object = runs_ls,
          file = file.path(outdir,outname))
}

## True streaks plot with confidence intervals
# Get prediction directory
pred_dir <- "Results/final_models/Predicts/"
if(!dir.exists(pred_dir)) stop("Predicts directory not found")

# Read models predict for model M2
pred_df <- read.csv(file.path(pred_dir,"preds_m2.csv"))
test_tt <- min(pred_df$t):max(pred_df$t)

# Create empty matrix to fill with the true probabilities
obs_probs <- matrix(0, nrow = SS, ncol = 5)

# Create a loop to obtain the probabilities of different streaks lengths
# per station for all the test period
for(ss in 1:SS){
  
  cat(paste0("...",ss))
  
  # Initiate object
  aux.streaks <- vector("numeric")
  for(tt in test_tt){
    aux.streaks <- c(aux.streaks, runs_ls[[tt,ss]]$lengths)
  }
  
  # Frequency table of observed probabilities
  table.prob <- table(aux.streaks)/sum(table(aux.streaks))
  
  # Store in matrix
  idx.num <- which(names(table.prob) == 1)
  if (length(idx.num) > 0) obs_probs[ss,1] <- table.prob[idx.num]
  
  idx.num <- which(names(table.prob) == 2)
  if (length(idx.num) > 0) obs_probs[ss,2] <- table.prob[idx.num]
  
  idx.num <- which(names(table.prob) == 3)
  if (length(idx.num) > 0) obs_probs[ss,3] <- table.prob[idx.num]
  
  idx.num <- which(names(table.prob) == 4)
  if (length(idx.num) > 0) obs_probs[ss,4] <- table.prob[idx.num]
  
  idx.num <- which(names(table.prob) >= 5)
  if (length(idx.num) > 0) obs_probs[ss,5] <- sum(table.prob[idx.num])
  
}# for ss stations


## Simulation of streaks using the M2 model's predict

# Now make simulated probabilities using a certain model
outname <- paste0("sim_streakprobs_m2.rds")

# If not found created, else read it
if(!file.exists(file.path(outdir,outname))){
  
  # Set seed
  set.seed(18950521)
  
  # Define input variables
  n_sim <- 10000
  
  # Create empty volume to fill with the simulated probabilities
  sim_probs <- array(data = 0,
                     dim = c(SS, ncol(obs_probs), 1+n_sim))
  sim_probs[,,1] <- obs_probs
  
  # For each simulation
  for(nn in 1:n_sim){
    
    if(nn%%100 == 0) cat(paste0("..",nn))
    
    # Apply the function for each year and station
    runs_ls <- array(list(), dim = c(TT, SS))
    for(tt in test_tt){
      for(ss in 1:SS) {
        # Get corresponding model predictions
        pred.idx <- which(pred_df$t == tt & pred_df$STAID == stations$STAID[ss])
        series <- as.numeric(pred_df$preds[pred.idx] > runif(length(pred.idx)))
        runs_ls[[tt,ss]] <- count_ones_runs_with_days(series)
      }# for ss station
    }# for tt year
    
    # Obtain simulated streak probabilities by station
    for(ss in 1:SS){
      # Initiate object
      aux.streaks <- vector("numeric")
      for(tt in test_tt){
        aux.streaks <- c(aux.streaks, runs_ls[[tt,ss]]$lengths)
      }
      # Frequency table of observed probabilities
      table.prob <- table(aux.streaks)/sum(table(aux.streaks))
      
      # Store in matrix
      idx.num <- which(names(table.prob) == 1)
      if(length(idx.num) > 0) sim_probs[ss,1,nn+1] <- table.prob[idx.num]
      idx.num <- which(names(table.prob) == 2)
      if(length(idx.num) > 0) sim_probs[ss,2,nn+1] <- table.prob[idx.num]
      idx.num <- which(names(table.prob) == 3)
      if(length(idx.num) > 0) sim_probs[ss,3,nn+1] <- table.prob[idx.num]
      idx.num <- which(names(table.prob) == 4)
      if(length(idx.num) > 0) sim_probs[ss,4,nn+1] <- table.prob[idx.num]
      idx.num <- which(names(table.prob) >= 5)
      if(length(idx.num) > 0) sim_probs[ss,5,nn+1] <- sum(table.prob[idx.num])
      
    }# for ss stations
  }# for nn simulation
  
  # Save results
  saveRDS(object = sim_probs,
          file = file.path(outdir,outname))
} else{
  
  sim_probs <- readRDS(file.path(outdir,outname))
  
}# if-else file exists


## Display results

# Create a data frame and add the errors for the streaks probabilities
mat.true.errors <- matrix(ncol = 2, nrow = 5)
colnames(mat.true.errors) <- c("t.min","t.max")
rownames(mat.true.errors) <- c("1","2","3","4","5")

for(i in 1:nrow(mat.true.errors)){
  mat.true.errors[i,] <- quantile(sim_probs[,i,1], probs = c(0.025,0.975))
} 

# Now add the true probabilities to the data frame
true.df <- as.data.frame(mat.true.errors)
true.df$true.probs <- apply(obs_probs, 2, mean)
true.df$value <- 1:5

# Now create a plot to show the true probabilities with their error bars
library(ggplot2)
g.plot.true <- ggplot(true.df, aes(x = value, y = true.probs)) +
  geom_point(aes(color = "True"), shape = 4, size = 3, stroke = 1) +
  geom_errorbar(aes(ymin = t.min, ymax = t.max, color = "True"), width = 0) +
  ggtitle("All stations") +
  xlab("Duration") +
  ylab("Probability density") +
  scale_color_manual(name = "",values = c("True" = "black")) +
  theme_minimal() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"), 
    plot.title = element_text(hjust = 0.5)
  )

show(g.plot.true)

# Calculate the confidence intervals for the simulated probabilities
sim.errors <- apply(sim_probs[,,-1], c(2,3), quantile,probs = c(0.025,0.975))
sim.errors.avg <- apply(sim.errors, c(1,2), mean)

# Calculate the values for the simulated probabilities
sim.probs.avg <- apply(sim_probs[,,-1], 2, mean)

# Create a data frame and add the errors for the simulated streaks probabilities
sim.errors.df <- data.frame(s.min = sim.errors.avg[1,],
                            s.max = sim.errors.avg[2,],
                            sim.prob = sim.probs.avg,
                            value = 1:5)

# Now create a plot to show the simulated probabilities with their error bars
g.plot.sim <- ggplot(sim.errors.df, aes(x = value, y = sim.prob)) +
  geom_point(aes(color = "Simulated") , shape = 21,fill = "blue", color = "blue",size = 2) +
  geom_errorbar(aes(ymin = s.min, ymax = s.max, color = "Simulated"), width = 0) +
  ggtitle("All stations") +
  xlab("Duration") +
  ylab("Probability density") +
  scale_color_manual(name = "",values = c("Simulated" = "blue")) +
  theme_minimal() +
  theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"), 
    plot.title = element_text(hjust = 0.5)
  )
show(g.plot.sim)

## Create plot with real streaks and simulated ones
# Now combine both plots into one
# Define a small displacement value
displacement <- 0.15

combined_plot <- ggplot() +
  # Simulated data (with displacement)
  geom_point(data = sim.errors.df, aes(x = value + displacement, y = sim.prob, color = "Simulated"), shape = 21, fill = "blue", size = 2) +
  geom_errorbar(data = sim.errors.df, aes(x = value + displacement, ymin = s.min, ymax = s.max, color = "Simulated"), width = 0) +
  # True data (no displacement)
  geom_point(data = true.df, aes(x = value, y = true.probs, color = "True"), shape = 4, size = 3, stroke = 1) +
  geom_errorbar(data = true.df, aes(x = value, ymin = t.min, ymax = t.max, color = "True"), width = 0) +
  # Titles and labels
  ggtitle("Streaks probabilities M2 model") +
  xlab("Duration") +
  ylab("Probability density") +
  # Custom colors for legend
  scale_color_manual(name = "", values = c("Simulated" = "blue", "True" = "black")) +
  # Themes and legend positioning
  theme_minimal() +
  theme(
    # Adjust legend size and position
    legend.position = c(0.60, 0.85),
    legend.text = element_text(size = 15),
    # Adjust axis text size
    axis.text = element_text(size = 15),
    # Adjust axis title size
    axis.title = element_text(size = 15),
    # Adjust plot title size
    plot.title = element_text(size = 20, face = "bold", hjust = 0.5)
  )

# Print the combined plot
show(combined_plot)
## Save final plot
# Save M2 model's predict
outname <- "streaks_sim_m2.pdf"
if(!file.exists(file.path(outdir,outname))){
  # Save results
  ggsave(filename = file.path(outdir,outname),
         plot = combined_plot,
         device = "pdf",
         width = 6,
         height = 4)
}

# Get percentile for each observed value within the simulated values
obs_probs[1,1]
