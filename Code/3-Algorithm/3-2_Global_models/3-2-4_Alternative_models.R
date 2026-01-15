# Clear workspace
rm(list = ls())

# Load libraries
if(!is.element("dplyr", row.names(installed.packages()))) install.packages("dplyr")
library(dplyr)
if(!is.element("MASS", row.names(installed.packages()))) install.packages("MASS")
library(MASS)

# Set data directory
data_dir <- "Data"
if(!dir.exists(data_dir)) stop("data_dir not found")

# Load data.frame
global.df <- readRDS(file.path(data_dir,"global_data","global_df.rds"))

# Load feature selected variables based on z-values
out_dir <- "Results/final_models/Alternative"
if(!dir.exists(out_dir)) dir.create(out_dir)

# Find geopotential variables
gvars <- grep("^g[0-9]00", names(global.df))
# Add lag1 variables
for(ii in 1:length(gvars)){
  # Find variable
  varname <- paste0(names(global.df)[gvars[ii]],".lag1")
  # Create lag1 variable
  global.df[[varname]] <- dplyr::lag(global.df[[gvars[ii]]], 1, 0)
}# for ii in g-vars

# Get variable names
eranames <- names(global.df)[gvars]
# Get lagged variables
lagnames <- names(global.df)[grep(".lag1", names(global.df))]
# Create poly2 terms
polynames <- sapply(eranames, function(name) paste0("poly(", name,",2)"))

# Geopotential model formula
frm_alt <- as.formula(paste0("Ix~",
                             capture.output(cat(c(eranames,lagnames,polynames),
                                                sep = "+"))))
# Fit model
idx.train <- which(global.df$t <= 51)

###############################################################################
# Alternative M1 model without Step 1
###############################################################################

# Check if models exists already
m_alt_frm <- file.path(out_dir,"m1_noStep1.rds")
if(!file.exists(m_alt_frm)){
  
  fit_alt <- glm(formula = frm_alt,
                 data = global.df[idx.train,],
                 family = binomial(link = "logit"))
  
  # Apply a stringent stepAIC
  m_null <- update(fit_alt, .~1)
  tic <- Sys.time()
  sink(file.path(out_dir,"stepAIC_output.txt"))
  m_alt <- stepAIC(object = m_null,
                   direction = "both",
                   scope = list(lower = m_null, upper = fit_alt),
                   k = 10.82757)
  sink()
  toc <- Sys.time()
  cat("M-Alt: ")
  print(toc-tic) # Time difference of 38 mins
  
  saveRDS(object = m_alt$formula, file = m_alt_frm)
}# if m_alt_frm not found
# Compute the model
# Formula obtained by checking the step of the stepAIC where the number of parameters is the same as in M1 model
m_alt_frm_aux <- as.formula('Ix ~ poly(g700., 2) + g700.45N.10W + poly(g300.35N.5E, 2) + 
poly(g500.35N.5E,2) + g700..lag1 + g300.35N.5E.lag1 + poly(g500.45N.10W, 2) + poly(g300., 2) + 
g300.35N.10W.lag1 + g500.35N.10W.lag1 + poly(g700.35N.5E, 2)')

m_alt <- glm(formula = m_alt_frm_aux, #readRDS(m_alt_frm)
             data = global.df[idx.train,],
             family = binomial(link = "logit"))

###############################################################################
# Alternative M1 model via Lasso (without Step 1)
###############################################################################

# Load libraries
if(!is.element("glmnet", row.names(installed.packages()))) install.packages("glmnet")
library(glmnet)

# Input parameters
X <- model.matrix(frm_alt , data=global.df[idx.train,])[,-1]
y <- global.df$Ix[idx.train]
lambda_seq <- 10^seq(2, -2, by = -.1)

# Tune model via 5-fold cross-validation
tic <- Sys.time()
cv_tune <- cv.glmnet(X,
                     y,
                     alpha = 1,
                     lambda = lambda_seq, 
                     nfolds = 5,
                     family = binomial(link = "logit"))
best_lam <- cv_tune$lambda.min
m_lasso <- glmnet(X,
                  y,
                  alpha = 1,
                  lambda = best_lam,
                  family = binomial(link = "logit"))
toc <- Sys.time()
print(toc-tic) # Time difference of 1.007692 mins

# Coefficients
lasso_coef <- coef(m_lasso, s = best_lam)
sum(coef(m_lasso, s = best_lam)!=0)
# Extract variable names that survive CV
lasso_vars <- rownames(lasso_coef)[which(lasso_coef != 0)[-1]]
poly_idx <- grep("poly", lasso_vars) 
if(length(poly_idx)>0){
  for(ii in 1:length(poly_idx)) lasso_vars[poly_idx[ii]] <- substr(lasso_vars[poly_idx[ii]], 1, nchar(lasso_vars[poly_idx[ii]])-1)
  lasso_vars <- unique(lasso_vars)
} 

# Compute the model
lasso_frm <- as.formula(paste("Ix~", paste(lasso_vars, collapse=" + ")))
m_lasso <- glm(formula = lasso_frm,
               data = global.df[idx.train,],
               family = binomial(link = "logit"))


###############################################################################
# Models performance
###############################################################################

# Load libraries
if(!is.element("pROC", row.names(installed.packages()))) install.packages("pROC")
library(pROC)

# M1: Simple global model
# Read the formula
m1.frm <- readRDS("Results/final_models/m1.rds")
# Compute the model
m1 <- glm(formula = m1.frm,
          data = global.df[idx.train,],
          family = binomial(link = "logit"))
# Models
m_ls <- c("m1","m_alt","m_lasso")

# Read list of observatories of interest. Based on previous work.
stations <- read.csv("Data/geo_peninsula_zones.csv")
idx <- which(!is.na(stations$Zona))
stations <- stations[idx,]
# Abbreviations
stations$STANAME[10] <- "FABRA OBSERVATORY                       "
stations$STANAME[25] <- "BCN/AEROPUERTO                          "
stations$abb <- substr(stations$STANAME,1,6)

# Global data.frame to store model performance
all_df <- data.frame(Model = m_ls,
                     k = vector("numeric", length(m_ls)),
                     n_var = vector("numeric", length(m_ls)), 
                     AUC = vector("numeric", length(m_ls)),
                     AUC.coast = vector("numeric", length(m_ls)),
                     AUC.inner = vector("numeric", length(m_ls)),
                     AIC = vector("numeric", length(m_ls)))

# Create list to store staid_df for each model
staid_list <- vector("list", length(m_ls))

# For each model check performance
for(mm in 1:length(m_ls)){
  
  print(paste0("Model: ",m_ls[mm]))
  
  # Create data.frame to store results
  staid_df <- data.frame(STAID = stations$STAID,
                         Name = stations$abb,
                         CoastDist = round(stations$CoastDist/1000,1),
                         Model = rep(m_ls[mm], nrow(stations)),
                         k = vector("numeric", nrow(stations)),
                         AUC = vector("numeric", nrow(stations)))
  
  # Get number of parameters
  all_df$k[mm] <- staid_df$k <- length(coef(get(m_ls[mm])))
  
  # Get number of variables
  all_df$n_var[mm] <- length(attr(terms(get(m_ls[mm])), "variables"))-1
  
  # Get global AUC
  predict.aux <- predict(object = get(m_ls[mm]),
                         newdata = global.df[-idx.train,],
                         type = "response")
  roc.aux <- roc(global.df$Ix[-idx.train], predict.aux, print.auc = TRUE)
  all_df$AUC[mm] <- auc(roc.aux)
  
  # Get AUC from stations near the coast
  idx.test.coast <- which(global.df$CoastDist[-idx.train]/1000 < 50)
  roc.coast <- roc(response = (global.df$Ix[-idx.train])[idx.test.coast],
                   predictor = predict.aux[idx.test.coast],
                   print.auc = TRUE)
  all_df$AUC.coast[mm] <- auc(roc.coast)
  
  # Get AUC from inner stations
  roc.inner <- roc(response = (global.df$Ix[-idx.train])[-idx.test.coast],
                   predictor = predict.aux[-idx.test.coast],
                   print.auc = TRUE)
  all_df$AUC.inner[mm] <- auc(roc.inner)
  
  # Get AIC value
  all_df$AIC[mm] <- AIC(get(m_ls[mm]))
  
  # Get AUC values at each station
  for(ss in 1:nrow(staid_df)){
    
    # Get indices
    idx.test.staid <- which(global.df$STAID[-idx.train] == staid_df$STAID[ss])
    
    # Calculate AUC
    roc.staid <- roc(response = (global.df$Ix[-idx.train])[idx.test.staid],
                     predictor = predict.aux[idx.test.staid],
                     print.auc = TRUE)
    staid_df$AUC[ss] <- auc(roc.staid)
    
  }# for ss stations
  
  # Store this staid_df in the list
  staid_list[[mm]] <- staid_df
  
  
}# for mm model

# Combine all into one big data.frame
staid_all <- do.call(rbind, staid_list)

################################################################################
# Create plot

# Gather all local and global AUCs
# (except for m5 that is repetitive)
# Local
outdir <- file.path(getwd(),"Results/final_models")
stations$m0 <- local.auc <- read.csv(file = file.path(dirname(outdir), "local_AUC.csv"))$AUC

# Convert data frame to long format
library(tidyr)
library(dplyr)
staid_wide <- staid_all |>
  dplyr::select(STAID, Name, Model, AUC, CoastDist) |>
  tidyr::pivot_wider(
    names_from = Model,
    values_from = AUC,
    values_fill = NA
  )

# Load libraries
if(!is.element("ggplot2", row.names(installed.packages()))) install.packages("ggplot2")
library(ggplot2)
if(!is.element("gridExtra", row.names(installed.packages()))) install.packages("gridExtra")
library(gridExtra)
if(!is.element("RColorBrewer", row.names(installed.packages()))) install.packages("RColorBrewer")
library(RColorBrewer)

# Plot
colorcito <- brewer.pal(n = 8, name = "Set1")

# Order from closest to the coast to furthest
#stations <- stations[order(stations$CoastDist),]
staid_wide <- staid_wide[order(stations$m0),]

# Create plot with M0, M1 and M2 models
g1 <- ggplot(data = staid_wide, aes(x = 1:nrow(stations))) +
  geom_point(aes(y = m1, color = "M1"), na.rm = TRUE, shape = 16, size = 3) +
  geom_point(aes(y = m_alt, color = "Step"), na.rm = TRUE, shape = 16, size = 3) +
  #geom_point(aes(y = m_lasso, color = "Lasso"), na.rm = TRUE, shape = 16, size = 3) +
  #geom_point(aes(y = m3, color = "M3"), na.rm = TRUE, shape = 18, size = 3) +
  labs(title = "Comparison of AUCs of M1 versions",
       y = "AUC",
       x = "") +
  scale_color_manual(name = "",
                     breaks = c('M1',"Step"), #,"Lasso"
                     values = c("M1" = colorcito[2],
                                "Step" = colorcito[5]))+
                                #"Lasso" = colorcito[7])) +
  scale_x_discrete(labels = stations$abb, limits = factor(1:nrow(stations))) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 55, hjust = 1,  face = "bold")) 

#geom_vline(xintercept = 16.5, linetype = "dashed", color = "black", linewidth = 0.5)

# Show plot
show(g1)

# Save plot
auc_dir <- file.path(outdir,"AUC")
outname <- paste0("plot_auc_m1StepLasso_ordered.pdf")
if(!file.exists(file.path(auc_dir,outname))) {
  ggsave(filename = file.path(auc_dir,outname),
         plot = g1,
         device = "pdf",
         width = 8,
         height = 4)
}

#################################################################################
# Read AUC data frame with all models
all_df$min.coast <- NA
all_df$mean.coast <- NA
#all_df$Q10.coast <- NA
all_df$Q25.coast <- NA
all_df$median.coast <- NA
#all_df$mean.diff.coast <- NA
all_df$median.diff.coast <- NA
all_df$Q10.diff.coast <- NA

# Read AUC per model for each stations
for (i in seq_along(m_ls)){
  
  # Select model
  mm <- m_ls[i]
  
  df <- staid_wide
  coastal_df <- staid_wide[staid_wide$CoastDist <= 50, ]
  
  local.auc.coast <- stations$m0[stations$CoastDist <= 50000]
  diff <- coastal_df[[mm]] - local.auc.coast
  
  # Add measures to all_df dataframe
  all_df$min.coast[i] <- min(coastal_df[[mm]])
  all_df$median.coast[i] <- median(coastal_df[[mm]])
  #all_df$Q10.coast[i] <- quantile(coastal_df[[mm]], 0.10)
  all_df$Q25.coast[i] <- quantile(coastal_df[[mm]], 0.25)
  all_df$mean.coast[i] <- mean(coastal_df[[mm]])
  
  #all_df$mean.diff.coast[i] <- abs(mean(diff))
  all_df$median.diff.coast[i] <- abs(median(diff))
  all_df$Q10.diff.coast[i] <- abs(quantile(diff, 0.10))
}

auc.global.aux <- all_df[,c('Model','k','n_var','AIC','AUC','AUC.inner','AUC.coast','min.coast','Q25.coast','median.coast','mean.coast','Q10.diff.coast','median.diff.coast')]
