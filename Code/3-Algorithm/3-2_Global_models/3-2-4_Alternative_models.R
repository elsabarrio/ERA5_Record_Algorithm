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
  m_alt <- stepAIC(object = m_null,
                   direction = "both",
                   scope = list(lower = m_null, upper = fit_alt),
                   k = 10.82757)
  toc <- Sys.time()
  cat("M-Alt: ")
  print(toc-tic) # Time difference of 38 mins
  
  saveRDS(object = m_alt$formula, file = m_alt_frm)
}# if m_alt_frm not found
# Compute the model
m_alt <- glm(formula = readRDS(m_alt_frm),
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
                     AUC = vector("numeric", length(m_ls)),
                     AUC.coast = vector("numeric", length(m_ls)),
                     AUC.inner = vector("numeric", length(m_ls)),
                     AIC = vector("numeric", length(m_ls)))

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
  
}# for mm model
