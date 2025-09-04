################################################################################
# Clear workspace
rm(list = ls())

# Set data directory
data_dir <- file.path(paste0(getwd(),"/Data"))
if(!dir.exists(data_dir)) stop("data_dir not found")

# Load data.frame
global.df <- readRDS(file.path(data_dir,"global_data","global_df.rds"))

# Load feature selected variables based on z-values
outdir <- file.path(paste0(getwd(),"/Results/final_models"))
if(!dir.exists(outdir)) stop("outdir not found")
fin.var <- readRDS(file.path(outdir,"final_variables.rds"))

# Detect lag1 variables
lag1idx <- grep(".lag1", fin.var)
if(length(lag1idx)>0){
  
  # Load libraries
  if(!is.element("dplyr", row.names(installed.packages()))) install.packages("dplyr")
  library(dplyr)
  
  # Calculate lag1 variables
  for(ii in 1:length(lag1idx)){
    # Find variable
    varname <- unlist(strsplit(x = fin.var[lag1idx[ii]], split = ".lag1"))
    # Create lag1
    if(!is.element(varname, names(global.df))) stop("Lag1 variable not found in global data.frame")
    global.df[[fin.var[lag1idx[ii]]]] <- dplyr::lag(global.df[[varname]], 1, 0)
    
  }# for ii lag1 found
}# if any lag1 found

# Detect poly-2 variables
poly2idx <- grep("poly", fin.var)
fin.single <- fin.var
if(length(poly2idx)>0){
  for(ii in 1:length(poly2idx)){
    fin.single[poly2idx[ii]] <- gsub(", 2[)]","",gsub("poly[(]","",fin.single[poly2idx[ii]]))
  } 
}

# Check if all final local variables are included in the global data.frame
if(!all(fin.single %in% names(global.df))) stop("Variable name not found in global data.frame")

################################################################################
# Set training set
idx.train <- which(global.df$t <= 51)

# Read m1 formula
m1.frm <- readRDS("~/ERA5_Record_Algorithm/Results/final_models/m1.rds")

# Compute m1 model
m1 <- glm(formula = m1.frm,
          data = global.df[idx.train,],
          family = binomial(link = "logit"))


# Get z values
z.vals <- summary(m1)$coefficients[, "z value"]


