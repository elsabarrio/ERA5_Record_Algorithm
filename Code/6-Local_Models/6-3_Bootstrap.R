# Clear workspace
rm(list = ls())

# Set data directory
data_dir <- "Data"
if(!dir.exists(data_dir)) stop("data_dir not found")

# Read list of observatories of interest. Based on previous work.
stations <- read.csv(file.path(data_dir,"geo_peninsula_zones.csv"))
idx <- which(!is.na(stations$Zona))
stations <- stations[idx,]
stations$point <- sapply(1:nrow(stations),
                         function(x){
                           aux <- as.character(round(stations$LON[x]))
                           if(grepl("^-",aux)){
                             aux <- paste0(substr(aux, 2, nchar(aux)),"W")
                           } else aux <- paste0(aux,"E")
                         })
stations$point <- paste0(round(stations$LAT),"N.",
                         stations$point)

# Corner points
cpoints <- c("45N.10W","45N.5E","35N.10W","35N.5E")

# Read record indicators matrix
itx.mat <- read.csv(file.path(data_dir,"record_data","record_matrix.csv"))
itx.mat$Date <- as.Date(itx.mat$Date)

# Read grid data
gall <- cbind(read.csv(file.path(data_dir,"grid_data","g300_grid.csv")),
              read.csv(file.path(data_dir,"grid_data","g500_grid.csv")),
              read.csv(file.path(data_dir,"grid_data","g700_grid.csv")))

# Define output directory to save the models
outdir <- "Results/local_zvalues/Bootstrap"

# Start loop to construct the local models
if(dir.exists(outdir)){
  
  # Load libraries
  if(!is.element("dplyr", row.names(installed.packages()))) install.packages("dplyr")
  library(dplyr)
  if(!is.element("MASS", row.names(installed.packages()))) install.packages("MASS")
  library(MASS)
  if(!is.element("pROC", row.names(installed.packages()))) install.packages("pROC")
  library(pROC)
  
  # Create output directory to save the models
  #dir.create(outdir, recursive = T)
  
  # Abbreviations
  stations$STANAME[10] <- "FABRA OBSERVATORY                       "
  stations$STANAME[25] <- "BCN/AEROPUERTO                          "
  stations$abb <- substr(stations$STANAME,1,6)
  
  # Set train-test sets
  idx.years <- format(itx.mat$Date, "%Y")
  tab.years <- table(idx.years)
  length(tab.years)
  idx.train <- which(!is.na(match(idx.years,names(tab.years)[1:51])))
  #idx.test <- which(is.na(match(idx.years,names(tab.years)[1:51])))
  
  # AUC matrix
  AUCmat <- matrix(data = as.numeric(NA), nrow = nrow(stations), ncol = 51)
  colnames(AUCmat) <- paste0("AUC",sprintf("%02d", 1:51))
  
  # Create matrix to save AUC values
  AUC.df <- data.frame(STAID = stations$STAID,
                       STAAB = stations$abb)
  AUC.df <- cbind(AUC.df, AUCmat)
  # Variable names
  eralevels <- paste0("g",c("300","500","700"))
  
  # Define variable names
  v.names <- c("g300.","g500.","g700.", # Variables closest to each of the station being considered
               "g300.45N.10W","g500.45N.10W","g700.45N.10W",
               "g300.35N.10W","g500.35N.10W","g700.35N.10W",
               "g300.45N.5E","g500.45N.5E","g700.45N.5E",
               "g300.35N.5E","g500.35N.5E","g700.35N.5E")
  
  # Now add lag variables for each of the geopotential variables
  lag.names <- paste0(v.names,".lag1")
  
  # Now add poly terms for each of the geopetential variables
  poly.terms <- paste0(rep(paste0(paste0("poly(",v.names),", 2)"),each=2),
                       c("1","2"))
  # Create an array containing all the variable's names
  final.variables <- c(v.names,lag.names,poly.terms)
  
  # Create matrix to save z-values
  z.3d <- array(data = as.numeric(NA),
                dim = c(nrow(stations), length(final.variables), 51))
  
  for(ss in 1:nrow(stations)){
    
    tic <- Sys.time()
    print(paste0("Station: ",ss,"/",nrow(stations)))
    
    # Create data.frame
    bin_df <- data.frame(Ix = itx.mat[[paste0("X",stations$STAID[ss])]])
    
    # Include variables of interest
    fivep <- c(stations$point[ss], cpoints)
    eranames <- paste(rep(eralevels, length(fivep)),
                      rep(fivep, each = length(eralevels)),
                      sep = ".")
    bin_df <- cbind(bin_df, gall[eranames])
    
    # Add lag-one for each variable
    for(ii in 2:ncol(bin_df)){
      col_name <- paste0(names(bin_df)[ii], ".lag1")
      bin_df[[col_name]] <- dplyr::lag(bin_df[,ii], 1, 0)
    }
    
    # Geopotential model formula
    frm1 <- as.formula(paste0("Ix~",
                              capture.output(cat(eranames, sep = "+"))))
    
    # Geopotential model with lag variables formula
    lagnames <- names(bin_df)[grep("lag",names(bin_df))]
    frm2 <- as.formula(paste0("Ix~",
                              capture.output(cat(c(eranames,lagnames),
                                                 sep = "+"))))
    
    # Geopotential model with second order polynomial terms formula
    polynames <- sapply(eranames, function(name) paste0("poly(", name,",2)"))
    frm3 <- as.formula(paste0("Ix~",
                              capture.output(cat(polynames, sep = "+"))))
    
    # Geopotential model with second order polynomial and lag terms formula
    frm4 <- as.formula(paste0("Ix~",
                              capture.output(cat(c(polynames,lagnames),
                                                 sep = "+"))))
    
    for(tt in 1:51){
      
      #print(paste0("Year: ",tt,"/51"))
      idx.k <- which(!is.na(match(idx.years,names(tab.years)[tt])))
      
      # Compute GLMs
      fit1 <- glm(formula = frm1,
                  data = bin_df[idx.train[-idx.k],],
                  family = binomial(link = "logit"))
      fit2 <- glm(formula = frm2,
                  data = bin_df[idx.train[-idx.k],],
                  family = binomial(link = "logit"))
      fit3 <- glm(formula = frm3,
                  data = bin_df[idx.train[-idx.k],],
                  family = binomial(link = "logit"))
      fit4 <- glm(formula = frm4,
                  data = bin_df[idx.train[-idx.k],],
                  family = binomial(link = "logit"))
      
      # Compute a stepAIC to each of the models
      #tic <- Sys.time()
      #cat("\nRunning stepAIC")
      fit1_step <- stepAIC(object = fit1, direction = "both", trace = FALSE)
      fit2_step <- stepAIC(object = fit2, direction = "both", trace = FALSE)
      fit3_step <- stepAIC(object = fit3, direction = "both", trace = FALSE)
      fit4_step <- stepAIC(object = fit4, direction = "both", trace = FALSE)
      #toc <- Sys.time()
      
      # List of fitted models
      fitted_models <- list(fit1_step,fit2_step,fit3_step,fit4_step)
      
      # Compute AIC for each model
      AIC_values <- sapply(fitted_models, AIC)
      
      # Identify the index of the model with the lowest AIC
      index_lowest_AIC <- which.min(AIC_values)
      
      # Save the fitted model with the lowest AIC
      best_model <- fitted_models[[index_lowest_AIC]]
      
      # Calculate the AUC value
      predict.test <- predict(best_model,
                              newdata = bin_df[-idx.train,],
                              type = "response")
      roc.aux <- roc(bin_df[-idx.train,]$Ix, predict.test,
                     print.auc = F, quiet = T)
      #print(auc(roc.aux))
      
      # Save AUC values in data.frame
      AUC.df[ss,tt+2] <- auc(roc.aux)
      
      # Save best model
      #saveRDS(best_model, file = file.path(outdir,paste0(stations$STAID[ss],".rds")))
      
      # Keep only the z values from the models
      z.value <- summary(best_model)$coefficients[,3]
      z.names <- names(z.value)
      
      # Define further grid and get the variables from the further grid
      far.idx <- grep("45N.10W|45N.5E|35N.10W|35N.5E", z.names)
      # Get the variables closest to each of the stations
      z.names[-c(1,far.idx)] <- gsub(pattern = "\\d+[A-Z]\\.\\d+[A-Z]",
                                     replacement =  "",
                                     x = z.names[-c(1,far.idx)])
      
      # Match the names with the z matrix names
      matched.idx <- match(z.names[-1], final.variables)
      
      # Add the data to the z matrix
      z.3d[ss, matched.idx, tt] <- z.value[-1]
      

      
    }# for tt in years
    
    # Save AUC matrix
    write.csv(x = AUC.df,
              file = file.path(outdir,"boot_AUC.csv"),
              row.names = F)
    
    # Save z-values
    saveRDS(object = z.3d,
            file = file.path(outdir,"boot_zval.rds"))
    
    toc <- Sys.time()
    print(toc-tic)# 13.69895 mins
    
  }# for ss in stations
  
  
}# if outdir exists
