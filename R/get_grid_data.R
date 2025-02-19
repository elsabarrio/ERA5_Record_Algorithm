#' Creates a grid data.frame of an atmospheric variable
#'
#' @param indir Input directory.
#' @param infile Input file name for atmospheric data.
#' @param inlat Input file name for latitude coordinates.
#' @param inlon Input file name for longitude coordinates.
#' @param indates Input file name for corresponding dates.
#' @param indays Give a vector of two elements with the firs day of the year you want to get data for and the last day of the year you want to get data for. It has to be in the format "%m-%d" where the month goes first and the day of the month second, and they are separated by a - E.g. c("06-01", "08-31").
#' @param outdir Output directory.
#' @param outname Output file name.
#' 
#'
#' @examples
#' get_grid_data(indir = file.path(data_dir, "geo.data"),
#'               infile <- "g_300_12pm_60_23.rds",
#'               inlat <- "lat.rds",
#'               inlon <- "lon.rds",
#'               indates <- "Date.rds",
#'               indays <- c("06-01", "08-31"),
#'               outdir <- file.path(data_dir, "grid_data"),
#'               outfile <- "g300_grid.csv")
#'
get_grid_data <- function(indir, infile, inlat, inlon, indates, indays, outdir, outfile){
  
  # Read the variable data
  eravar <- readRDS(file.path(indir, infile))
  eralat <- readRDS(file.path(indir, inlat))
  eralon <- readRDS(file.path(indir, inlon))
  eradates <- readRDS(file.path(indir, indates))
  
  # Create a data frame joining the variable, latitude, longitude, and date data
  new.df <- data.frame(eravar = eravar,
                       lat = as.character(eralat),
                       lon = as.character(eralon),
                       date = as.Date(eradates, format = "%m/%d/%Y"))
  
  # Extract variable and level from file name
  names(new.df)[1] <- paste(unlist(strsplit(infile,"_"))[1:2], collapse = "")
  
  # Remove the 29ths of February
  #new.df <- subset(new.df, !(format(new.df$date, "%m-%d") == "02-29"))
  
  # Select the data only for the data in between the days inputted
  new.df <- subset(new.df, format(date, "%m-%d") >= indays[1] & format(date, "%m-%d") <= indays[2])
  
  # Relabel latitude
  new.df$lat <- sapply(1:nrow(new.df), function(x){
    aux <- new.df$lat[x]
    if(grepl("^-",aux)){
      aux <- paste0(substr(aux, 2, nchar(aux)),"S")
    } else aux <- paste0(aux,"N")
  })
  # Relabel longitude
  new.df$lon <- sapply(1:nrow(new.df), function(x){
    aux <- new.df$lon[x]
    if(grepl("^-",aux)){
      aux <- paste0(substr(aux, 2, nchar(aux)),"W")
    } else aux <- paste0(aux,"E")
  })
  # Point variable
  new.df$p <- paste0(new.df$lat,new.df$lon)
  new.df <- subset(new.df, select=-c(lat,lon))
  
  # Reshape data.frame
  aux.df <- reshape(data = new.df,
                    idvar = "date",
                    timevar = "p",
                    direction = "wide")
  
  # Save data
  if(!dir.exists(outdir)) dir.create(outdir, recursive = T)
  write.csv(x = aux.df,
            file = file.path(outdir,outfile),
            row.names = F)
}