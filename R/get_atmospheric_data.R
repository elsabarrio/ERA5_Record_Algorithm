#' Get an array of data for an atmospheric variable
#'
#' @param folder_path Give the path of the folder where the data files are kept.
#' @param ftime Give a specific time E.g."12pm" It has to be in between "" and have the 12-hour-clock format.
#' @param variable Specify the variable "rh" for relative humidity, "g" for geopotential, "t" for temperature.
#' @param geo_level Give a value of the pressure level without units, E.g "300".
#' @param lat Give a value for latitude E.g 40.
#' @param lon Give a value for longitude E.g. -10
#' @param days Give a vector of two elements with the firs day of the year you want to get data for and the last day of the year you want to get data for. It has to be in the format "%m-%d" where the month goes first and the day of the month second, and they are separated by a - E.g. c("06-01", "08-31").
#'
#' @return An array with the data for an atmospheric variable in a geopotential level for a specific time in a specific point and for a range of days of the year. Build the file name
#'
#' @examples
#' get_atmospheric_data("C:/Users/elsac/Documents/ERA5-data/Data/geo.data/", "12pm", "rh", "300", 40, -10, c("06-01", "08-31"))
#'
get_atmospheric_data <- function(folder_path, ftime, variable, geo_level, lat, lon, days){

  # Find file name with given arguments
  file_name <- paste0(variable, "_", geo_level, "_", ftime, "_60_23.rds")

  # Read the variable data
  eravar <- readRDS(file.path(folder_path, file_name))

  # Read the latitude, longitude and date data
  latitudes  <- readRDS(file.path(folder_path, "lat.rds"))
  longitudes  <- readRDS(file.path(folder_path, "lon.rds"))
  dates <- readRDS(file.path(folder_path, "Date.rds"))

  # Create a data frame joining the variable, latitude, longitude, and date data
  new.df <- data.frame(v = eravar,
                       latitude = latitudes,
                       longitude = longitudes,
                       DATE = dates)

  # Remove the 29ths of February
  new.df$DATE <- as.Date(new.df$DATE, format = "%m/%d/%Y")
  new.df <- subset(new.df, !(format(new.df$DATE, "%m-%d") == "02-29"))

  # Select the data only for the data in between the days inputted
  new.df <- subset(new.df, format(DATE, "%m-%d") >= days[1] & format(DATE, "%m-%d") <= days[2])

  # Select the data only for the latitude and longitude inputted
  new.df <- subset(new.df, latitude == lat & longitude == lon)

  # Keep only variable data in an array
  new.data <- as.array(new.df$v)

  # Return variable data
  return(new.data)
}
