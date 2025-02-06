#' Get data for the atmospheric variable of your choice and the altitudes of your choice for the closest and the furthest four surrounding points of your station of choice
#'
#' @param sta_name Give the station name of your choice, it needs to be in between "" and match the names in the stations data frame that you also need to input in the function
#' @param data_dir Give your data folder, don't give the whole path, just the name of the folder inside the working directory
#' @param altitudes Give a vector or a value of altitude, they need to be in between "".
#' @param variable Give the variable that you want, either g,rh, or t. You can only give one of them and they need to be in between "".
#' @param folder Give the path of your folder containing the data for the atmospheric variables. Here you do need to give the whole path.
#' @param itx3d Give a previously generated array with the record series for all stations as binary indicators.
#' @param stations Give a previously generated data frame containing data for the stations (names, latitude, longitude) considered, they must be in the same order as in itx3d.
#' @param days Give a vector of two elements with the firs day of the year you want to get data for and the last day of the year you want to get data for. It has to be in the format "%m-%d" where the month goes first and the day of the month second, and they are separated by a - E.g. c("06-01", "08-31")
#'
#' @return A data frame
#' @export
#'
#' @examples
#' atm_var_station("ZARAGOZA", "Data", c("300", "500", "700"), "g", "C:/Users/elsac/Documents/ERA5-data/Data/geo.data/", itx3d, stations, c("06-01", "08-31"))

atm_var_station <- function(sta_name,data_dir,functions_dir,altitudes,variable,folder,itx3d, stations,days){

  # Find the data directory
  data_dir <- file.path(data_dir)

  # Take only the station in Zaragoza
  id_sta <- grep(sta_name, stations$STANAME)
  lat_sta <- stations$LAT[id_sta]
  lon_sta <- stations$LON[id_sta]

  # Read the latitudes and longitudes available in our atmospheric variables
  lat <- readRDS(file = file.path(data_dir,"geo.data/lat.rds"))
  lon <- readRDS(file = file.path(data_dir,"geo.data/lon.rds"))

  # Find the avaliable grid points
  aux_df <- data.frame(latitude = c(lat), longitude = c(lon))
  grid_points <- unique(aux_df)

  # Find the four column points surrounding the staion and the Iberian peninsula
  small_grid <- data.frame(latitude = c(floor(lat_sta),ceiling(lat_sta),floor(lat_sta),ceiling(lat_sta)),
                           longitude = c(floor(lon_sta),floor(lon_sta),ceiling(lon_sta),ceiling(lon_sta)))
  large_grid <- data.frame(latitude = c(max(grid_points$latitude),min(grid_points$latitude),max(grid_points$latitude),min(grid_points$latitude)),
                           longitude = c(max(grid_points$longitude),max(grid_points$longitude),min(grid_points$longitude),min(grid_points$longitude)))

  # Create the array and add the data for the records first.
  bin_df <- data.frame(Ix = c(t(itx3d[,,id_sta])),
                       t = rep(x = 1:voldim[1], each = voldim[2]),
                       l = rep(x = 1:voldim[2], times = voldim[1]))
  
  source(file.path(functions_dir,"get_atmospheric_data.R"))


  # Loop through each altitude
  for (altitude in altitudes) {
    print(altitude)
    # Loop through each pair of latitude and longitude values
    for (i in 1:nrow(small_grid)) {
      print(i)
      lat <- small_grid$latitude[i]
      lon <- small_grid$longitude[i]
      if (lat > 0.5){
        lat_name<- paste0(lat,"N")
      }else{lat_name<- paste0(lat,"S")}
      if (lon > 0.5){
        lon_name<- paste0(lon,"E")
      }else{lon_name<- paste0(abs(lon),"W")}
      # Call get_atmospheric_data for each combination of altitude, latitude, and longitude
      bin_df[paste0(variable,".", altitude, ".", lat_name,".",lon_name)] <- get_atmospheric_data(folder, "12pm", variable, altitude, lat, lon, days)
    }
    for (i in 1:nrow(large_grid)) {
      print(i)
      lat <- large_grid$latitude[i]
      lon <- large_grid$longitude[i]
      if (lat > 0.5){
        lat_name<- paste0(lat,"N")
      }else{lat_name<- paste0(lat,"S")}
      if (lon > 0.5){
        lon_name<- paste0(lon,"E")
      }else{lon_name<- paste0(abs(lon),"W")}
      # Call get_atmospheric_data for each combination of altitude, latitude, and longitude
      bin_df[paste0(variable,".", altitude, ".", lat_name,".",lon_name)] <- get_atmospheric_data(folder, "12pm", variable, altitude, lat, lon, days)
    }
  }

  return(bin_df)
}

