#' @title Odds ratio calculation between two volumes
#'
#' @description This function calculates the odds ratio between two volumes
#' containing binary data.
#'
#' @usage or3d(vol1, vol2)
#'
#' @param vol1 Input 3D volume.
#' @param vol2 Input 3D volume.
#'
#' @details The OR is computed along the 2nd and 3rd dimensions.
#'
#' @return a data.frame summarizing the OR along the 1st dimension of both
#' volumes.
#' 
#' @author Zeus Gracia-Tabuenca
#'
#' @examples
#' \donttest{
#' aux_df <- or3d(vol1, vol2)
#' }
#'
#' @export

or3d <- function(vol1, vol2){
  # Check dimension
  if(!identical(dim(vol1),dim(vol2))) stop("Dimensions differ!!")
  # Compute odds ratios by year
  n11 <- apply(vol1+vol2, 1, function(x) sum(x==2, na.rm = T))+0.5
  n00 <- apply(vol1+vol2, 1, function(x) sum(x==0, na.rm = T))+0.5
  n01 <- apply(vol1-vol2, 1, function(x) sum(x==-1, na.rm = T))+0.5
  n10 <- apply(vol1-vol2, 1, function(x) sum(x==1, na.rm = T))+0.5
  # Compute OR
  return(data.frame(OR=(n11*n00)/(n10*n01), t=1:dim(vol1)[1]))
}
