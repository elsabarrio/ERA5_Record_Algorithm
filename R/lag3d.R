#' @title Lag one for margin two in 3D volume
#'
#' @description This function applies a lag-one into the second dimension of a
#' 3D volume object.
#'
#' @usage lag3d(vol3d)
#'
#' @param vol3d Input 3D volume.
#'
#' @details Default for lagged value is zero.
#'
#' @return Another 3D volume with same dimensions as input data.
#' 
#' @author Zeus Gracia-Tabuenca
#'
#' @examples
#' \donttest{
#' Y365 <- readRDS("Data/Y365.rds")
#' I365 <- apply(X = Y365,
#'               MARGIN = c(2,3),
#'               FUN =  function(x) c(1,as.numeric(diff(cummax(x))>0)))
#' I365.lag1 <- lag3d(vol3D = I365)
#' }
#'
#' @export

lag3d <- function(vol3d){
  # Transpose
  vol3d_dim <- dim(vol3d)
  aux <- apply(vol3d, 3, function(x) t(x))
  # Apply lag1
  aux2 <- rbind(rep(0,ncol(aux)), aux[-nrow(aux),])
  # Transpose again
  aux3 <- array(data = apply(X = array(data = aux2, dim = vol3d_dim[c(2,1,3)]),
                             MARGIN = 3,
                             FUN = function(x) t(x)),
                dim = vol3d_dim)
  return(aux3)
}
