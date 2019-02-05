#' Koppen-Geiger climate classification
#' 
#' Find the Koppen-Geiger class for one location.
#' 
#' @param Tmonth A numerical vector of length 12. 
#' It contains near-surface temperature value for each month of the year
#' (January to December). Temperature is expressed in degrees Celsius.
#' @param Pmonth A numerical vector of length 12. 
#' It contains the value of precipitation for each month of the year
#' (January to December). Precipitation is expressed in millimeters.
#' @param lat the latitude of location for which \code{Tmonth} and \code{Pmonth} are provided.
#' @return A string with the name of the Koppen-Geiger class.
#' @examples

classify_koppen <- function(Tmonth, Pmonth, lat){
  stopifnot(length(Tmonth) == 12 & length(Pmonth == 12))
  if (all(is.na(Tmonth)) | all(is.na(Pmonth))) {
    return(NA)
  }
  Psmin <- compute_Psmin(Pmonth, lat)
  Psmax <- compute_Psmax(Pmonth, lat)
  Pwmin <- compute_Pwmin (Pmonth, lat)
  Pwmax <- compute_Pwmax(Pmonth, lat)
  Psummer <- compute_Psummer(Pmonth, lat)
  Pwinter <- compute_Pwinter(Pmonth, lat)
  Pann <- sum(Pmonth, na.rm = TRUE)
  stopifnot(Pann == Pwinter + Psummer)
  # Pmax <- max(Pmonth, na.rm = TRUE)
  Pmin<- min(Pmonth, na.rm = TRUE)
  Tann <- mean(Tmonth, na.rm = TRUE)
  Tmax <- max(Tmonth, na.rm = TRUE)
  Tmin <- min(Tmonth, na.rm = TRUE)
  Pth <- compute_Pth(Psummer, Pwinter, Tann)
  type <- ""
  if (check_E(Tmax)) {
    type <- "E" 
    if (check_ET(Tmax)) return(type <- paste0(type, "T"))
    if (check_EF(Tmax)) return(type <- paste0(type, "F"))
  }
  if (check_B(Pann, Pth)) {
    type <- "B" 
    if (check_BS(Pann, Pth)) {
      type <- paste0(type, "S")
    } else if (check_BW(Pann, Pth)) {
      type <- paste0(type, "W")
    }
    type <- paste0(type, get_hk(Tann))
    return(type)
  }
  if (check_A(Tmin)) {
    type <- "A" 
    if(check_Af(Pmin)) return(type <- paste0(type, "f"))
    if(check_Am(Pmin, Pann)) return(type <- paste0(type, "m"))
    if(check_As(Psmin)) return(type <- paste0(type, "s"))
    if(check_Aw(Pwmin)) return(type <- paste0(type, "w"))
  }
  if (check_C(Tmin)) {
    type <- "C"
  } else if (check_D(Tmin)) {
    type <- "D"
  }
  if (check_Xs(Psmin, Psmax, Pwmin, Pwmax)) {
    type <- paste0(type, "s")
  } else if (check_Xw( Psmin, Psmax, Pwmin, Pwmax)) {
    type <- paste0(type, "w") 
  } else if (check_Xf(Psmin, Psmax, Pwmin, Pwmax)) {
    type <- paste0(type, "f")
  }
  type <- paste0(type, get_abcd(Tmonth, Tmax, Tmin))
  return(type)
}