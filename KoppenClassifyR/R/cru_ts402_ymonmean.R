#' CRU ts4.02 datasets
#'
#' Temperature and precipitation data form the CRU dataset.
#'
#' @format A list with the following elements:
#' \describe{
#'   \item{Tmonth}{array of monthly temperature averaged over the 1987-2001 period.
#'   Temperatures are expressed in degrees Celsius. Tmonth as dimension [lon x lat x month]}
#'   \item{Pmonth}{array of monthly precipitaion averaged over the 1987-2001 period.
#'   Precipitaitons are expressed in degrees Celsius. Pmonth as dimension [lon x lat x month]}
#'   \item{lon}{vector of longitudes}
#'   \item{lat}{vector of latitudes}
#' }
#' @source \url{https://crudata.uea.ac.uk/cru/data/hrg/cru_ts_4.02/ge/}
#' Harris, I., Jones, P.D., Osborn, T.J. and Lister, D.H. (2014),
#' Updated high-resolution grids of monthly climatic observations - the CRU TS3.10 Dataset. 
#' International Journal of Climatology 34, 623-642  doi:10.1002/joc.3711
"cru_ts402_ymonmean"