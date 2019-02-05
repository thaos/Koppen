# seldate,date1[,date2] ifile ofile
# cdo ymonmean ifile ofile

system("cdo ymonmean -seldate,1987-01-01T00:00:00,2001-12-31T23:59:59 cru_ts4.02.1901.2017.pre.dat.nc cru_ts4.02.1987.2001.pre.ymonmean.nc")

system("cdo ymonmean -seldate,1987-01-01T00:00:00,2001-12-31T23:59:59 cru_ts4.02.1901.2017.tmp.dat.nc cru_ts4.02.1987.2001.tmp.ymonmean.nc")

library(ncdf4)
library(devtools)

climates <- c("Af","Am","As","Aw","BWk","BWh","BSk","BSh","Cfa","Cfb","Cfc","Csa","Csb","Csc","Cwa","Cwb","Cwc","Dfa","Dfb","Dfc","Dfd","Dsa","Dsb","Dsc","Dsd","Dwa","Dwb","Dwc","Dwd","EF","ET")
palette <- c("indianred3", "indianred1", "lightsalmon2", "lightcoral" , "lightgoldenrod", "yellow",    "gold",  "black", "darkseagreen2",  "mediumseagreen","darkseagreen1", "darkolivegreen2", "darkolivegreen1","lightgreen" , "antiquewhite","antiquewhite3", "bisque4","lightpink3",  "lightpink1", "mistyrose3", "rosybrown2","rosybrown1" , "lightpink", "lavender", "grey", "lightblue" ,"paleturquoise" ,"paleturquoise3",   "lightblue3", "lightcyan2", "lightcyan")

nc_T <- nc_open("cru_ts4.02.1987.2001.tmp.ymonmean.nc")
Tmonth <- ncvar_get(nc_T, "tmp")
lat <- ncvar_get(nc_T, "lat")
lon <- ncvar_get(nc_T, "lon")
nc_close(nc_T)
nc_P <- nc_open("cru_ts4.02.1987.2001.pre.ymonmean.nc")
Pmonth <- ncvar_get(nc_P, "pre")
nc_close(nc_T)
lat_mat <- matrix(lat, nrow = length(lon), ncol = length(lat), byrow = TRUE)

devtools::load_all("KoppenClassifyR/")
ij_df <- expand.grid(i = seq_along(lon), j = seq_along(lat))
koppen_classes <- mapply(
  function(i, j){
    classify_koppen(Tmonth[i, j, ], Pmonth[i, j,], lat_mat[i, j])
  },
  i = ij_df$i, j = ij_df$j, 
  SIMPLIFY = TRUE
) 
koppen_classes <- factor(koppen_classes, levels = climates)

koppen_map <- matrix(as.numeric(koppen_classes), nrow = length(lon), ncol = length(lat))
breaks <- seq.int(length(climates) + 1) - 0.5
image(koppen_map, breaks = breaks, col = palette)  

lat_length <- length(lat)
lon_length <- length(lon)
temp <- Tmonth
pluie <- Pmonth

source("Koppen_globe_determine_argLatLon_South2North.R")
koppen_classes_original <- factor(koppen, levels = climates)
koppen_map_original <- matrix(as.numeric(koppen_classes_original), nrow = length(lon), ncol = length(lat))
breaks <- seq.int(length(climates) + 1) - 0.5
image(koppen_map_original, breaks = breaks, col = palette) 


diff_map <- koppen_map - koppen_map_original
image(diff_map)
sum(!is.na(koppen_map))
idiff <- which(diff_map != 0)
length(idiff)
diff_df <- data.frame(koppen_classes, koppen_classes_original)[idiff,]
print(diff_df)
diff_table <- table(diff_df)
print(diff_table)
sum(diff_table)
