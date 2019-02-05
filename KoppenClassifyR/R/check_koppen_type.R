get_winter_months <- function(lat){
  stopifnot(length(lat) == 1)
  if (lat >= 0) {
    months <- c(1:3, 10:12)
  } else{
    months <- 4:9
  }
  return(months)
}
# get_winter_months(30)
# get_winter_months(-30)

get_summer_months <- function(lat){
  months <- 1:12
  months[!months %in% get_winter_months(lat)]
}
# get_summer_months(30)
# get_summer_months(-30)



compute_Pth <- function(Psummer, Pwinter, Tann){
  Pann <- Psummer + Pwinter
  if(Pwinter >= 2/3 * Pann){
    Pth <- 2 * Tann
  } else if(Psummer >=  2/3 * Pann){
    Pth <- 2 * Tann + 28
  } else{
    Pth <- 2 * Tann + 14
  }
  return(Pth)
}

create_compute_Pxmxx <- function(get_season_months, fun){
  compute_Psmxx <- function(Pmonth, lat){
    fun(Pmonth[get_season_months(lat)], na.rm = TRUE)
  }
}
compute_Psmin <- create_compute_Pxmxx(get_season_months = get_summer_months, fun = min)
compute_Psmax <- create_compute_Pxmxx(get_season_months = get_summer_months, fun = max)
compute_Pwmin <- create_compute_Pxmxx(get_season_months = get_winter_months, fun = min)
compute_Pwmax <- create_compute_Pxmxx(get_season_months = get_winter_months, fun = max)
compute_Psummer <- create_compute_Pxmxx(get_season_months = get_summer_months, fun = sum)
compute_Pwinter <- create_compute_Pxmxx(get_season_months = get_winter_months, fun = sum)

check_E <- function(Tmax){
  Tmax < 10
}

check_ET <- function(Tmax){
  Tmax >= 0 & Tmax < 10
}
check_EF <- function(Tmax){
  Tmax < 0
}

check_B <- function(Pann, Pth){
  Pann < 10 * Pth
}

check_BS <- function(Pann, Pth){
  Pann > 5 * Pth
}

check_BW <- function(Pann, Pth){
  !check_BS(Pann, Pth)
}

check_A <- function(Tmin){
  Tmin >= 18 
}

check_Af <- function(Pmin){
  Pmin >= 60
}

check_Am <- function(Pmin, Pann){
  Pann >= 25 * (100 - Pmin)
}

check_As <- function(Psmin){
  Psmin < 60
}

check_Aw <- function(Pwmin){
  Pwmin < 60
}

check_C <- function(Tmin){
  Tmin > -3 & Tmin < 18 
}

check_Xs <- function(Psmin, Psmax, Pwmin, Pwmax){
  Psmin < Pwmin &
    Pwmax > 3 * Psmin &
    Psmin < 40
}

check_Xw <- function(Psmin, Psmax, Pwmin, Pwmax){
  Pwmin < Psmin & Psmax > 10 * Pwmin 
}

check_Xf <- function(Psmin, Psmax, Pwmin, Pwmax){
  !check_Xs(Psmin, Psmax, Pwmin, Pwmax) &
    ! check_Xw(Psmin, Psmax, Pwmin, Pwmax) 
}

check_D <- function(Tmin){
  Tmin <=  -3
}

check_h <- function(Tann){
  Tann >= 18
}

check_k <- function(Tann){
  !check_h(Tann)
}

check_a <- function(Tmax){
  Tmax >= 22
}  

check_b <- function(Tmon, Tmax){
  !check_a(Tmax) & 
    sum(Tmon >= 10) >= 4
}

check_c <- function(Tmon, Tmax, Tmin){
  !check_b(Tmon, Tmax) &
    Tmin > -38
}

check_d <- function(Tmon, Tmax, Tmin){
  !check_b(Tmon, Tmax) &
    Tmin <= -38
}

get_hk <- function(Tann){
  if(check_h(Tann)) return("h")
  if(check_k(Tann)) return("k")
  return("")
}

get_abcd <- function(Tmon, Tmax, Tmin){
  if(check_a(Tmax)) return("a")
  if(check_b(Tmon, Tmax)) return("b")
  if(check_c(Tmon, Tmax, Tmin)) return("c")
  if(check_d(Tmon, Tmax, Tmin)) return("d")
  return("")
}

