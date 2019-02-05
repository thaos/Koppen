# Cree le 5 avril 2015
# calcule koppen, pour le globe, avec les dimensions de la grille en arguments
# copie adaptee du script original de Alexis Berg
# ---------------------------------------------------------------------------------------
# POUR DES DONNEES CLIM RANGEES DU SUD AU NORD
# ---------------------------------------------------------------------------------------

koppen <- array(0, dim=c(lon_length,lat_length))

eq_north <- 1+lat_length/2
eq_south <- lat_length/2


########## SNOW CLIMATE  ###################
print("det. group D")
#il faut définir Psmin, Psmax, Pwmin, Pwmax
Psmin <- array(NA, dim=c(lon_length,lat_length))
Psmax <- array(NA, dim=c(lon_length,lat_length))
Pwmin <- array(NA, dim=c(lon_length,lat_length))
Pwmax <- array(NA, dim=c(lon_length,lat_length))
#hemisphere nord:
for (i in 1:lon_length){
  for (j in eq_north:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      Psmin[i,j] <- min(pluie[i,j,4:9] , na.rm=T)
      Psmax[i,j] <- max(pluie[i,j,4:9] , na.rm=T)
      Pwmin[i,j] <- min(pluie[i,j,c(c(1:3),c(10:12) )] , na.rm=T )
      Pwmax[i,j] <- max(pluie[i,j,c(c(1:3),c(10:12) )] , na.rm=T )
    }
  }
}
#hemisphere sud:
for (i in 1:lon_length){
  for (j in 1:eq_south) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      Psmin[i,j] <- min(pluie[i,j,c(c(1:3),c(10:12) )] , na.rm=T)
      Psmax[i,j] <- max(pluie[i,j,c(c(1:3),c(10:12) )] , na.rm=T)
      Pwmin[i,j] <- min(pluie[i,j,4:9] , na.rm=T )
      Pwmax[i,j] <- max(pluie[i,j,4:9] , na.rm=T )
    }
  }
}

for (i in 1:lon_length){
  for (j in 1:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if   (min(temp[i,j,], na.rm=T) < -3) koppen[i,j] <-  "Df"
      if  ( (min(temp[i,j,], na.rm=T) < -3) && (Psmin[i,j] < Pwmin[i,j]) &&  (Pwmax[i,j] > 3*Psmin[i,j]) && (Psmin[i,j] < 40)) koppen[i,j] <-  "Ds"
      if  ( (min(temp[i,j,], na.rm=T) < -3) && (Pwmin[i,j] < Psmin[i,j]) && (Psmax[i,j] > 10*Pwmin[i,j])) koppen[i,j] <-  "Dw" 
    }
  }
}

## 3eme lettre
for (i in 1:lon_length){
  for (j in 1:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if ((is.na(koppen [i,j])==FALSE) &&  (substr( koppen [i,j], 1, 1) == "D") && (max(temp[i,j,], na.rm=T) >= 22 )){
        koppen[i,j] <- paste( koppen [i,j],"a", sep="") 
      } else if (   (is.na(koppen [i,j])==FALSE) && (substr( koppen [i,j], 1, 1) == "D") && (max(temp[i,j,], na.rm=T) < 22 ) & (length(which(temp[i,j,] >= 10)) >= 4)) {
        koppen[i,j] <- paste( koppen [i,j],"b", sep="") 
      } else if (   (is.na(koppen [i,j])==FALSE) && (substr( koppen [i,j], 1, 1) == "D") && (max(temp[i,j,], na.rm=T) < 22 ) & (length(which(temp[i,j,] >= 10)) < 4) && (min(temp[i,j,], na.rm=T) > -38)) {
        koppen[i,j] <- paste( koppen [i,j],"c", sep="") 
      } else if (   (is.na(koppen [i,j])==FALSE) && (substr( koppen [i,j], 1, 1) == "D") && (max(temp[i,j,], na.rm=T) < 22 ) & (length(which(temp[i,j,] >= 10)) < 4) && (min(temp[i,j,], na.rm=T) <= -38)) {
        koppen[i,j] <- paste( koppen [i,j],"d", sep="") 
      }
    }
  }
}

########## WARM TEMPERATE CLIMATE  ###################
print("det.group C")
for (i in 1:lon_length){
  for (j in 1:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if  ( (min(temp[i,j,], na.rm=T) > -3) && (min(temp[i,j,], na.rm=T) < 18)) koppen[i,j] <-  "Cf"
      if  ( (min(temp[i,j,], na.rm=T) > -3) && (min(temp[i,j,], na.rm=T) < 18) && (Psmin[i,j] < Pwmin[i,j]) &&  (Pwmax[i,j] > 3*Psmin[i,j]) && (Psmin[i,j] < 40))   koppen[i,j] <-  "Cs"
      if  ( (min(temp[i,j,], na.rm=T) > -3) && (min(temp[i,j,], na.rm=T) < 18) && (Pwmin[i,j] < Psmin[i,j]) && (Psmax[i,j] > 10*Pwmin[i,j]))   koppen[i,j] <-  "Cw" 
    }
  }
}

# 3eme lettre:

for (i in 1:lon_length){
  for (j in 1:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if (    (is.na(koppen [i,j])==FALSE) &&  (substr( koppen [i,j], 1, 1) == "C") && (max(temp[i,j,], na.rm=T) >= 22 )) {
        koppen[i,j] <- paste( koppen [i,j],"a", sep="") 
      } else if (   (is.na(koppen [i,j])==FALSE) && (substr( koppen [i,j], 1, 1) == "C") && (max(temp[i,j,], na.rm=T) < 22 ) && (length(which(temp[i,j,] >= 10)) >= 4)) {
        koppen[i,j] <- paste( koppen [i,j],"b", sep="") 
      } else if (   (is.na(koppen [i,j])==FALSE) && (substr( koppen [i,j], 1, 1) == "C") && (max(temp[i,j,], na.rm=T) < 22 ) && (length(which(temp[i,j,] >= 10)) < 4) && (min(temp[i,j,], na.rm=T) > -38)) {
        koppen[i,j] <- paste( koppen [i,j],"c", sep="") 
      } else if (   (is.na(koppen [i,j])==FALSE) && (substr( koppen [i,j], 1, 1) == "C") && (max(temp[i,j,], na.rm=T) < 22 ) && (length(which(temp[i,j,] >= 10)) < 4) && (min(temp[i,j,], na.rm=T) <= -38)) {
        koppen[i,j] <- paste( koppen [i,j],"d", sep="") 
      }
    }
  }
}

############ EQUATORIAL ################
print("det. group A")
# Equatorial savannah with dry summer
for (i in 1:lon_length){
  # Hemisphere Nord:
  for (j in eq_north:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if ( (min(temp[i,j,], na.rm=T) >= 18) && (min(pluie[i,j,4:9], na.rm=T)  < 60))  {
        koppen[i,j] <-  "As"
      } else if ( ( min(temp[i,j,], na.rm=T) >= 18) && (min(pluie[i,j,c(c(1:3),c(10:12) )], na.rm=T)  < 60))  {
        koppen[i,j] <-  "Aw"
      }
    }
  }
  # Hemisphere Sud:
  for (j in 1:eq_south) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if ( ( min(temp[i,j,], na.rm=T) >= 18) && (min(pluie[i,j,c(c(1:3),c(10:12) )], na.rm=T)  < 60))  {
        koppen[i,j] <-  "As"
      } else if ( ( min(temp[i,j,], na.rm=T) >= 18) && (min(pluie[i,j,4:9], na.rm=T)  < 60)) {
        koppen[i,j] <-  "Aw"
      }
    }
  }
}

#Equatorial monsoon
for (i in 1:lon_length){
  for (j in 1:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if (   (sum(pluie[i,j,], na.rm=T) >= 25*(100-(min(pluie[i,j,], na.rm=T) )) ) &&  ( min(temp[i,j,], na.rm=T) >= 18))  koppen[i,j] <-  "Am" 
    }
  }
} 
# Equatorial... A
#Equatorial rainforest...
for (i in 1:lon_length){
  for (j in 1:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if (   (min(pluie[i,j,], na.rm=T) >= 60) &&  ( min(temp[i,j,], na.rm=T) >= 18))  koppen[i,j] <- "Af"  
    }
  }
} 


################## ARID CLIMATE
print("det. group B")
# pour ca il faut calculer "Pth"
Pth <- array(NA, dim=c(lon_length,lat_length))
P_summer <- array(NA, dim=c(lon_length,lat_length))
P_winter <- array(NA, dim=c(lon_length,lat_length))
#hemisphere nord:
for (i in 1:lon_length){
  for (j in eq_north:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      P_summer[i,j] <- sum(pluie[i,j,4:9] , na.rm=T)
      P_winter[i,j] <- sum(pluie[i,j,c(c(1:3),c(10:12) )] , na.rm=T )
    }
  }
}
#hemisphere sud:
for (i in 1:lon_length){
  for (j in 1:eq_south) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      P_winter[i,j] <- sum(pluie[i,j,4:9] , na.rm=T)
      P_summer[i,j] <- sum(pluie[i,j,c(c(1:3),c(10:12) )] , na.rm=T )
    }
  }
}

for (i in 1:lon_length){
  for (j in 1:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      Pth[i,j] <- 2*mean(temp[i,j,], na.rm=T) + 14 
      if (P_winter[i,j] > 2/3*sum(pluie[i,j,], na.rm=T)) Pth[i,j] <- 2*mean(temp[i,j,], na.rm=T) 
      if (P_summer[i,j] > 2/3*sum(pluie[i,j,], na.rm=T)) Pth[i,j] <- 2*mean(temp[i,j,], na.rm=T) +28 
    }
  }
}
for (i in 1:lon_length){
  for (j in 1:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if ( (is.na(Pth[i,j])==F) &&  (sum(pluie[i,j,], na.rm=T) < Pth[i,j] * 10) && ( sum(pluie[i,j,], na.rm=T) > Pth[i,j] *5)){
        koppen[i,j] <- "BS"
      } else if ((is.na(Pth[i,j])==F) &&  sum(pluie[i,j,], na.rm=T) <= Pth[i,j] *5){
        koppen[i,j] <- "BW"
      }
    }
  }
}
## troisieme lettre B
for (i in 1:lon_length){
  for (j in 1:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if (    (is.na(koppen [i,j])==FALSE) &&  (substr( koppen [i,j], 1, 1) == "B") && (mean(temp[i,j,], na.rm=T) >= 18 )){
        koppen[i,j] <- paste( koppen [i,j],"h", sep="")
      } else if (   (is.na(koppen [i,j])==FALSE) && (substr( koppen [i,j], 1, 1) == "B") && (mean(temp[i,j,], na.rm=T) < 18 )) {
        koppen[i,j] <- paste( koppen [i,j],"k", sep="") 
      }
    }
  }
}

############ POLAR CLIMATE ##########################
print("det. group E")
for (i in 1:lon_length){
  for (j in 1:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if (   (max(temp[i,j,], na.rm=T) < 10) &&  ( max(temp[i,j,], na.rm=T) >= 0))  koppen[i,j] <- "ET"   
    }
  }
} 
for (i in 1:lon_length){
  for (j in 1:lat_length) {
    # ajout d'un filtre pour les points de mer : valeur NA, non traitable par le script
    if( all(is.na(temp[i,j,])) ) {koppen[i,j] <- "mer"
    } else if( all(is.na(pluie[i,j,])) ) {koppen[i,j] <- "mer"
    } else {
      if   (max(temp[i,j,], na.rm=T) < 0) koppen[i,j] <- "EF"   
    }
  }
} 


