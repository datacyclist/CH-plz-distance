##############################
# Berechnung von Distanzen zwischen PLZ
#
# github@georgruss.ch, 2021-08-06
##############################

require(rgdal)
#library(dplyr)
#library(tidyr)
library(sf)

source("10-download-data.R") # gets official data from URL
source("11-load-plzdata.R") # loads SpatialPolygonsDataFrame sPLZ

#sPLZ <- readOGR(dsn="plz/PLZO_GDB_LV95/PLZO_GDB_LV95.gdb", verbose=TRUE, layer="PLZO_OSNAME")
sPLZ <- readOGR(dsn="plz/PLZO_GDB_LV95/PLZO_GDB_LV95.gdb", layer="PLZO_PLZ")


# Distanzen zwischen zwei PLZ-Polygonen berechnen

get_dists <- function(plz1, plz2){

#plz1 <- 1007
#plz2 <- 8001
	
	# einmal mit SpatialPoints
	splz1 <- sPLZ[sPLZ$PLZ==plz1,]
	splz2 <- sPLZ[sPLZ$PLZ==plz2,]
	
	# einmal mit Simple Features
	sf_plz1 <- st_as_sf(splz1)
	sf_plz2 <- st_as_sf(splz2)
	
	##############################
	# Distanz = Abstand zwischen der Mitte der Bounding Boxes der jeweiligen PLZ
	##############################
	
	sp_mitte_bbox_plz1 <- SpatialPoints( coords=data.frame(
																	 		x=mean(bbox(splz1)[1,]),
																			 y=mean(bbox(splz1)[2,])
																			 ))
	sp_mitte_bbox_plz2 <- SpatialPoints( coords=data.frame(
																	 		x=mean(bbox(splz2)[1,]),
																			 y=mean(bbox(splz2)[2,])
																			 ))
	
	dist_mitte_bbox_meter <- spDistsN1(sp_mitte_bbox_plz1, sp_mitte_bbox_plz2)
	
	
	##############################
	# Distanz = Abstand der Zentroide beider PLZ
	##############################

	# Gemeinden können aus mehreren disjunkten Polygonen bestehen (z.B. 9500 Wil
	# SG), das ergibt dann mehrere Distanzen -- interessant ist aber das Minimum
	
	dist_centroid_meter <- min(st_distance(
																		 st_centroid(sf_plz1),
																		 st_centroid(sf_plz2)
																		 )
	)

	##############################
	# Distanz = minimaler Abstand der Polygone (bei benachbarten PLZ = Null)
	##############################
	
	
	# create an index of the nearest feature
	# index <- st_nearest_feature(x=sf_plz1, y=sf_plz2)
	
	dists_pol <- st_distance(x = sf_plz1, y = sf_plz2)
	
	# Gemeinden können aus mehreren disjunkten Polygonen bestehen (z.B. 9500 Wil
	# SG), das ergibt dann mehrere Distanzen -- interessant ist aber das Minimum
	dist_polygone_minimal_meter <- min(dists_pol)


dists <- data.frame(
										plz1,
										plz2,
										dist_mitte_bbox_meter,
										dist_centroid_meter,
										dist_polygone_minimal_meter
										)

return(dists)

}

# Beispiel: Lausanne und Zürich
t1 <- get_dists(1007,8001)

print(t1)



