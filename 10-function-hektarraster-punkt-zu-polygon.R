################################################################################
# Funktion, die es ermöglicht, Hektarrasterdaten (Punkt x/y plus Attribute) in
# Spatial Polygons mit 100x100m-Abmessungen zu konvertieren
#
# Georg Russ, 2015-09-04
################################################################################

require(rgeos)
require(maptools)
require(dplyr)
#require(png)
require(parallel)
#require(cleangeo) # zum Bearbeiten/Bereinigen von (kaputten) Geodaten

hr_to_shape <- function(df, relifield, xfield, yfield, CRSin, CRSout){

	time1 <- Sys.time()
	cat("Konvertierung von Hektarraster-Punktdaten zu Hektarraster-Polygonen\n\n")
	df <- as.data.frame(df)
	row.names(df) <- df[[relifield]]
	# zeile wird entsprechend vorbearbeitet, dass genau drei felder (reli, x, y)
	# in der richtigen Reihenfolge da stehen
	punkt_zu_hektar_polygon <- function(zeile){
					#cat(zeile)
					reli <- zeile[1]
					x <- as.numeric(zeile[2])
					y <- as.numeric(zeile[3])
					coords <- rbind(c(x,y),
													c(x+100,y),
													c(x+100,y+100),
													c(x,y+100),
													c(x,y)
													)
					p1 <- list(Polygon(coords))
					p2 <- Polygons(p1, ID=reli)
					return(p2)
	}

	cat("Polygone erstellen, (mit Cluster parallelisiert) ... \n")
	cl <- makeCluster(14)
	packs <- clusterEvalQ(cl, library("sp"))
	#clusterExport(cl, varlist=c(


	pols <- parApply(cl=cl, 
									 X=dplyr::select(df, one_of(relifield, xfield, yfield)),
									 MARGIN=1,
									 FUN=punkt_zu_hektar_polygon)

	names(pols) <- df[,relifield]
	stopCluster(cl)

#	cat("Polygone erstellen, (kann dauern, Parallelisierung wär gut) ... \n")
#	d1 <- apply(dplyr::select(df, one_of(relifield, xfield, yfield)), 1, punkt_zu_hektar_polygon)
#	names(d1) <- df$RELI

	cat("SpatialPolygons erstellen (Single-CPU...) \n")
	spp <- SpatialPolygons(pols, proj4string = CRS(CRSin))

	cat("SpatialPolygonsDataFrame erstellen (Single-CPU...) \n")
	sdhh <- SpatialPolygonsDataFrame(Sr=spp, data=dplyr::select(df, -one_of(relifield,xfield,yfield)))

	# Projektion könnte man auch vorher machen, wenn es eh nur zwischen LV03 oder
	# LV95 hin- und hergeht. Die 100m-Zellen sind ja bei beiden gleich in der Erstellung...
	cat("ins gewünschte CRS projizieren (Single-CPU...) \n")
	sdhh <- spTransform(sdhh, CRS(CRSout))

	time2 <- Sys.time()
	cat("Zeitbedarf für Umwandlung:\n")
	print(time2-time1)
	return(sdhh)
	
}

# Beispielaufruf:

# sft <- hr_to_shape(df=dfhh[1:100000,], 
# 									 relifield = "RELI",
# 									 xfield = "X_KOORD",
# 									 yfield = "Y_KOORD",
# 									 CRSin = "+init=epsg:21781",
# 									 CRSout = "+init=epsg:2056"
# 									 )
