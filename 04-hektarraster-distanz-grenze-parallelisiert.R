##################################################
# Berechnen der Abstände (Luftlinie) von allen HR-Zellen zur Landesgrenze
# Georg Russ, 2015-08-22
#
# Datenbasis: 
# - Hektarrasterzellen und Bevölkerung vom BfS 2013
# http://www.bfs.admin.ch/bfs/portal/de/index/dienstleistungen/geostat/datenbeschreibung/volks-__gebaeude-0.html
# 
# - Landesgrenzen von Swisstopo
# http://www.swisstopo.admin.ch/internet/swisstopo/de/home/products/landscape/swissBOUNDARIES3D.html
#
# alle Koordinatensysteme im LV95 (bzw. in dieses konvertiert)
#
##################################################

time1 <- Sys.time()

# Pakete laden
library(sp)
library(parallel)
library(readr)
library(dplyr)
library(raster)
library(rgdal)
library(maptools)
library(ggplot2)
library(png)

options(java.parameters = "-Xmx200g") 
options(scipen=500)

# proj4string=CRS("+init=epsg:2056")

########################################################
cat("Hektarrasterdaten einlesen\n")
########################################################
dfhh <- as.data.frame(read_csv("STATPOP2013G.csv"))

# RELI gibt die linke untere Ecke der HR-Zelle an. 
# Daher Zentroid der HR-Zelle = x+50m und y+50m
# (ändert auch an der RELI nichts, tip-top)
dfhh$X_KOORD <- dfhh$X_KOORD+50
dfhh$Y_KOORD <- dfhh$Y_KOORD+50

# HR-Zellen im LV03-Koordinatensystem (RELI, X, Y) in LV95 umwandeln (der
# Einfachheit halber via SpatialPointsDataFrame)
spreli <- SpatialPointsDataFrame(coords=dplyr::select(dfhh, c(X_KOORD, Y_KOORD)),
																 data=dfhh, 
																 match.ID=TRUE, 
																 proj4string = CRS("+init=epsg:21781"))

# Ins LV95 projizieren. Goht s bitz...
sprelilv95 <- spTransform(spreli, CRS("+init=epsg:2056")) 

########################################################
cat("Landesgrenzen einlesen (Koordinatensystem LV95)\n")
########################################################
sfch <- readShapeSpatial("swissBOUNDARIES3D_1_2_TLM_LANDESGEBIET" , proj4string <- CRS("+init=epsg:2056"))

# nur den Umriss behalten
sf1 <- unlist(unlist(sfch@polygons[[1]]))

# laut dem Polygon ist die Schweiz eine Insel (sehr lustig :-), d.h. alle
# HR-Zellen, die innerhalb des Polygons liegen, haben Abstand null zur Insel
# (Fläche gehört mit dazu, nicht nur das Grenzpolygon). ergo: aus der Insel ein Loch machen.
# sf2 <- Polygon(sf1@Polygons[[1]], hole=FALSE)
# sf21 <- Polygon(sf1@Polygons[[1]], hole=TRUE)
# # daraus wieder SpatialPolygon erstellen
# sf3 <- Polygons(list(sf2,sf21), ID="CH-Umriss")
# spch <- SpatialPolygons(list(sf3), proj4string = CRS("+init=epsg:2056"))
# # Okay, jetzt könnte man das mit Polygonen berechen. Geht aber irgendwie nicht.
# sfss <- sfhh_plus_reli[10000:10500,]
# d1 <- gDistance(sfss, spch, byid=TRUE)

# Na dann halt nich. Alternative: Punkte des Grenzpolygons verwenden und die
# Distanzen von den HR-Zellen zu den Grenzpolygonpunkten berechnen. Spass.

# Erstmal die Punkte im LV95 als Shape anlegen. Man weiss nie. Dazu die
# Koordinaten der Polygonpunkte von weiter oben verwenden.
sfchp <- SpatialPoints(sf1@Polygons[[1]]@coords, 
											 proj4string = CRS("+init=epsg:2056"))

# Aus den Shapes nur die Punkte rausholen zur Distanzberechnung. Ist ja eh
# alles in Meter, sehr praktisch, was sich das Militär damals ausgedacht hat.
chpoints <- sfchp@coords
relipoints <- data.frame(X_KOORD=sprelilv95@coords[,1],
												Y_KOORD=sprelilv95@coords[,2],
												 RELI=sprelilv95@data$RELI)

# Bei Bedarf zum Testen mal nicht alle Punkte nehmen.
#chp1 <- chpoints[1:100,]
#relip1 <- relipoints[1:10,]

# Hmm, alle 340k HR-Zellen und dann die Abstände zu allen 40k Grenzpunkten... 
# Öhm, das könnte länger gehn. Aber Optimierung eh sinnlos hier in dem Laden.

# Funktion zum Retournieren der minimalen Distanz zwischen einem Punkt und
# einer Menge von Punkten, kann so dann mittels parApply parallelisiert werden
getmindist <- function(pt, pts){

				# Grenzpolygonpunkte
				pts <- as.matrix(pts[,c("coords.x1", "coords.x2")])

				# Distanzen von einem Punkt zu den Grenzpolygonpunkten
				distances <- spDistsN1(pts = pts,
					 pt = as.numeric(pt[c("X_KOORD", "Y_KOORD")]),
								 longlat=FALSE)
				mindist = round(min(distances), digits=0)
				ptsdist <- data.frame(
													  X_KOORD = pt[1],
														Y_KOORD = pt[2],
														RELI = pt[3],
														grenzentfernung = mindist
														)
				return(ptsdist)
}

time1 <- Sys.time()

################################################
cat("mit Work-Cluster parallelisiert rechnen\n")
################################################

cat("Cluster anlegen\n")
cl <- makeCluster(14)
cat("Libraries auf Clusternodes laden\n")
packs <- clusterEvalQ(cl, library("sp"))

time2 <- Sys.time()

# Für die Funktion benötigte Daten auf Cluster exportieren
clusterExport(cl, varlist=c("chpoints"))
cat("jetzt parallelisiert rechnen...\n")
t3 <- parApply(cl=cl, X=relipoints, MARGIN=1, FUN=getmindist, pts=chpoints)

# Liste in data frame umwandeln
dists <- as.data.frame(t(sapply(t3, unlist)))

time3 <- Sys.time()

cat("Cluster schliessen\n")
stopCluster(cl)

# Rechnet etwa 14 Minuten. Nicht übel.
print(paste("Laufzeit Berechnung:", round(difftime(time3, time2, unit="mins")), "Minuten"))

########################################
cat("Bevölkerungsdaten joinen und LV03 raus, nur LV95 drinlassen...\n")
########################################

dfhh$RELI <- as.factor(dfhh$RELI)
dists$RELI <- as.factor(dists$RELI)
statpop2013plusdist <- dfhh %>%
	inner_join(dists, by="RELI") %>%
	mutate(X_KOORD=X_KOORD.y,
				 Y_KOORD=Y_KOORD.y,
				 X_KOORD.x=NULL,
				 Y_KOORD.x=NULL,
				 X_KOORD.y=NULL,
				 Y_KOORD.y=NULL)

#erstmal als CSV abspeichern
write.csv2(file="statpop2013plusdist.csv", 
					 statpop2013plusdist,
					 row.names=FALSE)

# So, jetzt noch Punkt-Shapes fürs GIS anlegen und rausschreiben
sfstatpop2013 <- SpatialPointsDataFrame(coords=statpop2013plusdist[c("X_KOORD","Y_KOORD")],
																data=statpop2013plusdist,
											 proj4string = CRS("+init=epsg:2056"))

shapefile(sfstatpop2013, filename="statpop2013plusdist", overwrite=TRUE) 

time4 <- Sys.time()
print(paste("Laufzeit Gesamtskript:", round(difftime(time4, time1, unit="mins")), "Minuten"))


if(FALSE){

source("theme-plots.R")
# Berechnungen Grenznaehe pro Entfernungsgruppe
bevproentf <- statpop2013plusdist %>%
	mutate(entfkm = grenzentfernung/1000,
				 entfgruppe = cut(entfkm, breaks=seq(from=0,to=70,by=5), dig.lab=2)
				 ) %>%
	group_by(entfgruppe) %>%
	summarise(bevsum = sum(B13BTOT)) %>%
	mutate(entfgruppepercent = bevsum/sum(bevsum),
				 entfperccum = cumsum(entfgruppepercent))

png(file="plot-grenzentf-bev.png", width=800,height=600)

p1 <- ggplot(bevproentf) +
	geom_bar(aes(x=entfgruppe,y=bevsum, group=1), colour="black", stat="identity", fill="grey", alpha=0.9) +
	theme_pngbig() +
	theme(axis.text.x = element_text(angle = 90, hjust = 0.5       )) +
	theme(axis.text.y = element_text(angle = 90, hjust = 0.5       )) +
	labs(x="Grenzabstand [km]",
			 y="Anzahl Personen",
			 title="Bevölkerung in Grenznähe"
			 )
print(p1)
dev.off()

png(file="plot-grenzentf-bev-prozent.png", width=800,height=600)

p1 <- ggplot(bevproentf) +
	geom_bar(aes(x=entfgruppe,y=entfgruppepercent, group=1), colour="black", stat="identity", fill="grey", alpha=0.9) +
	geom_line(aes(x=entfgruppe, y=entfperccum, group=1), colour="red", alpha=0.8) +
	theme_pngbig() +
	theme(axis.text.x = element_text(angle = 90, hjust = 0.5       )) +
	theme(axis.text.y = element_text(angle = 90, hjust = 0.5       )) +
	labs(x="Grenzabstand [km]",
			 y="Anteil an Gesamtbevölkerung",
			 title="Bevölkerung in Grenznähe anteilig/kumuliert"
			 )
print(p1)
dev.off()

}
