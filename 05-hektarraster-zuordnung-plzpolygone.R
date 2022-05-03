##################################################
# - Hektarrasterzellen in Polygone umwandeln
# - Hektarrasterzellen den PLZ-Polygonen zuordnen
# 
# Georg Russ, 2015-09-04
##################################################

# Pakete laden
# Bei Bedarf erst installieren
# library(devtools)
# install_github("tmap", username="mtennekes", subdir="pkg")
library(rgeos)
library(rgdal)
library(readr)
library(maptools)
library(sp)
library(plyr)
library(dplyr)
library(png)
library(cleangeo) # zum Bearbeiten/Bereinigen von (kaputten) Geodaten
library(parallel)
library(RColorBrewer)
library(raster)

# Work-Directory setzen
dir <- "."
setwd(dir)

# alle Koordinatensysteme im LV95 bitte, Projektion dafür:
# proj4string=CRS("+init=epsg:2056")

# Das Shape-File hat Umlaute und so Zeug, daher Locale entsprechend setzen,
# sonst meckert es. Wie ne Ziege, hehe...
Sys.setlocale(category = "LC_ALL", locale = "C")

########################################
# Postleitzahlgebietsumrisse einlesen
########################################
sfplz <- readShapePoly("shp/PLZO_PLZ", proj4string=CRS("+init=epsg:2056"))

##################################################
# Daten bereinigen
# http://gis.stackexchange.com/questions/113964/fixing-orphaned-holes-in-r
##################################################
# report <- clgeo_CollectionReport(sfplz)
# summary <- clgeo_SummaryReport(report)
# issues <- report[report$valid == FALSE,]
# nv <- clgeo_SuspiciousFeatures(report)
# mysp <- sfplz[nv[-14],]
# mysp.clean <- clgeo_Clean(mysp, print.log = TRUE)

sfplz <- clgeo_Clean(sfplz, print.log=TRUE)

########################################################
# Hektarrasterdaten laden und Polygone daraus erstellen
########################################################

dfhr <- read_csv(file="csv/STATPOP2013G.csv")

# Funktion ist in 10-function-hektarraster-punkt-zu-polygon.R definiert
source("10-function-hektarraster-punkt-zu-polygon.R")

# Hektarrasterpunkte in Polygone umwandeln
sfhh <- hr_to_shape(df=dfhr,
									 relifield = "RELI", xfield = "X_KOORD", yfield = "Y_KOORD",
									 CRSin = "+init=epsg:21781", CRSout = "+init=epsg:2056"
									 )
# als Shapefile abspeichern (man weiss nie)
shapefile(sfhh, filename="shp/statpop2013polygone", overwrite=TRUE) 

# Funktion zum später parallelisierten Berechnen der räumlichen Schnittmengen
get_ha_plz <- function(plz, sfplz, sfhh){
	plz <- plz[5] # schlechter Stil, über Index die PLZ anzusprechen. Hmmm. Bei Bedarf hier den Fehler suchen.
	plzindex <- which(sfplz@data$PLZ==plz)

	# spatial intersection berechnen
	t2 <- over(sfhh, sfplz[plzindex,])

	# Wenn die Schnittmenge nicht leer ist, berechnete Daten zurückgeben, sonst
	# einen leeren Datendummy.

	if(sum(!(is.na(t2))>0)){
		# shape der HA-Zellen subsetten
		sfhh1 <- sfhh[!(is.na(t2)),]
		# nur Daten nehmen und RELI mit PLZ verknüpfen
		hhz <- sfhh1@data
		hhz$RELI <- rownames(hhz)
		hhz$PLZ <- plz
		return(hhz)
		} else {
	  hhz <- sfhh@data
		hhz$RELI <- NA
		hhz$PLZ <- NA
		hhz <- hhz[0,]
		return(hhz)
	}
}


# Sampling der HA-Zellen zum Testen bei Bedarf
# samples <- sample(x=c(1:dim(sfhh)[1]), size=100000, replace=FALSE)
# sfhhs  <- sfhh[samples,]
sfhhs <- sfhh # alle Daten nehmen

# fuer welche PLZs samplen?
#dfplzs <- sfplz@data[c(1:100),]
dfplzs <- sfplz@data # fuer alle...

# parallelisiert berechnen
cat("Cluster anlegen\n")
cl <- makeCluster(14)
cat("Libraries auf Clusternodes laden\n")
# für den Schnitt von Polygon mit Polygon muss auch explizit rgeos geladen werden
packs <- clusterEvalQ(cl, expr={
											library("sp")
											library("rgeos")
									 })
cat("Daten auf Cluster exportieren\n")
clusterExport(cl, varlist=c("sfhhs", "sfplz"))

# Jetzt wird (ab)gerechnet:
t4 <- parApply(cl=cl, X=dfplzs, MARGIN=1, FUN=get_ha_plz, sfhh=sfhhs, sfplz=sfplz)

cat("Cluster schliessen\n")
stopCluster(cl)

# Ausgabe der Parallelbearbeitung zusammenfügen
hhaplzreli <- ldply(t4)[,-1]

# Daten fuer den Export, Zuordnung RELI zu PLZ und Anteil der Zuordnung an PLZ,
# dazu noch als Beispiel die Berechnung der gewichteten Bevölkerung pro HR-Zelle und PLZ
# sum(B13BTOT) > sum(B13BTOTgew)
reli_plz_faktor <- hhaplzreli %>%
	group_by(RELI) %>%
	dplyr::mutate(anteilrelizuplz= 1/n()) %>%
	ungroup() %>%
	dplyr::mutate(RELI=as.numeric(RELI),
								PLZ=as.numeric(PLZ),
								B13BTOTgew = B13BTOT*anteilrelizuplz) %>%
	dplyr::select(RELI, PLZ, anteilrelizuplz,B13BTOT, B13BTOTgew) 

write.csv2(x=as.data.frame(reli_plz_faktor), file="reli-plz-anteil.csv",
					 row.names=FALSE)



# Bei Bedarf: Grafiken mit tmap-Package erstellen.
# library(tmap)

# ######################################################################
# cat("Plot: Zuerich Kreis 1, PLZ 8001")
# ######################################################################
#
#sf_plz8001 <- subset(sfplz, PLZ==8001)
#hrz_reli_8001 <- subset(hhaplzreli, PLZ=="8001")
#sf_hr_8001 <- subset(sfhh_plus_reli, sfhh_plus_reli@data$RELI %in% hrz_reli_8001$RELI)
#
## palette <- colorRampPalette(brewer.pal(9,"Blues"))(100)
## jetzt mal Karten plotten
#
#png(file="figures/20150625-hr-plz-8001.png", width=1100,height=800)
#
#map1 <- 
#	tm_shape(sf_plz8001) +
#		tm_borders() +
#		tm_fill(col="grey90") +
#		tm_text("PLZ", size=1.8, fontcolor="dodgerblue", fontfamily="serif") +
#		#palette="dodgerblue3"
#	tm_shape(sf_hr_8001) +
#		tm_fill("PTOT", alpha=0.80, palette="YlOrRd") +
#		tm_layout(title="Zuordnung HR-Zellen zu PLZ-Gebiet 8001 (Kreis 1)",
#							title.position = c("left", "top"), 
#							legend.position = c("left", "top"), 
#							inner.margins = c(0, 0.1, 0, 0),
#							bg.color="grey95"
#							) +
#		tm_credits(text="BfS-Haushalte pro HR-Zelle 2013", size=2, position=c("left", "bottom"))
#print(map1)
#dev.off()
#
#######################################################################
#cat("Plot: Zuerich Kreis 4/5, PLZ 8004/8005\n")
#######################################################################
#
#sf_plz_subset <- subset(sfplz, PLZ %in% c(8005, 8004))
#hrz_reli_subset <- subset(hhaplzreli, PLZ %in% c("8005", "8004"))
#sf_hr_subset <- subset(sfhh_plus_reli, sfhh_plus_reli@data$RELI %in% hrz_reli_subset$RELI)
#
## palette <- colorRampPalette(brewer.pal(9,"Blues"))(100)
## jetzt mal Karten plotten
#
#png(file="figures/20150625-hr-plz-8005-8004.png", width=1100,height=800)
#
#map1 <- 
#	tm_shape(sf_plz_subset) +
#		tm_borders() +
#		tm_fill(col="grey90", title="Genossenschaftsgrenzen") +
#		#palette="dodgerblue3"
#	tm_shape(sf_hr_subset) +
#		tm_fill("PTOT", alpha=0.85, palette="YlOrRd") +
#		tm_layout(title="Zuordnung HR-Zellen zu PLZ-Gebiet 8004/8005 (Kreis 4/5)",
#							title.position = c("right", "top"), 
#							legend.position = c("left", "top"), 
#							inner.margins = c(0, 0.1, 0, 0),
#							bg.color="grey95"
#							) +
#		tm_credits(text="BfS-Haushalte pro HR-Zelle 2013", size=2, position=c("left", "bottom"))
#print(map1)
#dev.off()
#
#######################################################################
#cat("Plot: Zuerich Kreis 1,4,5, PLZ 8001/8004/8005\n")
#######################################################################
#
#sf_plz_subset <- subset(sfplz, PLZ %in% c(8001, 8005, 8004))
#hrz_reli_subset <- subset(hhaplzreli, PLZ %in% c("8001", "8005", "8004"))
#sf_hr_subset <- subset(sfhh_plus_reli, sfhh_plus_reli@data$RELI %in% hrz_reli_subset$RELI)
#
## palette <- colorRampPalette(brewer.pal(9,"Blues"))(100)
## jetzt mal Karten plotten
#
#png(file="figures/20150625-hr-plz-8001-8004-8005.png", width=1100,height=800)
#
#map1 <- 
#	tm_shape(sf_plz_subset) +
#		tm_borders() +
#		tm_fill(col="grey90") +
#		#palette="dodgerblue3"
#		tm_text("PLZ", size=1.8, fontcolor="dodgerblue", fontfamily="serif") +
#	tm_shape(sf_hr_subset) +
#		tm_fill("PTOT", alpha=0.80, palette="YlOrRd") +
#		tm_layout(title="Zuordnung HR-Zellen zu PLZ-Gebiet 8001/8004/8005 (Kreis 1/4/5)",
#							title.position = c("right", "top"), 
#							legend.position = c("left", "top"), 
#							inner.margins = c(0, 0.1, 0, 0),
#							bg.color="grey95"
#							) +
#		tm_credits(text="BfS-Haushalte pro HR-Zelle 2013", size=2, position=c("left", "bottom"))
#print(map1)
#dev.off()
