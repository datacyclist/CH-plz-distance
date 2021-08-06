##############################
# Einlesen von offiziellen PLZ-Geodaten von Swisstopo
# Zweck: Berechnung von Distanzen zwischen PLZ
#
# github@georgruss.ch, 2021-08-06
##############################

library(sp)
library(raster)
library(rgdal)
#library(sf)

#sPLZ <- readOGR(dsn="plz/PLZO_INTERLIS_LV95/PLZO_ITF_LV95.itf", verbose=TRUE)

ogrListLayers(dsn="plz/PLZO_GDB_LV95/PLZO_GDB_LV95.gdb")

#sPLZ <- readOGR(dsn="plz/PLZO_GDB_LV95/PLZO_GDB_LV95.gdb", verbose=TRUE, layer="PLZO_OSNAME")
sPLZ <- readOGR(dsn="plz/PLZO_GDB_LV95/PLZO_GDB_LV95.gdb", layer="PLZO_PLZ")
