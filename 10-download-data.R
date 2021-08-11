##############################
# Herunterladen von offiziellen PLZ-Geodaten von Swisstopo
# Zweck: Berechnung von Distanzen zwischen PLZ
#
# github@georgruss.ch, 2021-08-06
##############################

if(!(dir.exists("cache"))) dir.create("cache")
if(!(dir.exists("plz"))) dir.create("plz")
if(!(dir.exists("fig"))) dir.create("fig")

# offizielle Daten siehe Geokatalog hier:
# https://data.geo.admin.ch/ch.swisstopo-vd.ortschaftenverzeichnis_plz/

# Metadaten laden
download.file("https://data.geo.admin.ch/ch.swisstopo-vd.ortschaftenverzeichnis_plz/data.zip", destfile = "cache/data.zip")
unzip("cache/data.zip", exdir="cache/")

# im cache/readme.txt-File steht dann der URL zum PLZ-File (Deutsch, LV95)
# https://data.geo.admin.ch/ch.swisstopo-vd.ortschaftenverzeichnis_plz/PLZO_INTERLIS_LV95.zip

# interlis
# download.file("https://data.geo.admin.ch/ch.swisstopo-vd.ortschaftenverzeichnis_plz/PLZO_INTERLIS_LV95.zip", destfile="plz/PLZO.zip")

# geodatabase
download.file("https://data.geo.admin.ch/ch.swisstopo-vd.ortschaftenverzeichnis_plz/PLZO_GDB_LV95.zip", destfile="plz/PLZO.zip")
unzip("plz/PLZO.zip", exdir="plz/")

