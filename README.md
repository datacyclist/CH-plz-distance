# CH-plz-distance
Abstandsberechnung zwischen Postleitzahlgebieten der Schweiz

# Mögliche paarweise Distanzberechnungen:

- Abstand in m zwischen Mitte der Bounding Box zweier PLZ-Polygone
- Abstand in m zwischen den Zentroiden zweier PLZ-Polygone
- Minimaler Abstand in m zwischen den PLZ-Polygonen selbst 
(d.h. direkt benachbarte Polygone haben Abstand null)

# Beispielausgabe in 12-distanz.R

1007 (PLZ von Lausanne)
8001 (PLZ von Zürich)
176759.2 (1. Distanzberechnung)
177106.9 (2. Distanzberechnung)
173047.8 (3. Distanzberechnung)
