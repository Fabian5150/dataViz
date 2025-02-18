library(ggplot2)

# Take a look at all datasets. If needed merge multiple ones
# Create nice visualisations to compare size and distance to earth
# Maybe only take planets with discovery method: transit => more reliable data ?

# from http://exoplanets.org/table
exo <- read.csv("gitProjects/uni/dataviz/r/playground/data/RIAZFPNI.csv", header=TRUE)

# from https://exoplanetarchive.ipac.caltech.edu/cgi-bin/TblView/nph-tblView?app=ExoTbls&config=PS&constraint=default_flag=1&constraint=disc_facility+like+%27%25TESS%25%27
# (confirmed planets only)
nasa <- read.csv(
  "gitProjects/uni/dataviz/r/playground/data/PS_2025.02.18_01.36.05.csv",
  comment.char="#", stringsAsFactors=FALSE
)

