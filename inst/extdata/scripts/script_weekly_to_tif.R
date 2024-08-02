###############################################################################
# EXPORT TROPOMI DATA (HDF5, weekly observations) TO GEOTIF.
# alber.ipia@inpe.br
# Last update: 2024-04-11
#------------------------------------------------------------------------------
## Install required packages.
#install.packages(c("terra", "BiocManager"))
#BiocManager::install("rhdf5")
###############################################################################



library(rhdf5)
library(terra)

library(devtools)
devtools::load_all()



#---- Configuration ----

# Path to the TROPOMI HDF5 file.
fname <- "/home/alber/Documents/github/treeslabtropomi/inst/extdata/other/TROPOMI-SIF740nm_01-2018--12-2018_0_0833333deg_8-daily.nc"

# Variables to read from the HDF5 file.
# NOTE: lon & lat should always be the first two variables!
vars <- c("lon", "lat", "sif")

# Directory for storing GeoTif files.
out_dir <- "/home/alber/Downloads/tmp"



#---- Validation ----

stopifnot("File not found!" = file.exists(fname))
stopifnot("Output directory not found!" = dir.exists(out_dir))



#---- Utilitary functions ----




#----- Export TROPOMI to GeoTIF ----

tropomi8day2tif(fname, vars, out_dir)

