###############################################################################
# COMPUTE MONTHLY AVERGAGE TROPOMI FROM THE DAILY READINGS
# alber.ipia@inpe.br
# Last update: 2024-07-30
#------------------------------------------------------------------------------
## Install required packages.
#install.packages(c("terra", "BiocManager"))
#BiocManager::install("rhdf5")
###############################################################################


library(dplyr)
library(lubridate)
library(rhdf5)
library(purrr)
library(terra)
library(tidyr)

library(devtools)
devtools::load_all()



#---- Configuration ----

# Path to the TROPOMI daily files.
in_dir <- 
    "/home/alber/Documents/github/treeslabtropomi/inst/extdata/SIF_TROPOMI"

# Directory for storing GeoTif files.
out_dir <- "/home/alber/Downloads/tmp"

# Variables to read from the files.
# NOTE: lon & lat should always be the first two variables!
vars <- c("longitude", "latitude", "SIF_735", "SIF_743")

grid_resolution <- 0.083333
grid_resolution <- 1

# Create a grid for aggregating data.
grid <- make_grid_origin_res(
    xy_origin = c(-74, -17),
    xy_min = c(-74, -17),
    xy_max = c(-43, -6),
    cell_size = grid_resolution,
    crs = 4326,
    id_col = "id"
)

on.exit({
    rhdf5::h5closeAll()
}, add = TRUE)

#---- Validation ----

stopifnot("Input directory not found!" = file.exists(in_dir))
stopifnot("Output directory not found!" = file.exists(out_dir))



#---- Utilitary functions ----

# Export a grid to rasters
#
# @description
# Helper. Export each column of the given grid (an sf object) to a raster
# (terra object).
#
# @param grid an sf object (polygon). A grid with attributes.
# @param vars a character. Names of columns in the given grid.
# @param grid_resolution a numeric. Resolution for rasterizing the grid.
#
# @return a list of terra::rast.
#
grid_to_rasters <- function(grid, vars, grid_resolution) {

    stopifnot("Variables not found in grid!" = all(vars %in% colnames(grid)))

    rast_ls <- lapply(
        vars,
        function(v, grid, grid_resolution) {
            grid_to_raster(
                grid_sf = grid,
                grid_resolution = grid_resolution,
                cname = v
            )
        },
        grid = grid, grid_resolution = grid_resolution
    )

    names(rast_ls) <- vars
    return(rast_ls)
}



#---- List files ----

files_df <-
    in_dir %>%
    list.files(pattern = TROPOMI.DAILY.FILE.PATTERN,
        recursive = FALSE,
        full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = "value")



#----- Find & report broken files ----

files_df <- 
    files_df %>%
    dplyr::mutate(
        is_null = purrr::map(file_path, purrr::possibly(rhdf5::h5ls)),
        is_null = purrr::map_lgl(is_null, is.null)
    )

rhdf5::h5closeAll()

broken_files <-
    files_df %>%
    dplyr::filter(is_null == TRUE)

if (nrow(broken_files) > 0) {
    print("===============================================================")
    print("NOTE: Broken files found!")
    print("---------------------------------------------------------------")
    print(basename(broken_files$file_path))
    print("---------------------------------------------------------------")
}



#----- Export TROPOMI to GeoTIF ----



# Export TROPOMI to raster.

files_df <-
    files_df %>%
    # Ignore invalid files.
    dplyr::filter(is_null == FALSE) %>%
    dplyr::select(-is_null) %>%
    dplyr::mutate(file_name = basename(file_path)) %>%
    tidyr::separate_wider_delim(
        cols = file_name, 
        delim = "_", 
        names = c("mission", "type", "date")
    ) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    dplyr::mutate(
        grid = purrr::map(
            file_path,
            tropomiday2grid,
            vars = vars,
            grid = grid,
            f = "mean"
        )
    ) %>%
    dplyr::mutate(
        rast_ls = purrr::map(
            grid,
            grid_to_rasters,
            vars = vars[3:length(vars)],
            grid_resolution = grid_resolution
        )
    ) %>%
    dplyr::select(-grid) %>%
    tidyr::unnest(rast_ls) %>%
    dplyr::mutate(var_name = names(rast_ls)) %>%
    dplyr::mutate(
        tropo_r = file.path(
            out_dir,
            paste0(tools::file_path_sans_ext(basename(file_path)), 
                   "_", var_name, ".tif")
        )
    ) %>%
    dplyr::mutate(
        tropo_r = purrr::map2_chr(rast_ls, tropo_r,
            function(r, fname) {
                terra::writeRaster(
                    x = r,
                    filename = fname
                )
                return(fname)
            }
        )
    )



# Compute the monthly mean.
monthly_mean_df <-
    files_df %>%
    dplyr::select(tropo_r) %>%
    dplyr::mutate(file_name = tools::file_path_sans_ext(basename(tropo_r))) %>%
    tidyr::separate_wider_delim(
        cols = file_name, 
        delim = "_", 
        names = c("mission", "type", "date", "sensor", "wavelength")
    ) %>%
    dplyr::mutate(d_year = lubridate::year(date),
                  d_month = lubridate::month(date)) %>%
    dplyr::select(-date) %>%
    dplyr::group_by(mission, type, sensor, wavelength, d_year, d_month) %>%
    tidyr::nest(.key = "files_df") %>%
    dplyr::mutate(mean_r = purrr::map(files_df, function(x) {
        return(mean(terra::rast(x[["tropo_r"]]), na.rm = TRUE))
    })) %>%
    tidyr::unite(col = file_path, mission, type,
                 sensor, wavelength, d_year, d_month) %>%
    dplyr::mutate(
        file_path = paste0(file_path, ".tif"),
        file_path = file.path(out_dir, file_path)
    ) %>%
    dplyr::mutate(out_file = purrr::map2_chr(
        mean_r, file_path, function(mean_r, file_path) {
            terra::writeRaster(x = mean_r, filename = file_path)
            return(file_path)
        }
    )) %>%
    dplyr::select(-file_path)

