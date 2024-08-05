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

# Create a grid for aggregating data.
grid <- make_grid_origin_res(
    xy_origin = c(-45.886944, -23.178889),
    xy_min = c(-46, -24),
    xy_max = c(-45, -22),
    cell_size = grid_resolution,
    crs = 4326,
    id_col = "id"
)



#---- Validation ----

stopifnot("Input directory not found!" = file.exists(in_dir))
stopifnot("Output directory not found!" = file.exists(out_dir))



#---- Utilitary functions ----

# Aggregate TROPOMI data into a raster.
agg_tropomi <- function(file_path, vars, out_crs, grid, grid_resolution) {

    # Export TROPOMI to points.
    grid_bbox <- sf::st_bbox(grid)
    data_sf_ls <- tropomiday2sf(
        fname = file_path,
        vars = vars,
        out_crs = out_crs,
        min_x = grid_bbox["xmin"],
        min_y = grid_bbox["ymin"],
        max_x = grid_bbox["xmax"],
        max_y = grid_bbox["ymax"]
    )

    # Aggregate points into a vector grid.
    data_sf_ls <- lapply(
        data_sf_ls,
        function(x) {aggregate(x = x, by = grid, FUN = "mean")}
    )

    # Convert to raster.
    data_r_ls <- lapply(
        names(data_sf_ls),
        function(name, data_sf_ls, res) {
            return(grid_to_raster(grid_sf = data_sf_ls[[name]],
                grid_resolution = res, cname = name))
        },
        data_sf_ls = data_sf_ls, res = grid_resolution
    )
    names(data_r_ls) <- names(data_sf_ls)

    return(data_r_ls)

}



# Write a list of raster to disc
#
# @description
# Helper function. Write a list of terra::rast to disc.
#
# @param raster_ls a list of terra::rast objects.
# @param base_fname a character(1). A file path template used to build file
#   names.
#
# @return a character. File paths.
#
save_raster <- function(raster_ls, base_fname) {
    fnames <- character(0)
    for (name in names(raster_ls)) {
        r <- raster_ls[[name]]
        out_f <- paste0(base_fname, "_", paste0(name, ".tif"))
        terra::writeRaster(r, filename = out_f)
        fnames <- append(fnames, out_f)
    }
    return(fnames)
}



#----- Export TROPOMI to GeoTIF ----


# Export TROPOMI to raster.
files_df <-
    in_dir %>%
    list.files(pattern = TROPOMI.DAILY.FILE.PATTERN,
        recursive = FALSE,
        full.names = TRUE) %>%
    tibble::as_tibble() %>%
    dplyr::rename(file_path = "value") %>%
    dplyr::mutate(file_name = basename(file_path)) %>%
    tidyr::separate_wider_delim(
        cols = file_name, 
        delim = "_", 
        names = c("mission", "type", "date")
    ) %>%
    dplyr::mutate(date = lubridate::as_date(date)) %>%
    # Export TROPOMI to raster
    dplyr::mutate(raster_ls = purrr::map(
        file_path, 
# TODO: Remove call to possibly
        .f = purrr::possibly(agg_tropomi, NULL),
        vars = vars,
        out_crs = "epsg:4326",
        grid = grid,
        grid_resolution = grid_resolution
    ))

# TODO: Remove this block of code once the errors are found!
# Examine NULL
files_df %>%
    dplyr::select(file_path, raster_ls) %>%
    dplyr::mutate(is_null = purrr::map_lgl(raster_ls, is.null)) %>%
    dplyr::filter(is_null)
stop("Check for NULL before moving on")

files_df <-
    files_df %>%
    # Save rasters.
    dplyr::mutate(
        base_fname = file.path(out_dir,
            tools::file_path_sans_ext(basename(file_path))),
        tropo_r = purrr::map2(
            .x = raster_ls,
            .y = base_fname,
            .f = save_raster
        )
    ) %>%
    dplyr::select(-raster_ls, -base_fname) %>%
    tidyr::unnest(tropo_r)

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

