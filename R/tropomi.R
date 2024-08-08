#' Read data from a HDF5 file
#'
#' @description
#' DEPRECATED. Read the x variable from the given HDF5 file.
#' 
#' @param v     a character(1). Variable's name.
#' @param fname a character(1). Path to a HDF5 file.
#' @param group a character(1). Name of the variable's group.
#'
#' @return      an array.
#'
read_var <- function(v, fname, group = NA) {

    stop("DEPRECATED. Open and close HDF5 files inside the same function!")
    if (is.na(group))
        return(rhdf5::h5read(file = fname, name = v))

    g <- rhdf5::H5Gopen(
        h5loc = rhdf5::H5Fopen(name = fname, native = TRUE),
        name = group)
    d <- rhdf5::h5read(file = g, name = v)
    return(d)
}



#' Aggregate TROPOMI daily data into a grid
#'
#' @description
#' Assume the give HDF5 file contains points and aggregate them using the given
#' grid. The HDF5 file contents are assumed to be ungridded TROPOMI SIF daily
#' observations.
#'
#' @param file_path a character(1). Path to an HDF5 file.
#' @param vars a character. Names of the varibles to export. Note that the
#'   first two elements must be the names of the variables longitude and
#'   latitude in that order.
#' @param grid an sf object. A grid used to aggregate the given data.
#' @param f a character(1). Name of an aggregation function.
#' @param in_na_value a numeric. Numeric value used to represente NAs in the 
#'   given file.
#'
#' @return an sf object. The given grid with additional columns.
#'
#' @seealso
#' Ungridded TROPOMI SIF (at 740nm) \url{https://doi.org/10.22002/D1.1347}
#'
#' @export
#'
tropomiday2grid <- function(file_path, vars, grid, f = "mean",
    in_na_value = 9969209968386869046778552952102584320) {

    stopifnot("Expected only one file!" = length(file_path) == 1)
    stopifnot("Input file is missing!" = file.exists(file_path))
    stopifnot("At least 3 variables are expected!" = length(vars) > 2)

    h5_df <- rhdf5::h5ls(file = file_path)
    h5_df <- h5_df[h5_df["otype"] == "H5I_DATASET", ]
    h5vars <- h5_df[["name"]]
    if (!all(vars %in% h5vars))
        stop(sprintf("I cannot find the varialbe(s): %s. The HDF variables are:
        %s.",  setdiff(vars, h5vars), paste(h5vars, collapse = ",")))

    h5_df <- h5_df[h5_df[["name"]] %in% vars, ]

    stopifnot("All variables must belong to the same group!" = 
        length(unique(h5_df[["group"]])) == 1)

    # Read data.
    h5g_conn <- rhdf5::H5Gopen(
        h5loc = rhdf5::H5Fopen(name = file_path,
                               native = TRUE,
                               flags = "H5F_ACC_RDONLY"),
        name = unique(h5_df[["group"]])
    )
    vars_ls <- list()
    for (v in vars) {
        vars_ls[[v]] <- rhdf5::h5read(file = h5g_conn, name = v)
        # Replace numeric NAs with actual NA.
        vars_ls[[v]] [vars_ls[[v]] == in_na_value] <- NA
    }
    stopifnot("The number of elements in each var must match!" = 
        length(unique(vapply(vars_ls, length, integer(1)))) == 1
    )

    # Filter by longitude and latitude.
    grid_bbox <- sf::st_bbox(grid)
    filter_bb <- rep(TRUE, times = unique(vapply(vars_ls, length, integer(1))))
    filter_bb <- filter_bb & (vars_ls[[vars[1]]] >= grid_bbox["xmin"])
    filter_bb <- filter_bb & (vars_ls[[vars[1]]] <= grid_bbox["xmax"])
    filter_bb <- filter_bb & (vars_ls[[vars[2]]] >= grid_bbox["ymin"])
    filter_bb <- filter_bb & (vars_ls[[vars[2]]] <= grid_bbox["ymax"])
    if (sum(filter_bb) == 0) {
        rhdf5::h5closeAll()
        warning("No observations found in the grid!")
        for (v in vars[3:length(vars)]) {
            grid[v] <- NA
        }
        return(grid)
    }
    vars_ls <- lapply(vars_ls, function(x) {x[filter_bb]})

    # Aggregate points into a vector grid.
    grid <- stats::aggregate(
        x = sf::st_as_sf(x = as.data.frame(vars_ls),
                         coords = vars[1:2],
                         crs = sf::st_crs(grid)),
        by = grid,
        FUN = f,
        na.rm = TRUE
    )

    rhdf5::h5closeAll()

    return(grid)

}



#' Export from TROPOMI to GeoTIF
#'
#' @description
#' Export the given HDF5 file to GeoTIF. The HDF5 contains TROPOMI SIF 8 day
#' gridded observations.
#'
#' @param fname a character(1). Path to an HDF5 file.
#' @param vars a character. Names of the varibles to export. Note that the
#'   first two elements must be the names of the variables longitude and
#'   latitude in that order.
#' @param out_dir a character(1). Path to a directory.
#' @param in_nodata a double(1). The NO DATA flag in the input file.
#' @param out_crs a character(1). The spatial reference system for the output
#'   files.
#'
#' @return a character. The path to the tif files.
#'
tropomi8day2tif <- function(fname, vars, out_dir, in_nodata = -999,
                            out_crs = "epsg:4326") {

    stopifnot("Input file doesn't exist!" = file.exists(fname))
    stopifnot("Only one file allowed!" = length(fname) == 1)
    stopifnot("Output directory not found!" = dir.exists(out_dir))
    stopifnot("At least 3 variables are expected!" = length(vars) > 2)

    h5_df <- rhdf5::h5ls(fname)
    h5_df <- h5_df[h5_df["otype"] == "H5I_DATASET", ]
    h5vars <- h5_df[["name"]]
    if (!all(vars %in% h5vars))
    stop(sprintf("I cannot find the varialbe(s): %s. The HDF variables are:
        %s.",  setdiff(vars, h5vars), paste(h5vars, collapse = ",")))

    h5_df <- h5_df[h5_df[["name"]] %in% vars, ]

    # Read data.
    vars_ls <- list()
    for (v in vars) {
        vars_ls[[v]] <- rhdf5::h5read(file = fname, name = v)
    }

    # Get extent.
    # NOTE: The coordinates correspond to pixel centers.
    x_size <- diff(vars_ls[[vars[1]]][1:2])
    y_size <- diff(vars_ls[[vars[2]]][1:2])
    x_min <- range(vars_ls[[vars[1]]])[1] - (x_size / 2)
    x_max <- range(vars_ls[[vars[1]]])[2] + (x_size / 2)
    y_min <- range(vars_ls[[vars[2]]])[1] - (y_size / 2)
    y_max <- range(vars_ls[[vars[2]]])[2] + (y_size / 2)

    # Build name for out file.
    out_fname_base <- basename(tools::file_path_sans_ext(fname))

    # Loop varialbes in the given file.
    # NOTE: The first 2 variables in vars are longitude and latitude.
    out_files <- character(0)
    for (v in vars[3:length(vars)]) {
        vdata <- vars_ls[[v]]
        stopifnot("Unexpected number of dimensions!" = length(dim(vdata)) == 3)

        # Loop the data in each variable.
        # NOTE: Assume the 1st dimension is time, the 2nd is longitude, and the
        #       3rd is latitude.
        for (tindex in seq(dim(vdata)[1])) {
            tdata <- vdata[tindex,,]

            if (all(tdata == in_nodata)) {
                warning("No data found for time index: ", tindex)
                next
            }

            out_f <- file.path(out_dir, paste0(out_fname_base, "_",
                sprintf("%03d", tindex), ".tif")
            )

            r <- terra::flip(terra::trans(terra::rast(tdata)))

            terra::crs(r)  <- out_crs
            terra::xmin(r) <- x_min
            terra::xmax(r) <- x_max
            terra::ymin(r) <- y_min
            terra::ymax(r) <- y_max

            terra::writeRaster(
                r,
                filename = out_f,
                wopt= list(gdal = c("COMPRESS=LZW"), datatype = "FLT4S",
                    NAflag = in_nodata,
                    names = paste(basename(fname), v, sprintf("%03d", tindex),
                        sep = "_")),
                overwrite = FALSE
            )
            out_files <- append(out_files, out_f)
        }
    }

    invisible(out_files)

}

