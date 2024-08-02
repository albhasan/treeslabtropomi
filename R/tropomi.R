#' Read data from a HDF5 file
#'
#' @description
#' Read the x variable from the given HDF5 file.
#' 
#' @param v     a character(1). Variable's name.
#' @param fname a character(1). Path to a HDF5 file.
#' @param group a character(1). Name of the variable's group.
#'
#' @return      an array.
#'
read_var <- function(v, fname, group) {
    g <- rhdf5::H5Gopen(
        h5loc = rhdf5::H5Fopen(name = fname, native = TRUE),
        name = group)
    d <- rhdf5::h5read(file = g, name = v)
    return(d)
}


#' Export from TROPOMI to GeoTIF
#'
#' @description
#' Export the given HDF5 TROPOMI file as points (sf object).
#'
#' @param fname a character(1). Path to an HDF5 file.
#' @param vars a character. Names of the varibles to export. Note that the
#'   first two elements must be the names of the variables longitude and
#'   latitude in that order.
#' @param out_crs a spatial reference system for the output tif files.
#' @param min_x,min_y,max_x,max_y a numeric(1). Return the points faling in
#'   these ranges.
#'
#' @return a list of sf objects (point).
#'
#' @export
#'
tropomi2sf <- function(fname, vars, out_crs = "epsg:4326",
                       min_x = -Inf, min_y = -Inf,
                       max_x = Inf,  max_y = Inf) {

    stopifnot("Input file is missing!" = file.exists(fname))
    stopifnot("At least 3 variables are expected!" = length(vars) > 2)

    h5_df <- rhdf5::h5ls(fname)
    h5_df <- h5_df[h5_df["otype"] == "H5I_DATASET", ]
    h5vars <- h5_df[["name"]]
    if (!all(vars %in% h5vars))
        stop(sprintf("I cannot find the varialbe(s): %s. The HDF variables are:
        %s.",  setdiff(vars, h5vars), paste(h5vars, collapse = ",")))

    h5_df <- h5_df[h5_df[["name"]] %in% vars, ]
    h5_df["fname"] <- fname

    # Read data.
    data_ls <- lapply(seq(nrow(h5_df)), function(r, h5_df) {
        read_var(
            v = h5_df[r, "name"],
            fname = h5_df[r, "fname"],
            group = h5_df[r, "group"]
        )
    }, h5_df = h5_df)
    names(data_ls) <- h5_df[["name"]]

    # Build name for output file.
    out_fname_base <- basename(tools::file_path_sans_ext(fname))

    # Loop varialbes in the given file.
    # NOTE: The first 2 variables must be longitude and latitude.
    sf_ls <- list()
    for (v in names(data_ls)) {

        if (v %in% vars[1:2]) next

        vdata <- data_ls[[v]]

        if (length(dim(vdata)) == 1) {
            data_df <- as.data.frame(cbind(data_ls[[vars[1]]],
                                           data_ls[[vars[2]]],
                                           vdata))
            colnames(data_df) <- c(vars[1:2], v)
        } else if (lenght(dim(vdata)) == 3) {
            stop("3D case not implemented!")
        } else if (lenght(dim(vdata)) == 4) {
            stop("4D case not implemented!")
        } else{
            stop("Unexpected number of dimensions!")
        }

        if (!is.infinite(min_x))
            data_df <- data_df[data_df[[vars[1]]] >= min_x, ]
        if (!is.infinite(min_y))
            data_df <- data_df[data_df[[vars[2]]] >= min_y, ]
        if (!is.infinite(max_x))
            data_df <- data_df[data_df[[vars[1]]] <= max_x, ]
        if (!is.infinite(max_y))
            data_df <- data_df[data_df[[vars[2]]] <= max_y, ]

        sf_ls[[v]]<- sf::st_as_sf(data_df, coords = vars[1:2], crs = out_crs)

    }
    # TODO: I need to close the file connections!
    #rhdf5::H5Gclose()
    invisible(sf_ls)
}

