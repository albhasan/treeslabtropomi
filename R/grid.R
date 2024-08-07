#' Build a vector from the origin approximating the minimum and maximum values
#'
#' @description
#' Build a vector between the minimum and maximum ensuring that the given
#' origin value is part of the returned vector.
#'
#' @param o   a numeric(1). Origin.
#' @param min a numeric(1). Mininum value.
#' @param max a numeric(1). Maximum value.
#' @param res a numeric(1). Resultuion.
#'
#' @return    A numeric.
#'
grid_helper <- function(o, min, max, res) {
  sort(c(seq(from = o, to = max, by = res),
         seq(from = o, to = min, by = -res)[-1]))
}



#' Build a grid using a known point, a resolution, and a extent
#'
#' @description
#' `make_grid_origin_res` returns a grid using the coordinates of an origin
#'  point inside an area and a cell size. The new grid will have a vertex on
#'  the origin and will fit as many cells as possible in the area.
#'
#' @param xy_origin A numeric(2) representing an XY point.
#' @param xy_min,xy_max A pair of numeric vectors with the minimum and maximum
#'   coordiantes of the grid.
#' @param cell_size A numeric of length 1 or 2. The width and heigth o a cell.
#'   The units are those of the `crs`.
#' @param crs Numeric or character representing a Coordinate Reference System.
#' @param id_col character of length 1. Name of the column for identifying grid
#'   cells.
#'
#' @export
#'
make_grid_origin_res <- function(xy_origin,
                                 xy_min,
                                 xy_max,
                                 cell_size,
                                 crs = 4326,
                                 id_col = "id") {

    stopifnot("The origin must fall in the given ranges!" = all(
        xy_min[1] <= xy_origin[1], xy_origin[1] <= xy_max[1],
        xy_min[2] <= xy_origin[2], xy_origin[2] <= xy_max[2]
    ))
    stopifnot("`cell_sizse` is too large!" =
              all((xy_max - xy_min) >= cell_size))

    lon_grid <- grid_helper(o = xy_origin[1], min = xy_min[1], max = xy_max[1],
                            res = cell_size)
    lat_grid <- grid_helper(o = xy_origin[2], min = xy_min[2], max = xy_max[2],
                            res = cell_size)

    stopifnot("Not enough room to fit a grid!" =
        all(length(lon_grid) > 1, length(lat_grid) > 1))

    aoi_grid <- sf::st_as_sf(sf::st_make_grid(
        x = sf::st_bbox(c(xmin = min(lon_grid), xmax = max(lon_grid),
            ymin = min(lat_grid), ymax = max(lat_grid)),
            crs = sf::st_crs(crs)),
        cellsize = cell_size
    ))
    aoi_grid[id_col] <- seq(nrow(aoi_grid))

    return(aoi_grid)

}



#' Rasterize a grid
#'
#' @description
#' Transform a grid (sf object) into a raster (terra object).
#'
#' @param grid_sf an sf object of type polygon.
#' @param grid_resolution a numeric of length 1 or 2.
#' @param cname a character(1). Name of an attribute in grid_sf.
#'
#' @return a raster (terra object).
#'
#' @seealso [terra::rasterize] which this function wraps.
#'
#' @export
#'
grid_to_raster <- function(grid_sf, grid_resolution, cname) {

    stopifnot("Expected character(1) for `cname`" = length(cname) == 1)
    stopifnot("`cname` not found in grid_sf!" = cname %in% colnames(grid_sf))
    stopifnot("Expected an sf object for a grid!" =
        inherits(grid_sf, what = "sf"))
    stopifnot("Expected a grid of type POLYGON" =
        as.character(sf::st_geometry_type(grid_sf, by_geometry = FALSE)) %in%
            "POLYGON")

    grid_vect <- terra::vect(grid_sf)
    template <- terra::rast(grid_vect, resolution = grid_resolution)
    grid_r <- terra::rasterize(grid_vect, y = template, field = cname)

    return(grid_r)

}


