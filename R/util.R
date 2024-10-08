# Identify outliers
#
# @description
# Identify the outliers in the given numeric vector.
#
# @param x a numeric.
#
# @return a logical.
#
is_outlier <- function(x) {
    return(x %in% grDevices::boxplot.stats(x)$out)
}

