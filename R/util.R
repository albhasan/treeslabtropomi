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
    return(x %in% boxplot.stats(x)$out)
}

