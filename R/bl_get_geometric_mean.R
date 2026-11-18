#' bl geometric mean
#'
#' bl_calculate_geometric_mean calculates the geometric mean from a vector of distances
#' bl_get_spatial_geometric_mean calculates the geometric mean of distances of a spatial object to nearest neighbours of another (or the same) object
#'
#' @param x In bl_get_geometric_mean a vector of distances of length >1, in bl_spatial_geometric_mean a spatial objects object
#' @param y optional: a second spatial objects object. If given, distances are calculated between each object of x and y.
#' @param na_rm shall NA values be removed?
#' @param zero_propagate Propagate neighbours that touch
#' @param no_neighbours Number of nearest neighbours to which the spatial geometric mean should be calculated.
#' @usage bl_get_geometric_mean(x, na_rm = TRUE, zero_propagate = FALSE)
#' @usage bl_get_spatial_geometric_mean(x = NULL, y = NULL, ID = NA, no_neighbours = 3)
#' @examples
#' for (i in unique(bl_ecology_data$sub_area)){
#'     cat("Gemoetric mean of distances across all study plots in sub area ")
#'     cat(crayon::blue(i))
#'     cat("\n")
#'     print (bl_get_geometric_mean(bl_ecology_data[bl_ecology_data$sub_area == i,]$isolation))
#'     cat("\n \n")
#' }
#'
#'
#' # check out common urban gardening in Dortmund!
#' # Load necessary library for spatial
#' library(sf)
#'
#' # URL for the KML file
#' url <- "https://open-data.dortmund.de/api/v2/catalog/datasets/querbeet-gemeinschaftsgarten/exports/kml"
#' # Read the KML file from the URL
#' common_gardens <- st_read(url)
#' # Check the structure of the spatial data
#' print(common_gardens)
#'
#' # get the spatial gemeoetric mean to the closest three neighboring common gardens
#' common_gardens$distance_to_closest_three <- bl_get_spatial_geometric_mean(common_gardens, no_neighbours = 3)$geometric_mean
#' @export
bl_get_geometric_mean <- function(x, na_rm=TRUE, zero_propagate = FALSE){
  if(any(na.omit(x) < 0)){
    cat ("Negative values have been provided for x. Check your inputs!")
    return(NaN)
  }
  if(zero_propagate){
    if(any(na.omit(x) == 0)){
        cat ("zero propagation!")
      return(0)
    }
    exp(mean(log(x), na.rm = na_rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na_rm) / length(x))
  }
}



#' bl spatial geometric mean
#'
#' bl_calculate_geometric_mean calculates the geometric mean from a vector of distances
#' bl_get_spatial_geometric_mean calculates the geometric mean of distances of a spatial object to nearest neighbours of another (or the same) object
#'
#' @param x In bl_get_geometric_mean a vector of distances of length >1, in bl_spatial_geometric_mean a spatial objects object
#' @param y optional: a second spatial objects object. If given, distances are calculated between each object of x and y.
#' @param na_rm shall NA values be removed?
#' @param zero_propagate Propagate neighbours that touch
#' @param no_neighbours Number of nearest neighbours to which the spatial geometric mean should be calculated.
#' @usage bl_get_geometric_mean(x, na_rm = TRUE, zero_propagate = FALSE)
#' @usage bl_get_spatial_geometric_mean(x = NULL, y = NULL, ID = NA, no_neighbours = 3)
#' @examples
#' for (i in unique(bl_ecology_data$sub_area)){
#'     cat("Gemoetric mean of distances across all study plots in sub area ")
#'     cat(crayon::blue(i))
#'     cat("\n")
#'     print (bl_get_geometric_mean(bl_ecology_data[bl_ecology_data$sub_area == i,]$isolation))
#'     cat("\n \n")
#' }
#'
#'
#' # check out common urban gardening in Dortmund!
#' # Load necessary library for spatial
#' library(sf)
#'
#' # URL for the KML file
#' url <- "https://open-data.dortmund.de/api/v2/catalog/datasets/querbeet-gemeinschaftsgarten/exports/kml"
#' # Read the KML file from the URL
#' common_gardens <- st_read(url)
#' # Check the structure of the spatial data
#' print(common_gardens)
#'
#' # get the spatial gemeoetric mean to the closest three neighboring common gardens
#' common_gardens$distance_to_closest_three <- bl_get_spatial_geometric_mean(common_gardens, no_neighbours = 3)$geometric_mean
#' @export
bl_get_spatial_geometric_mean <- function (x = NULL, y = NULL, ID = NA, no_neighbours = 3) {
    # error when no x is set
    if (is.null(x)) {
        stop ("x was not specified")
    }

    # if ID is not set, make continuous ID
    if (is.na(ID)) {
        my_ID = 1:length(x[[1]])
    } else {
        my_ID = x[[ID]]
    }

    # if y is not set, calculate distance to x
    if (is.null(y)) {
        y = x
        first_neighbour = 2
    } else {
        first_neighbour = 1
    }
    # calculate distances between all objects in x and y
    distances <- st_distance (x, y) %>% t %>% as.data.frame()

    # calculate geometric mean of distance to nearest neighbours and put into data frame
    geometric_means <- data.frame ("ID" = my_ID)
    for (i in 1: length (geometric_means$ID)) {
        geometric_means [i, "geometric_mean"] <- sort (distances [[i]]) [first_neighbour:no_neighbours] %>%
                                                                as.numeric %>%
                                                                bl_get_geometric_mean ()
    }
    return (geometric_means)
}
