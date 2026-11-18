#' bl spatial clipper
#'
#' Cuts out polygon y from polygon x using sf:: functions
#'
#' @param x the polygon to be clipped
#' @param y the polygon to be used as clipping mask
#' @export
bl_st_erase = function(x, y) sf::st_difference(x, sf::st_union(sf::st_combine(y)))
