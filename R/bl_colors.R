#' bl Colors
#'
#' @description blue, green and red in different tints for usage in bl plots.
#'
#'
#' @param tint An Integer between 1 and 5 or 'CI' (sets typical colors for confidence intervals) which specifies the tint of the color. For bl_red (), only "1" is possible.
#' @examples
#' ## bl_blue(3)
#' #
#' #
#' #
#' p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'             geom_point (color = bl_blue (3))
#'
#' p
#'
#' p + geom_point (color = bl_red ())
#' p + geom_abline (intercept = 0,
#'                  slope = .5,
#'                  color = bl_green (3))
#'
#' @usage bl_blue (tint = 3)
#'
#' bl_green (tint = 3)
#'
#' bl_red (tint = 1)
#' @export

bl_blue <- function (tint = 3) {
    if (tint == 1) {
        the_color <- "#95bceb"
    } else if (tint == 2) {
        the_color <- "#82a7d4"
    } else if (tint == 3) {
        the_color <- "#6183ac"
    } else if (tint == 4) {
        the_color <- "#47668c"
    } else if (tint == 5) {
        the_color <- "#345276"
    } else if (tint == "CI") {
        the_color <- "#cfdae6"
    } else {
        stop ("immense color error: tint must be one of 1, 2, 3, 4, 5, 'CI'")
    }

    the_color
}


#' bl Colors
#'
#' @description blue, green and red in different tints for usage in bl plots.
#'
#'
#' @param tint An Integer between 1 and 5 which specifies the tint of the color. For bl_red (), only "1" is possible.
#' @examples
#' ## bl_blue(3)
#' #
#' #
#' #
#' p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'             geom_point (color = bl_blue (3))
#'
#' p
#'
#' p + geom_point (color = bl_red ())
#' p + geom_abline (intercept = 0,
#'                  slope = .5,
#'                  color = bl_green (3))
#'
#' @usage bl_blue (tint = 3)
#'
#' bl_green (tint = 3)
#'
#' bl_red (tint = 1)
#' @export
bl_green <- function (tint = 3) {
    if (tint == 1) {
        the_color <- "#a0d093"
    } else if (tint == 2) {
        the_color <- "#8ab87f"
    } else if (tint == 3) {
        the_color <- "#709b67"
    } else if (tint == 4) {
        the_color <- "#537a4a"
    } else if (tint == 5) {
        the_color <- "#406337"
    } else if (tint == "CI") {
        the_color <- "#c7d2c4ff"
    } else {
        stop ("immense color error: tint must be one of 1, 2, 3, 4, 5")
    }

    the_color
}


#' bl Colors
#'
#' @description blue, green and red in different tints for usage in bl plots.
#'
#'
#' @param tint An Integer between 1 and 5 which specifies the tint of the color. For bl_red (), only "1" is possible.
#' @examples
#' ## bl_blue(3)
#' #
#' #
#' #
#' p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'             geom_point (color = bl_blue (3))
#'
#' p
#'
#' p + geom_point (color = bl_red ())
#' p + geom_abline (intercept = 0,
#'                  slope = .5,
#'                  color = bl_green (3))
#'
#' @usage bl_blue (tint = 3)
#'
#' bl_green (tint = 3)
#'
#' bl_red (tint = 1)
#' @export
bl_red <- function (tint = 1) {
    if (tint == 1) {
        the_color <- "#d55639"
    } else {
        stop ("immense color error: tint must be 1")
    }
    the_color
}
