#' bl scale
#'
#' @description Creates axis with customizable ticks. Minor ticks are possible.
#'
#' @usage bl_y_scale (sides = "l",
#'                            major_y = 5,
#'                            minor_y = 1,
#'                            major_mode = 1,
#'                            major_ticklength = 2,
#'                            minor_ticklength = 1,
#'                            tick_unit = "mm",
#'                            air = NA,
#'                            limits = NULL,
#'                            ...)
#'
#'
#'        bl_y_scale (sides = "l",
#'                            major_y = 5,
#'                            minor_y = 1,
#'                            major_mode = 1,
#'                            major_ticklength = 2,
#'                            minor_ticklength = 1,
#'                            tick_unit = "mm",
#'                            air = NA,
#'                            limits = NULL,
#'                            ...)
#' @param sides One or a combination of the letters "b" (bottom), "l" (left), "t" (top), "r" (right)
#' @param major_y the positons of the major ticks on the y axis. can be (i) a single number, based on which the breaks are calculated automatically or (ii) a vector of y positions
#' @param minor_y the number of minor ticks between two major ticks.
#' @param major_mode either 1 or 2 for different methods of automatic tick position calculation. Is neglected when major_y is a vector
#' @param major_ticklength the length of the major ticks to be drawn, specified in mm.
#' @param minor_ticklength the length of the minor ticks to be drawn, specified in mm.
#' @param air expands the plots over the actual plot limits.
#' @param limits set the limits of the axis
#' @param ... additional arguments passed to scale_y_continuous within the function
#' @examples
#' # a scatterplot
#'  ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'         geom_point (color = bl_green (3)) +
#'         bl_x_scale () +
#'         bl_y_scale () +
#'         bl_theme ()
#'
#'
#' # a barplot
#' my_dat <- bl_calculate_grouped_mean_and_SE (npk, "yield", "N", "P")
#'
#' ggplot (my_dat, aes (x = group_N , y = yield, fill = subgrp_P)) +
#'
#' geom_bar (stat = "identity", position = "dodge", color = "black") +
#' scale_fill_manual (values = c (bl_blue (1), bl_blue (5))) +
#' bl_y_scale (limits = c (0, 75), air = F) +
#' theme (legend.position = c (.1, .85)) +
#' bl_theme ()
#'
#' @export

bl_y_scale <- function (sides = "l",
                           major_y = 5,
                           minor_y = 1,
                           major_mode = 1,
                           major_ticklength = 2,
                           minor_ticklength = 1,
                           tick_unit = "mm",
                           air = NA,
                           limits = NULL,
                           ...) {
    sides <- side_preparations(sides)

    # calculate / set major breaks for tick placement
    the_breaks <- set_breaks(major_y, minor_y, major_mode)

    # set the tick mark length
    the_ticks <- set_tick_length(major_ticklength, minor_ticklength, minor_y, tick_unit)



    # set bottom argument (especially for barplots)
    bottom <- set_bottom(air)


    # if limits are not set, get limits from plot
    if (is.null(limits)) {
          limits <- ggplot_build(last_plot())$layout$panel_params[[1]]$y.range

    }



    # make and return scale objects
    argument_list <- list (
        coord_cartesian (clip = "off"),
        scale_y_continuous (
            # breaks
            breaks = the_breaks,

            # set plot limits
            limits = limits,

            # add distances to plot limits
            expand = bottom,

            # define axis position (left/right)
            position = sides,
            ...),
        theme (
             # set tick length
             axis.ticks.length.y = the_ticks

            )
    )
    return (argument_list)
}


#' bl scale
#'
#' @description Creates axis with customizable ticks. Minor ticks are possible.
#'
#' @usage bl_x_scale (sides = "l",
#'                            major_x = 5,
#'                            minor_x = 1,
#'                            major_mode = 1,
#'                            major_ticklength = 2,
#'                            minor_ticklength = 1,
#'                            tick_unit = "mm",
#'                            air = NA,
#'                            limits = NULL,
#'                            ...)
#'
#'
#'        bl_y_scale (sides = "l",
#'                            major_y = 5,
#'                            minor_y = 1,
#'                            major_mode = 1,
#'                            major_ticklength = 2,
#'                            minor_ticklength = 1,
#'                            tick_unit = "mm",
#'                            air = NA,
#'                            limits = NULL,
#'                            ...)
#' @param sides One or a combination of the letters "b" (bottom), "l" (left), "t" (top), "r" (right)
#' @param major_x the positons of the major ticks on the x axis. can be (i) a single number, based on which the breaks are calculated automatically or (ii) a vector of x positions
#' @param minor_x the number of minor ticks between two major ticks.
#' @param major_mode either 1 or 2 for different methods of automatic tick position calculation. Is neglected when major_x is a vector
#' @param major_ticklength the length of the major ticks to be drawn, specified in mm.
#' @param minor_ticklength the length of the minor ticks to be drawn, specified in mm.
#' @param air expands the plots over the actual plot limits.
#' @param limits set the limits of the axis
#' @param ... additional arguments passed to scale_x_continuous within the function
#' @examples
#' # a scatterplot
#'  ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
#'         geom_point (color = bl_green (3)) +
#'         bl_x_scale () +
#'         bl_y_scale () +
#'         bl_theme ()
#'
#'
#' # a barplot
#' my_dat <- bl_calculate_grouped_mean_and_SE (npk, "yield", "N", "P")
#'
#' ggplot (my_dat, aes (x = group_N , y = yield, fill = subgrp_P)) +
#'
#' geom_bar (stat = "identity", position = "dodge", color = "black") +
#' scale_fill_manual (values = c (bl_blue (1), bl_blue (5))) +
#' bl_y_scale (limits = c (0, 75), air = F) +
#' theme (legend.position = c (.1, .85)) +
#' bl_theme ()
#'
#' @export
bl_x_scale <- function (sides = "l",
                           major_x = 5,
                           minor_x = 1,
                           major_mode = 1,
                           major_ticklength = 2,
                           minor_ticklength = 1,
                           tick_unit = "mm",
                           air = NA,
                           limits = NULL,
                           ...) {
    sides <- side_preparations(sides)

    # calculate / set major breaks for tick placement
    the_breaks <- set_breaks(major_x, minor_x, major_mode)

    # set the tick mark length
    the_ticks <- set_tick_length(major_ticklength, minor_ticklength, minor_x, tick_unit)



    # set bottom argument (especially for barplots)
    bottom <- set_bottom(air)


    # if limits are not set, get limits from plot
    if (is.null(limits)) {
          limits <- ggplot_build(last_plot())$layout$panel_params[[1]]$x.range

    }



    # make and return scale objects
    argument_list <- list (
        coord_cartesian (clip = "off"),
        scale_x_continuous (
            # breaks
            breaks = the_breaks,

            # set plot limits
            limits = limits,

            # add distances to plot limits
            expand = bottom,

            # define axis position (left/right)
            position = sides,
            ...),
        theme (
             # set tick length
             axis.ticks.length.x = the_ticks

            )
    )
    return (argument_list)
}

# helper functions
# Function to calculate fractions vector
calculate_fractions_vector <- function(original_vector, number_fractions) {
  # Calculate the interval between each element
  intervals <- diff(original_vector)

  # Calculate the fractions
  fractions <- seq(1 / (number_fractions + 1), 1 - 1 / (number_fractions + 1), length.out = number_fractions)

  # Initialize an empty vector to store the fractions vector
  fractions_vector <- numeric(0)

  # Iterate over each interval and add fractions
  for (i in 1:(length(original_vector) - 1)) {
    start_value <- original_vector[i]
    end_value <- original_vector[i + 1]

    # Calculate the fractions within each interval
    interval_fractions <- start_value + intervals[i] * fractions
    fractions_vector <- c(fractions_vector, interval_fractions)
  }

  return(fractions_vector)
}


#scale preparations
side_preparations <- function (s) {
    # preps ####
    # check for invalid side
    if (grepl("[^lr]", s)) {
        stop(gsub("[lr]", "", s), " is not a valid side: only l and r are valid")
    }
    # split s to character vector
    s <- unlist(strsplit(s, ""))
    if (length (s ) > 1) {
        s <- s [1]
        warning ("at the moment, no more than one side can be specified")
    }
    # Replace "l" with "left"
    s <- gsub("l", "left", s)
    # Replace "r" with "right"
    s <- gsub("r", "right", s)

    return(s)
}


# set major breaks
set_breaks <- function(major, minor, the_mode){
    if (identical(minor, NA)){
        stop("'minor_x'/'minor_y' must be numeric!")
    }
    minor_ <- minor + 1
    if (length (major) == 1) {
        major_breaks <-
            if (the_mode == 1) {
                scales::breaks_extended (n = major*minor_)
            } else if ( the_mode == 2) {
                scales::breaks_pretty (n = major*minor_)
            } else {
                stop ("the_mode must either be 1 or 2.")
            }
    } else {
        major_breaks <- major
    }
    return(major_breaks)
}


# convert licklength to mm unit
set_tick_length <- function (major_length, minor_length, minor, the_unit){
    major_lgth <- ggplot2::unit (major_length, the_unit)
    minor_lgth <- ggplot2::unit (minor_length, the_unit)
    tick_length <- c(major_length, rep(minor_length, minor)) %>% ggplot2::unit(the_unit)
    return(tick_length)
}


# set bottom argument
set_bottom <- function(air){
    if (is.numeric (air)) {
        the_bottom <- expansion (mult = air)
    } else if (!is.na (air [1])) {
        if (air [1] == TRUE) {
            the_bottom <- expansion (mult = c(0.1, 0.1))
        } else if (air [1] == FALSE) {
            the_bottom <- expansion (mult = c(0, 0))
        }
    } else {
            the_bottom <- expansion (mult = c(0.05, 0.05))
    }
    return(the_bottom)
}
