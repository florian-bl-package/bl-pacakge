#' bl_draw_plot
#'
#' A function which can be used to draw plots
#'
#' @param plot plot or list of plots to be drawn. Several plots MUST be put in a list.
#' @param plot_width sets the with of each plot in the panel in mm. Is overwritten by "single_column" if existant, else defaults to 90 mm.
#' @param plot_ratio sets the aspect ratio of each plot in the panel. Defaults to 1, but plots with fixed aspect ratio keep their aspect ratio if nothing is provided.
#' @param plot_rows for multiple plotting sets the number of rows of the plot panel.
#' @param plot_cols for multiple plotting sets the number of columns of the plot panel.
#' @param plot_distance_x sets the distance between multiple plots in x direction.
#' @param plot_distance_y sets the distance between multiple plots in y direction.
#' @param align_plots if speciefied, multiple plots are aligned in the panel. Possible values: TRUE and "hv" align both horizontally and vertically. "h" aligns horizontally. "v" aligns vertically
#' @param rel_heights vector of relative heights of each row in the facceted plot. length should match the number of rows to be drawn.
#' @param rel_widths the same as rel_heights but for  column width. length should match the number of columns to be drawn.
#' @examples
#' # create a simple scatterplot
#' p1 <- bl_ecology_data %>%
#'     ggplot (aes(x = bare_ground, y = grasshopper)) +
#'     geom_point (color = bl_blue(3)) +
#'     bl_theme ()
#'
#' bl_draw_plot(p1)
#'
#' # create three more plots
#' p2 <- bl_ecology_data %>%
#'     ggplot (aes(x = bare_ground, y = butterflies)) +
#'     geom_point (color = bl_blue(3)) +
#'     bl_theme ()
#'
#'
#' p3 <- bl_ecology_data %>%
#'     ggplot (aes(x = bare_ground, y = leafhoppers)) +
#'     geom_point (color = bl_blue(3)) +
#'     bl_theme ()
#'
#'
#' # plot all three plots at once -
#' bl_draw_plot(list(p1, p2, p3))
#'
#'
#' # make a fourth plot
#' p4 <- bl_ecology_data %>%
#'     ggplot (aes(x = bare_ground, y = elevation)) +
#'     geom_point (color = bl_blue(3)) +
#'     bl_theme ()
#'
#' # plot all four plots -  see how they align!
#'
#' bl_draw_plot(list(p1, p2, p3, p4))

#'

#' @export

bl_draw_plot <- function (plot = ggplot2::last_plot (),
                           plot_width = NA,
                           plot_ratio = NA,
                           plot_rows  = NA,                     # wie viele Plots sollen uebereinander dargestellt werden?
                           plot_cols  = NA,                     # wie viele Plots sollen nebeneinander dargestellt werden?
                           plot_distance_x = NA,                # abstand der plots voneinander (wenn mehere gleichzeitig uebereinander dargestellt werden)
                           plot_distance_y = NA,
                           align_plots = T,
                           rel_heights = 1,
                           rel_widths = 1
) {

# check if plot is a list, else make a list. also handle mixtures of list and non_list objects and nested lists in lists
    plot <- list (plot)
    plot <- recursive_unnest_plotlist (plot)
# check if inputs have correct length
    if (length (plot_width) != 1) {
        stop ("plot_width must be numeric and of length == 1.")
    }

    if (length(plot_ratio) != 1) {
        if (length(plot_ratio) != length (plot)) {
            stop ("plot_ratio must be numeric and of length either 1 (will be recycled) or of length matching the total numer of plots.")
        }
    }

    if (length(plot_rows) != 1) {
        stop ("plot_rows must be an integer of length == 1.")
    }

    if (length(plot_cols) != 1) {
        stop ("plot_cols must be an integer of length == 1.")
    }

    if (length (plot_distance_x) != 1) {
        stop ("plot_distance_x must be numeric and of length == 1.")
    }

    if (length (plot_distance_y) != 1) {
        stop ("plot_distance_y must be numeric and of length == 1.")
    }







# check plot width
    if (is.na (plot_width)) {
        if (exists ("single_column")) {
                plot_width <- single_column
                message ("plot width was set by single_column")
            } else {
                plot_width <- 90
            }
    }

# check number of plots and set rows + columns
    #funktioniert noch nicht perfekt (manchmal zu wenig platz fuer alle plots) --> behoben?
    no_plots <- length (plot)
    if (is.na (plot_rows)){
        if (is.na (plot_cols)) {
            rows <- round (sqrt (no_plots), digits = 0)
            cols <- ceiling (sqrt (no_plots))
        } else {
            rows <- ceiling (no_plots / plot_cols)
            cols <- plot_cols
        }
    } else if (is.na (plot_cols)) {
            cols <- ceiling (no_plots / plot_rows)
            rows <- plot_rows
    } else {
        rows <- plot_rows
        cols <- plot_cols
    }

    # check if inputs (rel_heights and rel_widths) have correct length
    if (length(rel_heights) != 1) {
        if (length(rel_heights) != cols) {
            stop ("rel_heights must be numeric and of length either 1 (will be recycled) or of length matching the total numer of rows to be plotted.")
        }
    }


    if (length(rel_widths) != 1) {
        if (length(rel_widths) != rows) {
            stop ("rel_widths must be numeric and of length either 1 (will be recycled) or of length matching the total numer of columns to be plotted.")
        }
    }


# check if plot(s) has / have a fixed aspect ratio, otherwise assign aspect.ratio = plot_ratio
    # make vector of plot ratios
    pr <- c ()

    # check whether plot_ratio is defined in function input
    if (is.na (plot_ratio [[1]])) {

        # if plot_ratio is not set - get fixed ratio of plot (if it has one)
        for (i in 1:length (plot)) {
            if (is_fixed_ratio (plot [[i]])) {
                pr [i] <- plot[[i]]$theme$aspect.ratio
            } else {
        # if plot_ratio is not set and plot has no fixed ratio, set plot_ratio = 1
                pr [i] <- 1
            }
        }

    # if plot ratio is set
    } else {

        # if plot ratio has length == 1, all plots shall have the same plot_ratio
        if (length(plot_ratio) == 1) {
            pr <- rep (plot_ratio, length (plot))
        }

        # if plot_ratio is a vector of length of the number of plots, these ratios shall be used-
        else {
            pr <- plot_ratio
        }
    }



#check if plot(s) has / have margin. Otherwise set margin = 0000. Add plot_distance_x and _y to margin.

    if (is.na (plot_distance_x)){
       plot_distance_x <- 0
       message ("plot_distance_x set to 0")
    }

    if (is.na (plot_distance_y)){
       plot_distance_y <- 0
       message ("plot_distance_y set to 0")
    }




#create plot-grid
    if (isTRUE (align_plots)) {
          gr = "hv"
          message ("plot aligned horizontally and vertically")
        } else if (is.na (align_plots)) {
          gr = "none"
          message ("plots not aligned")
        } else if (isFALSE (align_plots)){
          gr = "none"
          message ("plots not aligned")
        } else {
            gr = align_plots
        }


    withCallingHandlers({

        # for-Schleife (Margins)
        for (i in 1:length(plot)) {
            if (has_margin(plot[[i]])) {
                plot[[i]] <- plot[[i]] + theme(plot.margin = plot[[i]]$theme$plot.margin +
                                                   unit(rep(c(plot_distance_y/2, plot_distance_x/2), 2), "mm"))
                message(paste0("plot margin added to plot_distance_x/y of plot ", i))
            } else {
                plot[[i]] <- plot[[i]] + theme(plot.margin = unit(rep(c(plot_distance_y/2, plot_distance_x/2), 2), "mm"))
                message("plot has no margin; margin set to plot_distance_x/y")
            }
        }

        # plot-grid
        multi_plot <- cowplot::plot_grid(plotlist = plot,
                                         align = gr,
                                         nrow = rows,
                                         ncol = cols,
                                         rel_heights = rel_heights,
                                         rel_widths = rel_widths)

        # panel dimensions
        # panel dimensions
        cell_w_mm <- plot_width + plot_distance_x   # Margin (dist/2 je Seite) steckt schon im Plot

        cell_h_mm <- vapply(seq_along(plot), function(i) {
            needed_height_mm(plot[[i]], cell_w_mm, pr[i])
        }, numeric(1))

        panel_height <- (mean(cell_h_mm) * rows) / 25.4
        panel_width  <- (cell_w_mm * cols) / 25.4

        # panel_height <- (plot_width * mean(pr) * rows + plot_distance_y * rows) / 25.4
        # panel_width  <- (plot_width * cols + plot_distance_x * cols) / 25.4

        # fenster öffnen
        #windows(width = panel_width, height = panel_height, rescale = "fixed")

    },
    warning = function(w) {
        if (grepl("PostScript|Zeichensatzfamilie", conditionMessage(w))) {
            invokeRestart("muffleWarning")
        }
    })

#return object
    # return object (mit Klasse)
    panel_return <- structure(
        list("panel_height" = panel_height,
             "panel_width"  = panel_width,
             "plot"         = multi_plot),
        class = c("bl_panel", "list")
    )

    return (panel_return)
}






##### extra functions ####
is_fixed_ratio <- function (plot) {
  ggplot2::ggplotGrob (plot) $ respect
}


has_margin <- function (plot) {
   !is.null(plot$theme$plot.margin )
}

# function to handle plots in and outside lists
recursive_unnest_plotlist <- function(lst) {
  result <- list()
  for (element in lst) {
    if (inherits(element, "gg")) {
      result <- c(result, list(element))

    } else if (is.list(element)) {
      result <- c(result, recursive_unnest_plotlist(element))
    }
  }
  return(result)
}

# Hilfsfunktion: benötigte Gesamthöhe (mm) für ein Plot mit fixem Seitenverhältnis,
# wenn die Gesamtbreite (inkl. Labels, Legende, Ränder) cell_width_mm sein soll
needed_height_mm <- function(p, cell_width_mm, ratio) {
    g <- ggplot2::ggplotGrob(p)

    w_null <- grid::unitType(g$widths)  == "null"
    h_null <- grid::unitType(g$heights) == "null"

    fixed_w <- sum(grid::convertWidth (g$widths [!w_null], "mm", valueOnly = TRUE))
    fixed_h <- sum(grid::convertHeight(g$heights[!h_null], "mm", valueOnly = TRUE))

    panel_w <- cell_width_mm - fixed_w
    panel_h <- panel_w * ratio
    panel_h + fixed_h
}


#' Print method for bl_panel objects
#'
#' Opens a graphics window with the correct dimensions and draws the plot.
#'
#' @param x An object of class `bl_panel`, as returned by [bl_draw_plot()].
#' @param ... Not used.
#'
#' @return `x`, invisibly.
#' @exportS3Method
print.bl_panel <- function(x, ...) {
    withCallingHandlers({
        windows(width = x$panel_width, height = x$panel_height, rescale = "fixed")
        print(x$plot)
    },
    warning = function(w) {
        if (grepl("PostScript|Zeichensatzfamilie", conditionMessage(w))) {
            invokeRestart("muffleWarning")
        }
    })
    invisible(x)
}


