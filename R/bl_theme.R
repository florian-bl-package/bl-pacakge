#' bl Theme
#'
#' @description A standard bl Theme for plotting with ggplot2. 'bl_theme' is a wrapper for 'ggplot2::theme'. It accepts all additional parameters of 'theme'.
#'
#' @param aspect.ratio The ratio of x and y axis. Is specified via plot_ratio; if plot_ratio is not given defaults to 1.
#' @param panel.background Defaults to a blank background.
#' @param lsize Defines the thickness of axes and major ticks. If given, is specified via line_size; else defaults to 0.3 mm.
#' @param tick.length Globally defines the length of the major ticks in mm. Defaults to 1.5.
#' @param axis.text.color Sets the color of tick annotations.
#' @param axis.text.size Sets the text size of tick annotations.
#' @param text.size Sets the text size of the axis labels.
#' @param text.font Globally sets the fornt for the plot.
#' @param label.text.color Globally Sets the Text Color.
#' @param x.margin sets the vertical margin around the plot. If not given defautls to 5. Interacts with y.margin. Particularly important for plotting several graphs on a page. Is overwritten by plot_distance_x.
#' @param y.margin sets the horizontal margin around the plot. If not given defautls to 5. Interacts with x.margin. Particularly important for plotting several graphs on a page. Is overwritten by plot_distance_y.
#' @param ... all parameters accepted by 'ggplot2::theme()' can also be used with bl_theme (). Some logical conflicts produce an error. In these cases, it is adviced to use 'theme()' seperately. For further assistance view '?theme'.
#' @examples
#' library (ggplot2)    ## Required for plotting.
#' library (extrafont)  ## Required for the standard - bl - font "ChantillyLH". ChantillyLH must be registered in R.
#'
#' ggplot (iris, aes (Sepal.Length, Sepal.Width, color = Species)) +
#'         geom_point ()+
#'         bl_theme (x.margin = 10)
#'
#'
#'
#' ########
#' ## external control of plot ratio and line size
#'
#' library (ggplot2)
#' library (extrafont)
#'
#' plot_ratio <- 0.3
#' line_size <- 2
#'
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'        geom_point()+
#'        bl_theme(x.margin = 10)
#'
#' ##
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'        geom_point()+
#'        bl_theme(x.margin = 10, aspect.ratio = 2, lsize = 0.1)   # aspect.ratio and lsize are not overwritten by plot_ratio and line_size
#'
#'
#' plot_distance_x <- 100
#'
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'        geom_point()+
#'        bl_theme(x.margin = 10)                                  # x.margin and y.margin are overwritten by plot_distance_x and plot_distance_y
#' @export

bl_theme <- function (aspect.ratio = NA,
                       panel.background = "blank",
                       lsize = NA,
                       #tick.length = 1.5,
                       axis.text.color = "black",
                       axis.text.size = 14,
                       text.size = 18,
                       text.font = "ChantillyLH",
                       label.text.color = "black",
                       x.margin = 5,
                       y.margin = 5,
                       axis.title.x = NA,
                       axis.title.y = NA,
                       ...
                      ) {

    if (panel.background == "blank") {
        panel.background <- ggplot2::element_blank()
    } else {
        panel.background <- panel.background
    }


     axis.text <- ggplot2::element_text (color = axis.text.color,
                                         size = axis.text.size)


    if (is.na (aspect.ratio)) {
        if (exists("plot_ratio")) {
                aspect.ratio <- plot_ratio
                message ("aspect ratio was set by plot_ratio")
            } else {
                aspect.ratio <- 1
                message ("aspect.ratio set to 1")
            }
    }


    if (is.na (lsize)) {
        if (exists("line_size")) {
            lsize <- line_size
            message ("lsize was set by line_size")
            } else {
            lsize <- 0.5
            message ("lsize set to .5")
            }
        }


    if (exists("plot_distance_x")) {
            x.margin <- plot_distance_x
            message ("x.margin was set by plot_distance_x")
        }


    if (exists("plot_distance_y")) {
        y.margin <- plot_distance_y
        message ("y.margin was set by plot_distance_y")
    }

     if (is.na (axis.title.x)) {
         axis.tit.x = element_text(margin = margin(t = 5, r = 0, b = 0, l = 0))
     } else {
         axis.tit.x = axis.title.x
     }

     if (is.na (axis.title.y)) {
         axis.tit.y = element_text(margin = margin(t = 0, r = 5, b = 0, l = 0))
     } else {
         axis.tit.y = axis.title.y
     }


    return (ggplot2::theme (
       aspect.ratio = aspect.ratio,
       panel.background = panel.background,
       panel.border = ggplot2::element_rect (fill = NA,
                                        size = lsize),
       plot.margin = ggplot2::unit (rep (c(x.margin/2, y.margin/2), 2), "mm"),
       axis.ticks = ggplot2::element_line (color = "black",
                                       lineend = "round",
                                       size = lsize),
       #axis.ticks.length = ggplot2::unit (tick.length, "mm"),
       axis.text = axis.text,
       text = ggplot2::element_text (size=text.size,
                                family=text.font,
                                color = label.text.color),
       axis.title.x = axis.tit.x,
       axis.title.y = axis.tit.y,
       ...
       )
    )
}


#' bl_clean_theme
#'
#' @description A ready-to-use standard theme for ggplot2 plots appropriate for most bl graphs.
#' @examples
#'
#' # make a simple scatterplot using the standard bl theme
#' pp <- ggplot (iris, aes (Sepal.Length, Sepal.Width, color = Species)) +
#'         geom_point ()
#'
#' # create deviations of pp using "bl_clean_theme"
#' pp1 <- pp + bl_clean_theme
#' pp2 <- pp + bl_clean_theme + theme (legend.title=element_blank())
#' pp3 <- pp + bl_clean_theme + theme (legend.position="none")
#'
#' # draw facetted plot
#' bl_draw_plot (list (pp, pp1, pp2, pp3))

#' @export

bl_clean_theme <- function() {
  # Aufrufen von bl_theme – das Paket "bl" wird über Imports bereitgestellt.
  ggplot2::theme_set(        # optional: wenn du das Theme sofort setzen willst, entferne das `+` unten
    bl::bl_theme(
      plot.title = ggplot2::element_text(family = "Chantilly", size = 14),
      text.size = 15,
      legend.position = c(.8, .85),
      legend.text.align = 0,
      legend.background = ggplot2::element_rect(fill = "transparent"),
      legend.key = ggplot2::element_rect(fill = "transparent"),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = "transparent"),
      legend.title = ggplot2::element_text(family = "Chantilly", size = 12),
      legend.text = ggplot2::element_text(family = "Chantilly", size = 12),
      legend.spacing.y = ggplot2::unit(0.0001, "npc"),
      legend.spacing.x = ggplot2::unit(.004, "npc")
    ) +
    ggplot2::theme(
      axis.text  = ggplot2::element_text(family = "Chantilly", size = 11),
      axis.title = ggplot2::element_text(family = "Chantilly", size = 13)
    )
  )
}

# bl_clean_theme <- bl_theme ( # should be kept silently in the package and backed up with a new theme
#                 plot.title = element_text(family = "Chantilly", size = 14),
#                 text.size = 15,
#                 legend.position = c(.8, .85),
#                 legend.text.align = 0,
#                 legend.background = element_rect(fill="transparent"),
#                 legend.key = element_rect(fill="transparent"),
#                 panel.grid.major = element_blank(),
#                 panel.grid.minor = element_blank(),
#                 plot.background = element_rect(fill="transparent"),
#                 legend.title = element_text(family = "Chantilly", size = 12),
#                 legend.text = element_text(family = "Chantilly", size = 12),
#                 legend.spacing.y = unit (0.0001, units = "npc"),
#                 legend.spacing.x = unit (.004, units = "npc")
#                ) +
#             theme (axis.text = element_text(family = "Chantilly", size = 11),
#             axis.title = element_text (family = "Chantilly", size = 13))
