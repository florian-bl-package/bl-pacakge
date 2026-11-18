#' bl Save Svg
#'
#' Saves a plot with embedded font to svg. Can either be the last plot that has been drawn or any plot stored as an object. See also Cairo::CairoSVG, as bl_save_svg is based on it.
#' @param plot The plot to be saved. Defaults to the last plot created.
#' @param file The path of the file to be created.
#' @param width Width of the Plot Panel to be created. Can be supplied via panel_width. Otherwise, if 'plot' has a panel width argument, this is taken as width. Defaults to 90 mm.
#' @param heigth heigth of the Plot Panel to be created. Can be supplied via panel_height. Defaults to 90 mm.
#' @examples
#' library (ggplot2)    ## Required for plotting.
#'
#' my_plot <- ggplot (iris, aes (Sepal.Length, Sepal.Width, color = Species)) +
#'         geom_point () +
#'         bl_theme ()
#'
#' bl_save_svg (my_plot, "path/to/drive/plotName")
#' @export

bl_save_svg <- function (plot = ggplot2::last_plot(),
                         file_path = NA,
                         width = NA,
                         height = NA) {

    if (is.na(file_path)) {
        stop("Cannot save to .svg if file_path is not set.")
    }

    # bl_panel-Objekt: Maße und eigentlichen Plot herausholen
    if (inherits(plot, "bl_panel")) {
        if (is.na(width))  width  <- plot$panel_width
        if (is.na(height)) height <- plot$panel_height
        plot <- plot$plot
    }

    # Fallbacks
    if (is.na(width)) {
        if (exists("panel_width")) {
            width <- panel_width
            message("panel width was set by panel_width")
        } else {
            width <- 90 / 25.4
            message("panel width was set to 90 mm")
        }
    }

    if (is.na(height)) {
        if (exists("panel_height")) {
            height <- panel_height
            message("panel height was set by panel_height")
        } else {
            height <- 90 / 25.4
            message("panel height was set to 90 mm")
        }
    }

    if (!grepl("\\.svg$", file_path)) {
        file_path <- paste0(file_path, ".svg")
    }

    grDevices::svg(file = file_path, width = width, height = height)
    on.exit(grDevices::dev.off(), add = TRUE)
    print(plot)

    invisible(file_path)
}
# bl_save_svg <- function (plot = ggplot2::last_plot(),
#                           file_path = NA,
#                           width = NA,
#                           height = NA
#                           ) {
#     if(is.na(file_path)){
#         stop("Cannot save to .svg if file_path is not set.")
#     }
#
#         if (is.na (width)) {
#         if (exists("panel_width")) {
#                 width <- panel_width
#                 message ("panel width was set by panel_width")
#             } else if (length (plot  $ panel_width) == 1){
#                 width <- plot $ panel_width
#                 message ("panel width was set by plot extent")
#             } else {
#                 width <- 90/25.4
#                 message ("panel width was set to 90 mm")
#         }
#         }
#
#
#         if (is.na (height)) {
#             if (exists("panel_height")) {
#                 height <- panel_height
#                 message ("panel height was set by panel_height")
#             } else if (length (plot  $ panel_height) == 1){
#                 height <- plot $ panel_height
#                 message ("panel height was set by plot extent")
#             } else {
#                 height <- 90/25.4
#                 message ("panel height was set to 90 mm")
#             }
#         }
#
#
#     if (isFALSE (grepl (".svg", file_path))) {
#         file_path <- paste0 (file_path, ".svg")
#     }
#
#
#     grDevices::svg (file = file_path,
#             width = width, height = height)
#     print (plot)
#     dev.off()
#   }


