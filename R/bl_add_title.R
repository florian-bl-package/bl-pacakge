#' bl Title
#'
#' Adds a title to the plot, by default inside the plotting region. This is particularely useful when creating several plots to be aligned within the same panel. Note that the function needs a ggplot2 - plot object as input. The workflow should thus be: i) create the plot and ii) add the titel afterwards.
#'
#' @param plot the plot the title will be added to.
#' @param title the title for the plot.
#' @param x the x-position of the title. If the x-axis is a continuous scale and xrel is set to "TRUE", this is a relative position (e.g. at 50 percent of the x-axis). Otherwise, this is an absolute position (e.g. at x = .5).
#' @param y the y-position of the title. If the y-axis is a continuous scale and yrel is set to "TRUE", this is a relative position (e.g. at 90 percent of the y-axis). Otherwise, this is an absolute position (e.g. at y = .9).
#' @param headroom defines the space that shall be added to a plot in order to draw the title without overlapping with plotted data
#' @param expand_y logical. shall the y axis be expanded to fit a title in the plot?
#' @param xrel logical. relative or absolute x-position?
#' @param yrel logical. relative or absolute y-position?
#' @param font the font of the title
#' @param text_size the text size of the title
#' @param align_lr adjusts the horizontal anchor of the title
#' @param align_tb adjusts the vertical anchor of the title
#' @param ... additional arguments, for detail see '?geom_text'.
#' @examples
#' ## grouped barplot with error
#' plot_dat <- bl_calculate_grouped_mean_and_SE (bl_environmental_data, "snow_cover_duration", "sub_area", error_bar = T)
#'
#' p <- ggplot (plot_dat, aes (x = group_sub_area , y = snow_cover_duration)) +
#' geom_bar (stat = "identity", position = position_dodge2 (padding = 0.15), color = "black", fill = bl_blue(3), width = 0.7 ) +
#' geom_errorbar (aes (ymin = snow_cover_duration - SE_snow_cover_duration ,
#'               ymax = snow_cover_duration + SE_snow_cover_duration ),
#'               width = 0.3,
#'               position = position_dodge (0.7)) +
#' bl_y_scale (limits = c (0, 190), air = F) +
#' labs (x = "Sub area", y = "Snow cover duration (days)") +
#' bl_theme (legend.position = "none")
#'
#' bl_draw_plot (bl_add_title (p, title = "Snow cover duration in sub-areas", text_size = 12))
#'
#' @export
bl_add_title <-
    function (plot = ggplot2::last_plot(),
              title = "plot title",
              x = .5,
              y = .97,
              headroom = 0.13,
              expand_y = TRUE,
              xrel = TRUE,
              yrel = TRUE,
              font = "Chantilly",
              text_size = 14,
              align_lr = 0,
              align_tb = 1,


              ...) {

        if (is.null(layer_scales(plot)$y$limits)) {
            if (!is.numeric(layer_scales(plot)$y$range$range)) {
                yrel = FALSE
            }
        }
        if (is.null(layer_scales(plot)$x$limits)) {
            if (!is.numeric(layer_scales(plot)$x$range$range)) {
                xrel = FALSE
            }
        }

        pb <- ggplot_build(plot)

        if (isTRUE(expand_y) && isTRUE(yrel)) {

            y_for_expansion <- if (!is.null(headroom)) 1 - headroom else y

            scale_idx <- which(sapply(plot$scales$scales, function(s) {
                "y" %in% s$aesthetics
            }))

            # --- derive original y range, guarding against previous bl_add_title calls ---
            if (length(scale_idx) > 0) {

                # check if we already tagged this scale in a previous call
                if (!is.null(attr(plot$scales$scales[[scale_idx]], "bl_original_ymax"))) {
                    # use the stored original range
                    original_ymin <- attr(plot$scales$scales[[scale_idx]], "bl_original_ymin")
                    original_ymax <- attr(plot$scales$scales[[scale_idx]], "bl_original_ymax")
                } else if (!is.null(plot$scales$scales[[scale_idx]]$limits)) {
                    # explicit limits exist but not set by us — treat as original
                    original_ymin <- plot$scales$scales[[scale_idx]]$limits[[1]]
                    original_ymax <- plot$scales$scales[[scale_idx]]$limits[[2]]
                } else {
                    # no limits set — fall back to raw data range
                    original_ymin <- layer_scales(plot)$y$range$range[[1]]
                    original_ymax <- layer_scales(plot)$y$range$range[[2]]
                }

            } else {
                # no y scale at all — use raw data range
                original_ymin <- layer_scales(plot)$y$range$range[[1]]
                original_ymax <- layer_scales(plot)$y$range$range[[2]]
            }
            # -------------------------------------------------------------------------

            data_range <- original_ymax - original_ymin
            new_ymax   <- original_ymin + data_range / y_for_expansion

            if (length(scale_idx) > 0) {
                plot$scales$scales[[scale_idx]]$limits <- c(original_ymin, new_ymax)
                plot$scales$scales[[scale_idx]]$expand <- expansion(mult = c(0, 0), add = 0)
                # tag the scale with the original range so repeated calls don't re-expand
                attr(plot$scales$scales[[scale_idx]], "bl_original_ymin") <- original_ymin
                attr(plot$scales$scales[[scale_idx]], "bl_original_ymax") <- original_ymax
            } else {
                plot <- plot + scale_y_continuous(
                    limits = c(original_ymin, new_ymax),
                    expand = expansion(mult = c(0, 0), add = 0)
                )
                # tag the newly added scale
                new_idx <- which(sapply(plot$scales$scales, function(s) "y" %in% s$aesthetics))
                attr(plot$scales$scales[[new_idx]], "bl_original_ymin") <- original_ymin
                attr(plot$scales$scales[[new_idx]], "bl_original_ymax") <- original_ymax
            }

            # rebuild against final axis
            pb     <- ggplot_build(plot)
            y_lims <- pb$layout$panel_params[[1]]$y.range
        }

        # calculate y position
        if (isTRUE(yrel)) {
            y_lims <- pb$layout$panel_params[[1]]$y.range
            y_pos  <- (y_lims[[2]] - y_lims[[1]]) * y + y_lims[[1]]
        } else {
            y_pos <- y
        }

        # calculate x position
        if (isTRUE(xrel)) {
            x_lims <- pb$layout$panel_params[[1]]$x.range
            x_pos  <- (x_lims[[2]] - x_lims[[1]]) * x + x_lims[[1]]
        } else {
            x_pos <- x
        }

        # extract size from ... if present, otherwise use text_size
        dots <- list(...)
        if ("size" %in% names(dots)) {
            dots[["size"]] <- NULL  # remove from dots to avoid conflict
        }

        plot +
            do.call(cowplot::draw_label,
                    c(list(x          = x_pos,
                           y          = y_pos,
                           label      = title,
                           hjust      = align_lr,
                           vjust      = align_tb,
                           fontfamily = font,
                           size       = text_size),
                      dots))
    }
