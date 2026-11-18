#' bl significance codes
#'
#'
#'
#' @param signi_codes A character vector of length 1 or the number of groups displayed in the barplot. This is the text to be plotted above the bars
#' @param bar "ontop" or "between". Shall the annotations be drawn on top of the (grouped) bars or between the bars / bar groups?
#' @param distance A numeric vector of length 1 or the number of groups displayed in the barplot. Adjusting for different label types may be useful when different types of text shall be displayed (e.g. 0.9 for "n.s." and 0.1 for "*"). This is the distance of the text to the bars, or - if present - error bars.
#' @param font A character vector of length 1 or the number of groups displayed in the barplot. Can be any font known to your R. Each label can have their own font (which is not advicably in most cases).
#' @param text_size A numeric vector of length 1 or the number of groups displayed in the barplot. As fot the distance, can be adjusted for different types of text (e.g. 6 for "n.s." and 8 for "*").
#'
#' @examples
#'## grouped barplot with error
#' plot_dat <- bl_calculate_grouped_mean_and_SE (bl_environmental_data, "snow_cover_duration", "sub_area", error_bar = T)
#'
#' p2 <- ggplot (plot_dat, aes (x = group_sub_area , y = snow_cover_duration)) +
#' geom_bar (stat = "identity", position = position_dodge2 (padding = 0.15), color = "black", fill = bl_blue(3), width = 0.7 ) +
#' geom_errorbar (aes (ymin = snow_cover_duration - SE_snow_cover_duration ,
#'               ymax = snow_cover_duration + SE_snow_cover_duration ),
#'               width = 0.3,
#'               position = position_dodge (0.7)) +
#' bl_y_scale (limits = c (0, 175), air = F) +
#' labs (x = "Sub area", y = "Snow cover duration (days)") +
#' bl_theme (legend.position = "none")
#'
#' p2
#'
#' p2 + bl_add_significance_codes(c ("a", "a", "a", "b", "b", "a"))
#'
#'
#' @export
bl_add_significance_codes <- function (signi_codes = NA,
                             bar = "ontop",
                             distance = 0.4,
                             font = "ChantillyLH",
                             text_size = 5,
                             ...) {

    signi_codes <- as.character (signi_codes)

# warnings
    if (is.na (signi_codes [1])) {
        warning ("no significance codes given")
    }


# annotation above each bar:
    the_text <- geom_text (data  =  function(x) {
        # test if data frame has a SE column
        if (length (x[ , grepl( "SE" , names( x ) ) ]) > 0) {
            SE <- TRUE
        } else {
            SE <- FALSE
        }

        # test whether data frame has a subgroup-column
        if (length (x[ , grepl( "subgrp" , names( x ) ) ]) > 0) {
            subgrp <- TRUE
        } else {
            subgrp <- FALSE
        }

        renamed_frame <- x
        if (SE == T) {
            if (subgrp == T) {
                names (renamed_frame) <- c ("value", "se", "group", "subgrp")
            } else {
                names (renamed_frame) <- c ("value", "se", "group")
            }

        } else {
            if (subgrp == T) {
                names (renamed_frame) <- c ("value", "group", "subgrp")
            } else {
                names (renamed_frame) <- c ("value", "group")
            }

        }


        label_y <- renamed_frame %>%
                        dplyr::group_by (group) %>%
                        dplyr::summarize (place = if (SE == T){
                            max (value + se)
                        } else {
                            max (value)
                        }
                            )


        if (bar == "between") {
            label_y <- label_y %>% dplyr::top_n(1, place)
            label_y ["group"] <- 1.5

            label_y [3] <- 1.5
        }


        return (label_y)
    },
    mapping = aes (label = signi_codes, x = group, y = place),
    vjust = - distance,
    family = font,
    size = text_size,
    inherit.aes = F)

    return (the_text)
}



#' bl significance codes for grouped barplots.
#' @param signi_codes A character vector of length 1 or the number of groups displayed in the barplot. This is the text to be plotted above the bars
#' @param distance A numeric vector of length 1 or the number of groups displayed in the barplot. Adjusting for different label types may be useful when different types of text shall be displayed (e.g. 0.9 for "n.s." and 0.1 for "*"). This is the distance of the text to the bars, or - if present - error bars.
#' @param font A character vector of length 1 or the number of groups displayed in the barplot. Can be any font known to your R. Each label can have their own font (which is not advicably in most cases).
#' @param text_size A numeric vector of length 1 or the number of groups displayed in the barplot. As fot the distance, can be adjusted for different types of text (e.g. 6 for "n.s." and 8 for "*").
#'
#' @examples
#' plot_dat <- bl_calculate_grouped_mean_and_SE (npk, "yield", "N", "P", error_bar = T)
#'
#' p <- ggplot (plot_dat, aes (x = group_N, y = yield, fill = subgrp_P)) +
#' geom_bar (stat = "identity", position = "dodge", color = "black", width = .5 ) +
#' geom_errorbar (aes (ymin = yield - SE_yield,
#'               ymax = yield + SE_yield),
#'               width = 0.3,
#'               position = position_dodge (.5)) +
#' bl_y_scale (limits = c (0, 70), air = F) +
#' labs (x = "N", y = "Yield") +
#' theme (legend.position = "none") +
#' bl_theme ()
#'
#' p
#'
#' p + bl_add_significance_codes_grouped (c ("n.s.", "*"),
#'                        text_size = c (6,8),
#'                        distance = c (0.9, 0.1))
#'
#'
#' @export
# bl_add_significance_codes_grouped <- function (signi_codes = NA,
#                                distance = 0.3,
#                                font = "ChantillyLH",
#                                text_size = 6
# ){
#     if (is.na (signi_codes [1])) {
#         stop ("no significance codes provided")
#     }
#         geom_text (data =
#                        function(x) {
#         label_y <- dplyr::tibble ()
#         # test if data frame has a SE column
#         if (length (x[ , grepl( "SE" , names( x ) ) ]) > 0) {
#         SE <- TRUE
#         } else {
#         SE <- FALSE
#         }
#         renamed_frame <- x
#         if (SE == T) {
#           names (renamed_frame) <- c ("value", "se", "group", "subgroup")
#         } else {
#           names (renamed_frame) <- c ("value", "group", "subgroup")
#         }
#
#
#
#         label_y <- renamed_frame %>%
#                         dplyr::group_by (group) %>%
#                         dplyr::summarize (place = if (SE == T){
#                             max (value + se)
#                         } else {
#                             max (value)
#                         }
#                             )
#
#         if (subgrp == T) {
#             label_y $ subgrp <- levels(x  [ , grepl ("subgrp", names (x) ) ] [[1]])
#         }
#
#
#         # label_y <- renamed_frame %>% dplyr::group_by (group) %>% dplyr::summarize (max_var = max (value), max_se = max (se))
#         #
#         #
#         # if (SE == T) {
#         #     label_y $ place <- label_y $ max_var + label_y $ max_se
#         # } else {
#         #     label_y $ place <- label_y $ max_var
#         # }
#
#
#         ## label_y [x [ , grepl ("group", names (x) ) ] %>% names ()] <- levels(x  [ , grepl ("group", names (x) ) ] [[1]])
#         ## label_y [x [ , grepl ("subgrp", names (x) ) ] %>% names ()] <- levels(x  [ , grepl ("subgrp", names (x) ) ] [[1]])
#         ## label_y [names (x [1])] <- label_y $ place
#         return (label_y)
#     },
#     mapping = aes (label = signi_codes, x = group, y = place, fill = subgrp),
#     vjust = - distance,
#     family = font,
#     size = text_size,
#     inherit.aes = F)
# }
#
#
# # plot_dat <- bl_calculate_grouped_mean_and_SE (npk, "yield", "N", "P", error_bar = T)
# #
# # p <- ggplot (plot_dat, aes (x = N, y = yield, fill = P)) +
# # geom_bar (stat = "identity", position = "dodge", color = "black", width = 0.5 ) +
# # geom_errorbar (aes (ymin = yield - SE_yield,
# #               ymax = yield + SE_yield),
# #               width = 0.3,
# #               position = position_dodge (.5)) +
# # scale_fill_manual (values = c (bl_blue (1), bl_blue (5))) +
# # bl_y_scale (limits = c (0, 65), air = F) +
# # theme (legend.position = "none") +
# # bl_theme ()
#
#
# #
# # label.df <- data.frame(N = c("0","1"),
# #                        P = c ("0", "1"),
# #                        yield = c (58, 62))
# #
# # p + geom_text (data = label.df, aes (label = c ("n.s.", "***"), y = yield))
