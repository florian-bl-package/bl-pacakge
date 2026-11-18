#' bl grouped mean and standard error`
#'
#' @description bl_calculate_grouped_mean_and_SE takes a table of raw data and puts out a summary (mean and standard error) of a focal variable grouped by up to two grouping variables. The resulting table is especially useful for plotting barplots (e.g. using ggplot2).
#'
#' @param input_frame A data frame containing the raw data to be plotted.
#' @param focal_var The focal variable. Must be numeric. Can contain NAs, however, NAs always need a sensible approach.
#' @param group_var1 The first order groupng variable. Must be specified.
#' @param group_var2 Optional. A second order grouping variable.
#' @param order1 Optional. The order of the levels of group_var1
#' @param order2 Optional. The order of the levels of group_var2. Is ignored if group_var2 is not specified.
#' @param error_bar Logical. Should error bars be computed?
#' @examples #
#' morley
#' bl_calculate_grouped_mean_and_SE (input_frame = morley, focal_var = "Speed", group_var1 = "Expt")
#' #
#' #
#' #
#' mpg
#' bl_calculate_grouped_mean_and_SE (mpg, "cty", "drv", "class", error_bar = T, order1 = c ("f", "4", "r"))
#' #
#' #
#' #
#' library (tidyverse)
#' npk %>% bl_calculate_grouped_mean_and_SE ("yield", "N", "P", error_bar = T)
#'
#' plot_dat <- bl_calculate_grouped_mean_and_SE (npk, "yield", "N", "P", error_bar = T)

#' ggplot (plot_dat, aes (x = group_N, y = yield, fill = subgrp_P)) +
#' geom_bar (stat = "identity", position = "dodge", color = "black" ) +
#' geom_errorbar (aes (ymin = yield - SE_yield,
#'               ymax = yield + SE_yield),
#'               width = 0.3,
#'               position = position_dodge (.9)) +
#' scale_fill_manual (values = c (bl_blue (1), bl_blue (5))) +
#' bl_y_scale (limits = c (0, 65), air = F) +
#' theme (legend.position = "none") +
#' bl_theme ()
#'
#' ## no use without categorical variables:
#' ## nottem
#' ## bl_calculate_grouped_mean_and_SE (nottem, "Jan", "Feb")
#'
#' @importFrom magrittr %>%
#' @export


# perhaps this function should be updated: use dplyr::group_by and also glm-approach for mean and SE?

bl_calculate_grouped_mean_and_SE <- function (input_frame = NA,
                             focal_var = NA,
                             group_var1 = NA,
                             group_var2 = NA,
                             order1 = NA,
                             order2 = NA,
                             error_bar = F
                             ){

    if (!is.character(focal_var)) {
        stop ("focal_var must be of class character.")
    }

    if (!is.character(group_var1)) {
        stop ("group_var1 must be of class character.")
    }


# 2 groups (group and subgroup) #
    if (!is.na (group_var2)) {
        if (!is.character(group_var2)) {
            stop ("group_var2 must be of class character.")
        }

        if (anyNA (focal_var)) {
            warning ("focal variable contains NA values.")
        }

        group1 <- input_frame [[#noquote
            (group_var1)]]
        group2 <- input_frame [[#noquote
            (group_var2)]]
        focal  <- input_frame [[#noquote
            (focal_var)]]

        grouping <- dplyr::tibble (!! group_var1 := group1, !! group_var2 := group2, !! focal_var := focal)

        output_tab <- input_frame %>% dplyr::group_by (a = get (group_var1), b = get (group_var2)) %>%
                                            dplyr::summarize (focal_var = mean (get (focal_var), na.rm = T)) %>%
                                            dplyr::ungroup ()
        names (output_tab) [1:3] <- names (grouping)


# with errorbar
            if (isTRUE (error_bar)) {
                output_tab2 <- input_frame %>% dplyr::group_by (a = get (group_var1), b = get (group_var2)) %>%
                                               dplyr::summarize (focal_var = se (get (focal_var))) %>%
                                               dplyr::ungroup ()
                names (output_tab2) <- c (names (grouping [1:2]), paste0 ("SE_", names (grouping [3])))
                output_tab <- dplyr:: left_join (output_tab, output_tab2)
                output_tab <- output_tab [c (3, 4, 1, 2)]
                names (output_tab) [3] <- paste0 ("group_", names (output_tab [3]))
                names (output_tab) [4] <- paste0 ("subgrp_", names (output_tab [4]))

                # sort data frame by order (if specified) or by natural order
                    if (is.na (order1 [1])) {
                        ord1 <- factor(output_tab[[3]], levels = sort (levels (factor (output_tab[[3]]))))
                    } else {
                        ord1 <- factor(output_tab[[3]], levels = factor (order1, levels = order1))
                    }

                    if (is.na (order2 [1])) {
                        ord2 <- factor(output_tab[[4]], levels = sort (levels (factor (output_tab[[4]]))))
                    } else {
                        ord2 <- factor(output_tab[[4]], levels = factor (order2, levels = order2))
                    }

                output_tab [3] <- factor (output_tab [[3]], levels = levels (ord1))
                output_tab [4] <- factor (output_tab [[4]], levels = levels (ord2))

                output_tab <- output_tab [order(output_tab[[3]], output_tab[[4]]),]

# without errorbar
          } else {
                output_tab <- output_tab [c (3, 1, 2)]
                names (output_tab) [2] <- paste0 ("group_", names (output_tab [2]))
                names (output_tab) [3] <- paste0 ("subgrp_", names (output_tab [3]))


                    # sort data frame by order (if specified) or by natural order
                    if (is.na (order1 [1])) {
                        ord1 <- factor(output_tab[[2]], levels = sort (levels (factor (output_tab[[2]]))))
                    } else {
                        ord1 <- factor(output_tab[[2]], levels = factor (order1, levels = order1))
                    }

                    if (is.na (order2 [1])) {
                        ord2 <- factor(output_tab[[3]], levels = sort (levels (factor (output_tab[[3]]))))
                    } else {
                        ord2 <- factor(output_tab[[3]], levels = factor (order2, levels = order2))
                    }

                output_tab [2] <- factor(output_tab [[2]], levels = levels (ord1))
                output_tab [3] <- factor(output_tab [[3]], levels = levels (ord2))
                output_tab <- output_tab [order(output_tab[[2]], output_tab[[3]]),]
          }



#1 group #
    } else {
        group1 <- input_frame [[#noquote
            (group_var1)]]
        focal  <- input_frame [[#noquote
            (focal_var)]]


        grouping <- dplyr::tibble (!! group_var1 := group1, !! focal_var := focal)

        output_tab <- input_frame %>% dplyr::group_by (a = get (group_var1)) %>%
                                            dplyr::summarize (focal_var = mean (get (focal_var), na.rm = T)) %>%
                                            dplyr::ungroup ()

        names (output_tab) [1:2] <- names (grouping)


# with errorbar
            if (isTRUE (error_bar)) {
                output_tab2 <- input_frame %>% dplyr::group_by (a = get (group_var1)) %>%
                                               dplyr::summarize (focal_var = se (get (focal_var))) %>%
                                               dplyr::ungroup ()
                names (output_tab2) <- c (names (grouping [1]), paste0 ("SE_", names (grouping [2])))
                output_tab <- dplyr:: left_join (output_tab, output_tab2)

                output_tab <- output_tab [c (2, 3, 1)]
                #output_tab [3] <- factor(output_tab [3], levels = output_tab [3])

                    # sort data frame by order (if specified) or by natural order
                    if (is.na (order1 [1])) {
                        ord1 <- factor(output_tab[[3]], levels = sort (levels (factor (output_tab[[3]]))))
                    } else {
                        ord1 <- factor(output_tab[[3]], levels = factor (order1, levels = order1))
                    }

                output_tab [3] <- factor(output_tab [[3]], levels = levels (ord1))
                output_tab <- output_tab [order(output_tab[[3]]),]
                names (output_tab) [3] <- paste0 ("group_", names (output_tab [3]))
# without errorbar
            } else {
                output_tab <- output_tab [c (2, 1)]

                # sort data frame by order (if specified) or by natural order
                    if (is.na (order1 [1])) {
                        ord1 <- factor(output_tab[[2]], levels = sort (levels (factor (output_tab[[2]]))))
                    } else {
                        ord1 <- factor(output_tab[[2]], levels = factor (order1, levels = order1))
                    }
                output_tab [2] <- factor(output_tab [[2]], levels = levels (ord1))
                output_tab <- output_tab [order(output_tab[[2]]),]
                names (output_tab) [2] <- paste0 ("group_", names (output_tab [2]))
                }
    }

    return (output_tab)

}






se <- function (x) {
    sd(x, na.rm = T)/sqrt(length(x[!is.na(x)]))
}


############################

# plot_data <- mpg %>% dplyr::group_by (cyl, class) %>% dplyr::summarize (fuel = mean (hwy)) %>% dplyr::ungroup ()
# max <- plot_data %>% dplyr::group_by (gear) %>% top_n(1)
# labels <-   dplyr::tibble (gear = max$gear, y_pos = max$fuel, lab = c("*", "n.s.", "***"))
#
# ggplot (plot_data,
#                 aes (x = gear, y = fuel)) +
#     geom_col (position = "dodge2",
#                 aes (fill = factor (vs))) +
#     scale_fill_manual (values = c (bl_blue(1),
#                                    bl_blue(5))) +
#     geom_text (data = labels,
#                 aes (y = y_pos + 5,
#                      label = lab),
#                family = "ChantillyLH", size = 8) +
#     bl_theme()
#
#
#
#
# plot_data2 <- mtcars %>% dplyr::group_by (gear) %>% dplyr::summarize (fuel = mean (mpg)) %>% dplyr::ungroup ()
# max2 <- plot_data2 %>% dplyr::group_by (gear) %>% top_n(1)
# labels2 <-   dplyr::tibble (gear = max2$gear, y_pos = max2$fuel, lab = c("a", "b", "c"))
#
#
# ggplot (plot_data2, aes (x = gear, y = fuel)) +
#     geom_col (position = "dodge2", fill = bl_blue(3)) +
#     geom_text (data = labels2, aes (y = y_pos + 5, label = lab), family = "ChantillyLH", size = 16)
