#' bl_make_regression_plot
#'
#' bl_make_all_regression_plots
#'
#' @description function to create a scatterplot of original data alongside with regression curve + confidence interval. bl_make_all_regression_plots is used to iterate over all fixed effects in a given model.
#'
#' @usage bl_make_regression_plot(
#'  dataset,
#'  response,
#'  fixed,
#'  predictions,
#'  xlab = NA,
#'  ylab = NA,
#'  point_color = bl_blue(3),
#'  point_pch = 21,
#'  point_size = 2,
#'  reg_line_color = bl_blue(3),
#'  CI_color = "#cfdae6",
#'  jitter = F
#'  )
#'
#'
#'  bl_make_all_regression_plots(
#'  model,
#'  list_of_predicitons,
#'  varnames = NA,
#'  jitter = F,
#'  point_color = bl_blue(3),
#'  point_pch = 21,
#'  point_size = 2,
#'  reg_line_color = bl_blue(3),
#'  CI_color = "#cfdae6"
#'  )
#'
#' @param dataset A dataset.frame wih the original dataset that has been modeled using a regression model.
#' @param response Name of the response variable in the model
#' @param fixed Name of the fixed effect that shall be drawn
#' @param predictions predictions table (can be obtained using 'bl_predict_regression_models')
#' @param xlab x-Axis label text
#' @param ylab y-Axis label text
#' @param point_color color of the points (Default: bl_blue(3))
#' @param point_pch Point shape (Default: 21)
#' @param point_size Point size (default: 2)
#' @param reg_line_color Color of the regression curve (Default: bl_blue(3))
#' @param CI_color Color of the confidence interval. Note that transparency might not work properly when exporting. Default: "#cfdae6" which is imitates setting bl_blue(3) transparent.
#' @param jitter adds  jitter. Can be FALSE (no jitter), TRUE (predifined jitter) or a vector of length 2 defining vertical and horizontal jitter, respectively. NOT IMPLEMENTED: Alternatively, 'symmetric' creates a symmetric jitter based on 'bl_symmetric_jitter ()'
#' @param model A regression model
#' @param list_of_predicitons A list of model predictions including CIs. Can be created with 'bl_predict_regression_models'.
#' @param varnames Either NA (then the names of the fixed effects are used as x-Axis label), a vector of variable names to be used as x-Axis labels or a reference dataset.frame for 'bl_replace' to replace dummy names with final varnames.

#' @export
#' @examples
#' # a univariable model
#' univar_model <- lme4::glmer(grasshopper ~ elevation + (1|sub_area), data = bl_ecology_data, family = "poisson")
#' summary (univar_model)
#' # create regression plot
#' bl_make_regression_plot(dataset = bl_ecology_data,
#'                         response = "grasshopper",
#'                         fixed = "elevation",
#'                         predictions = bl_predict_regression_models(univar_model))
#'
#' ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#' # a multivariable model
#' multivar_model <- lme4::glmer(grasshopper ~ elevation + heat_load + bare_ground + (1|sub_area), data = bl_ecology_data, family = "poisson")
#'
#' # create multiple regression plots
#' mulivar_regression_plots <- bl_make_all_regression_plots(multivar_model, bl_predict_regression_models(multivar_model))
#'
#' # draw multiple regression plots in one graph (facetted)
#' bl_draw_plot(mulivar_regression_plots)

# bl_make_regression_plot: make a scatterplot of original data alongside with regression + confidence interval ####

bl_make_regression_plot <- function (dataset,
                                 response,
                                 fixed,
                                 predictions,
                                 xlab = NA,
                                 ylab = NA,
                                 point_color = bl_blue (3),
                                 outer_point_color = NA,
                                 point_pch = 21,
                                 point_size = 2,
                                 reg_line_color = bl_blue (3),
                                 CI_color = bl_blue("CI"),
                                 #jitter = F,
                                 count_overlays = "size"# may be "jitter", "jitter_x" or "jitter_x_y" (where x and y are the amount of x and y jitter) "size" or FALSE.
                                 ) {
    # check outer_point_color
    if (is.na (outer_point_color)) {
        outer_point_color <- point_color
    }

    # check for axis labels
    if (is.na (xlab)) {
            xlab = fixed
    }
    if (is.na (ylab)) {
                ylab = response
    }



    ## check for action with overlaying points
    # version one: FALSE --> don't account for overlaying points
    if (count_overlays == F) {
        the_points <- geom_point (mapping = aes (x = .data[[fixed]], y = .data[[response]]),
                                  fill = point_color, color = outer_point_color, pch = point_pch, size = point_size[1]
                                  )
    }

    # version two: "size" --> adjust point size based on number of overlaying points
    if (count_overlays == "size") {
        the_points <- geom_count (mapping = aes (x = .data[[fixed]], y = .data[[response]]),
                                  fill = point_color, color = outer_point_color, pch = point_pch
                                  )
    }

    # version three: "jitter" --> add some jitter to the points
    if (grepl("jitter", count_overlays)) {
        # Split the string
        split_parts <- strsplit(count_overlays, "_")[[1]]

        # Assign variables
        a <- split_parts[1]
        x_jitter <- as.numeric(split_parts[2])
        if (is.na(x_jitter)) {
            x_jitter <- 0.5
            y_jitter <- 0.5
        } else {
            y_jitter <- as.numeric(split_parts[3])
            if (is.na(y_jitter)) {
                y_jitter <- 0
            }
        }

        #  make geom_jitter argument
        the_points <- geom_jitter(mapping = aes (x = .data [[fixed]], y = .data[[response]]),
                                  fill = point_color, color = outer_point_color, pch = point_pch, size = point_size[1],
                                  width = x_jitter, height = y_jitter)

    }

    # sonderfall: symmetric. make own jitter data
    if (count_overlays == "symmetric_jitter") {
                    the_points <- geom_point (mapping = aes (x = .dataset [[fixed]], y = bl_symmetric_jitter(data_table = dataset,
                                                                                                           variable = response,
                                                                                                           co_var = fixed)),
                    fill = point_color, color = outer_point_color, pch = point_pch, size = point_size)
    }





    # point size
        #
        if (count_overlays == F) {
            point_size <- c (point_size[1], point_size[1])
        }

        if (count_overlays == "size") {
            # Calculate the count of points at each location
            point_counts <- table(paste(dataset [[fixed]], dataset [[response]])) %>% as.data.frame()
            min_overlays <- min (point_counts$Freq)
            max_overlays <- max (point_counts$Freq)
            # if point size has length 1
            if (length (point_size)==1) {
                the_size <- scale_size_continuous(range = c(min_overlays*0.5, max_overlays*point_size)/25)
            # if point size ha length 2
            } else {
                the_size <-  scale_size_continuous(range = c(min_overlays*point_size[1], max_overlays*point_size[2])/25)
            }
        # else make a placeholder
        } else {
            the_size <- theme()
        }


    # prep preditions
    pred_name <- names (predictions)
    predictions <- as.data.frame(predictions)
    names(predictions) <- gsub(paste0(pred_name, "."), "", names(predictions))
    # create plot
    reg_plot <- ggplot (dataset) +

        # draw ribbon (= CI) [draw first with exact color and plot the rest on top]
        geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high), data = predictions, fill = CI_color) +

        #draw regression line
        geom_line (aes(x = x, predicted), data = predictions, col =  reg_line_color) +

        # draw scatterplot
        the_points +

        # add size argument for overlaying points (only affects the plot when "count_overlays == "size")
        the_size +

        # add legend argument
        # the_legend +

        # aesthetics and theme
        xlab(xlab) +
        ylab(ylab)+

        bl_clean_theme +
        theme (legend.position = "none")

    # return plit
    return (reg_plot)
}











#' bl_make_all_regression_plots
#'
#' @description make list of scatterplots of original data alongside with regression + confidence intervals based on bl_make_regression_plot
#'
#' @usage bl_make_all_regression_plots(
#'  model,
#'  list_of_predicitons,
#'  varnames = NA,
#'  ...
#'  )
#'

#' @param model A regression model
#' @param list_of_predicitons A list of model predictions including CIs. Can be created with 'bl_predict_regression_models'.
#' @param varnames Either NA (then the names of the fixed effects are used as x-Axis label), a vector of variable names to be used as x-Axis labels or a reference dataset.frame for 'bl_replace' to replace dummy names with final varnames.
#' @export
#' @examples
#' ### ### ### ### ### ### ### ### ### ### ### ### ### ###
#' # a multivariable model
#' multivar_model <- lme4::glmer(grasshopper ~ elevation + heat_load + bare_ground + (1|sub_area), data = bl_ecology_data, family = "poisson")
#'
#' # create multiple regression plots
#' mulivar_regression_plots <- bl_make_all_regression_plots(multivar_model, bl_predict_regression_models(multivar_model))
#'
#' # draw multiple regression plots in one graph (facetted)
#' bl_draw_plot(mulivar_regression_plots)


bl_make_all_regression_plots <- function (model,
                                         list_of_predicitons,
                                         varnames = NA,
                                         ...
                                         # jitter = F,
                                         #
                                         # point_color = bl_blue (3), point_pch = 21, point_size = 2,
                                         # reg_line_color = bl_blue (3), CI_color = "#cfdae6"
                                         ) {

    # names of fixed effects
    fixed_effects <- names (list_of_predicitons)

    # get dataset table from the model
    model_dataset <- model.frame(model)

    # get response variable
    the_response <- bl_get_model_coefficients(model)[[1]]

    # check for ylab
    if (!"ylab" %in% names(list(...))) {
            # Append an element to the call
            call <- match.call()
            call$ylab <- the_response
    }

    # make variable names for the x-Axis
    if (all(is.na(varnames))) {
        varnames = fixed_effects
    } else if (is.vector(varnames)) {
        varnames = varnames
    } else if (is.data.frame(varnames)) {
        varnames = bl_replace(fixed_effects, varnames)
    }




    # make list of scatterplits with regression curve and confint
    plot_list <- list ()
    for (i in fixed_effects) {
    plot_list [[i]] <- bl_make_regression_plot (dataset = model_dataset, response = the_response, fixed = i,
                                            predictions = list_of_predicitons[[i]],
                                            xlab = varnames [[match (i, fixed_effects)]], #ylab = ylab,
                                            ...
                                            # jitter,
                                            # point_color = point_color, point_pch = point_pch, point_size = point_size,
                                            # reg_line_color = reg_line_color, CI_color = CI_color
                                            )
    }
    return (plot_list)
}
