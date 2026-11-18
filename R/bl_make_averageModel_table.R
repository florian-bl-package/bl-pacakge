#' bl_make_averageModel_table
#'
#' @description takes an averaged model from MuMIn package and puts out a "beautiful" table
#'
#' @param average_model an averaged model from MuMIn package. Experimentally, single models can also be used (e.g. single best models from dredge)
#' @param type 'full' or 'conditional'/'subset' for full or conditional average
#' @param P the type of representation of the P value. "stars" (the default) puts out '*' symbols, "round" uses beautiful rounding, integers set the number of digits.
#' @param SE sets the standard error that should be displayed, the non-adjusted ("raw") or the adjusted ("adjusted"; the default) standard error.
#' @param ordering a vector determining the order of the output table
#' @export
#' @examples
#' # load packages
#' library (MuMIn)
#' library (lme4)
#' # calculate global model with all independent variables
#' global_model <- lmer (mpg ~ hp + drat + (1|cyl ),
#'                       data = mtcars)
#' # set na.action for dredge
#' options (na.action = na.fail)
#'
#' # perform dredge on global model
#' cars_dredge <- dredge(global_model)
#'
#' # calculate average model from best-ranked models
#' cars_avg <- model.avg(cars_dredge, subset = delta < 2, fit=TRUE)
#' summary (cars_avg)
#'
#' # make nice table out of average model
#' bl_make_averageModel_table (cars_avg)
#' bl_make_averageModel_table (cars_avg, type = "conditional")

# bl_make_averageModel_table: takes an averaged model and puts out a "beautiful" table####
bl_make_averageModel_table <- function (average_model, type = "full", P = "stars", SE = "adjusted",
                                     ordering = NA) {
    # check if average model is an average model
    if(class(average_model) == "averaging") {
        # make summary table - either "full average" or "conditional average"
        if (type == "full") {
            avg_summary <- summary (average_model)$coefmat.full %>% as.data.frame()
            the_text <- "Full average"
        }
        if (type == "conditional" | type == "subset") {
            avg_summary <- summary (average_model)$coefmat.subset %>% as.data.frame()
            the_text <- "Conditional average"
        }
    } else {
        avg_summary <- summary(average_model)$"coefficients" %>% as.data.frame()
        cat("The input model is not an average model. The function tries to make summary table for single (best?) model instead. \n")
    }


    # add 'Variable' row
    avg_summary <- avg_summary %>% tibble::add_column ("Variable" = row.names(avg_summary), .before=1)

    # sort avg table according to 'ordering'
    if (!is.na (ordering[[1]])) {
        avg_summary <- bl_order(input = avg_summary, order_column = "Variable", ordering = ordering, factor_levels = T, complete = )
    }

    # make beautiful table
    avg_help_table <- as.data.frame (avg_summary)

    # convert
    if (P == "stars") {
        avg_help_table$`Pr(>|z|)` <- lapply (avg_help_table$`Pr(>|z|)`, bl_convert_P_to_stars, n_s = "n.s.")
    }
    if (P == "round") {
        avg_help_table$`Pr(>|z|)` <- lapply (avg_help_table$`Pr(>|z|)`, bl_round_beautifully())
    }
    if (is.numeric(P)) {
        avg_help_table$`Pr(>|z|)` <- lapply (avg_help_table$`Pr(>|z|)`, round, digits = round (P, digits = 0))
    }
    if (is.na (P)) {
        avg_help_table$`Pr(>|z|)` <- lapply (avg_help_table$`Pr(>|z|)`, round, digits = 7)
    }

    # choose standard error based on 'SE' specification
    if(class(average_model) == "averaging") {
        if (SE == "adjusted") {
            my_SE <- avg_help_table$`Adjusted SE`
        }
        if (SE == "raw") {
            my_SE <- avg_help_table$`Std. Error`
        }
    } else {
        my_SE <- avg_help_table$`Std. Error`
    }


    # create export table by pasting everything together
    avg_export_table <- data.frame (Variable = avg_help_table$Variable, `Estimate ± SE` = paste0 (round(avg_help_table$Estimate, 2), " ± ", round(my_SE, 2)),
                                   `P` = avg_help_table$`Pr(>|z|)`%>%unlist())
    # make useful column names
    colnames(avg_export_table) <- c ("Variable","Estimate ± SE", "P")
    return (avg_export_table)
}




