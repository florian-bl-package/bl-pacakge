#' bl_get_correlations
#'
#' @description bl function to quickly get beautiful correlation tables using Spearman correlation.
#'
#' @param data_table a data.frame of numeric variables
#' @param show_only_significant logical. Should only significant correlations be displayed? Defaults to TRUE
#' @param show_threshold Numeric. A threshold, only correlations stronger than the threshold are displayed. Defaults to 0.
#' @param show_significance Logical. Should the significance be indicated with "**" symbolys? Defaults to TRUE
#' @param raw Logical. If TRUE, the output consists of the raw r and p value tables. Defaults to FALSE
#' @param self_sign Character. A character that should be displayed in the diagonal of the correlations table; that is the self-correlation of each parameter. Default to "/".
#' @param lower_empty Logical. Should the cells under the diagonal be empty? Defaults to TRUE
#' @examples
#' bl_environmental_data %>%
#'     select(-c ("sub_area", "land_use_type")) %>%
#'     bl_get_correlations()
#'
#'
#' @export

bl_get_correlations <- function (data_table,
                              show_only_significant = T,
                              show_threshold=0,
                              show_significance = T,
                              raw = F,
                              self_sign = "/",
                              lower_empty = T
                              ) {
    # make a basic correlations table
    cor_tab <- Hmisc::rcorr (as.matrix(data_table), type = "spearman")
    cor_r <- as.data.frame (cor_tab $ r)

    cor_p <- as.data.frame (cor_tab $ P)
    if (raw == T) {
        correlations = list ("Raw corralation coefficients" = cor_r, "Raw p-values of the correlations" = cor_p)
    } else {
        # convert p values to stars
        cor_p_star <- bl_convert_P_to_stars(cor_p)

        # get indices of values to replace (then round; then replace)
        # Finde the values smaller than the "show_threshold"
        idy <- apply(cor_r, MARGIN = 2, abs) < show_threshold
        # Find the values in cor_r that are == 1
        set_ <- cor_r==1
        # Find the values in cor_p that are smaller than 0.05
        idx <- cor_p > 0.05

        # round cor_r
        cor_r <- cor_r%>% round (digits = 2)


        # replace values (if applicable)
        # replace values in cor_r and cor_p where cor_r < threshold
        cor_r[idy] <- "·"
        cor_p[idy] <- ""
        cor_p_star[idy] <- ""
        # replace "1" in cor_r with "/"
        cor_r[set_] <- self_sign
        cor_p[is.na (cor_p)] <- ""
        # optionally replace non-significant values
        if (show_only_significant == T) {
            # Replace the corresponding values in both data frames with "." or ""
            cor_r[idx] <- "·"
            cor_p[idx] <- ""
        }

        #make the results data.frame
        correlations <- data.frame (row.names = names (cor_r))
        if (show_significance == T) {
            for (i in seq_along (cor_r)) {
                correlations [names (cor_r)[[i]]] <- paste0 (cor_r[[i]], cor_p_star[[i]])
            }
        } else {
            correlations <- cor_r
        }

        # delete values in the lower triangle (these double with those in the upper triangle)
        if (lower_empty == T) {
            correlations[lower.tri(correlations)] <- ""
        }
    }


    return (correlations)
}

