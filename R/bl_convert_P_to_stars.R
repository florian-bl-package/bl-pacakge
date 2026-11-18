#' bl_convert_P_to_stars
#'
#' @description function to convert P values into significance codes (*** symbols)
#'
#' @param x A numeric vector
#' @param n_s Hide (FALSE, the default) or show (TRUE) not siginificant values. A character vector can also be specified; e.g. "." puts out points as a not-significant marker.
#' @examples
#' # convert a vector p-values to stars:
#' set.seed (364)
#' p_values <- sample (seq (0.00001, 0.1, length.out = 100), 10)
#'
#' significance_codes <- bl_convert_P_to_stars(p_values, n_s = T)
#' p_value_table <- tibble (p_values = p_values,
#'                          chars = significance_codes)
#'
#' p_value_table

#' @export





bl_convert_P_to_stars <- function (x, n_s = F) {
    if (is.data.frame(x)) {
        results <- x
        for (i in 1:length (colnames(x))){
            results [[i]] <- basic_star_function (x[[i]], n_s = n_s)
        }
    } else {
        results <- basic_star_function (x, n_s = n_s)
    }
    return (results)
}





basic_star_function<- function (z, n_s=F) {
    P_stars <- c()
    for (i in 1:length (z)) {

        y <- z [[i]]
        # check if z == NA
        if (is.na (z[[i]])) {
            y <- NA
        }

        # check if z is not numeric
        if (!is.numeric(z[[i]])) {
          # Try to create a numeric 'y' from 'z'
          y <- suppressWarnings (try(as.numeric(z[[i]]), silent = TRUE))
        }

        # # probably not needed
        # if (!is.numeric(y)) {
        #     y <- 1
        #     warning ("z is not numeric and cannot be converted to numeric.")
        # }

        # Check if 'y' is NA
        if (is.na (y)) {
            if (n_s == F) {
                P_stars [i] <- ""
                } else {
                P_stars [i] <- n_s
                }
        } else {
            y = z[[i]]


            if (is.numeric(y)) {
                if (y <= 0.001) {
                P_stars [i] <- ("***")}
                else if (y <= 0.01){
                    P_stars [i] <- ("**")}
                else if (y <= 0.05){
                    P_stars [i] <- ("*")
                }
                else if (n_s == F) {
                    P_stars [i] <- ("")
                }
                else if (n_s == T){
                    P_stars [i] <- ("n.s.")
                }
                else {
                    P_stars [i] <- (n_s)
                }
            }
        }
        suppressWarnings (rm (y))
    }
    return (P_stars)


}
