#' bl_round_beautifully
#'
#' @description function to beautifully round numbers based on  significant digits
#'
#' @param x A number
#' @param last Logical: Should the number be rounded in the same way as the previous number? Useful for tables where numbers with different numbers of significant digits should be combined; eg 0.2325 ± 0.0089 shoul be 0.23 ± 0.01. The second number in this case uses the same amount of rounding digits as the first number. If "TRUE", throws an error when no other number has been rounded previously. This is mainly intended to be used within functions where the previous rounding-processes can be controlled more efficiently than in plain code.
#' @examples
#' # check out differenct behaviour with respect to significant digits
#' bl_round_beautifully(0.078346587263)
#' bl_round_beautifully(0.78346587263)
#' bl_round_beautifully(7.8346587263)
#' bl_round_beautifully(78.346587263)
#'
#' # setting last=T makes the function react to the last time it was called
#' bl_round_beautifully(9.87767, last=T)
#' bl_round_beautifully(9.87767, last=F)
#'
#' bl_round_beautifully(0.078346587263)
#' bl_round_beautifully(29.87767, last=T)
#' bl_round_beautifully(29.87767, last=F)
#'
#' # can also handle vectors
#' bl_round_beautifully(c(0.008547, 475, 34.56, 1.566))
#' @export

bl_round_beautifully <- function(x, last = F, max_dgts = 4) {
    # test whether object is single number or vector
    if (is.numeric(x) && length(x) == 1 && last == T){
        if (!exists ("last_round_digits")) {
            return ("no last value stored")
        } else {
            return (format (round (x, last_round_digits), nsmall = last_round_digits) %>% as.numeric ())
        }
    }
    # abfrage ob kommastellen gespeichert wurden; wenn ja, nutze und lösche kommastellen

     else {
         round_values <- rep ("NA", length (unlist (x)))
         for (i in 1 : length (unlist (x))) {
             round_values [i] <- basic_bl_rounder(unlist(x)[i], tsh = max_dgts)
             }
         }


            return (round_values %>% as.numeric)
            #speichere kommastellen in globalenv
    #}
}



basic_bl_rounder <- function (z, tsh = 4) {
    if (is.numeric(z) && !is.na (z)) {
        digits <- (log10(abs(z))) + 1

        if (digits < -2) {
            round_digits <- 4
        } else if (digits < -1) {
            round_digits <- 3
        } else if (digits < 1) {
            round_digits <- 2
        } else if (digits < 3) {
            round_digits <- 1
        } else {
            round_digits <- 0
        }

        # set max number of digits for output (if should be smaller than 4)
        if(round_digits > tsh){
            round_digits <- tsh
        }

        assign ("last_round_digits", round_digits, envir = .GlobalEnv)
        round_value <- format (round (z, digits = round_digits), scientific = FALSE, nsmall = round_digits)
    } else {
        round_value <- NA
    }
    return (round_value)
}
