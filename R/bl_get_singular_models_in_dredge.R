#' bl_get_singular_models_in_dredge
#'
#' @description bl function that shows all models that suffer from singularity in a dredge object.
#'
#' @param dredge_object a dredge object (from MuMIn package)
#' @param AICc_delta all models with smaller AICc are considered
#' @param max_number_of_models maximum n of models that are considered. If there are more models with AICc smaller than AICc_delta, the n top-ranked models are considered

#' @examples

#'
#'
#' @export


# function to display (non)singularity in models in dredge
bl_get_singular_models_in_dredge <- function(dredge_object, AICc_delta = NA, max_number_of_models = 10){
    # Check if AICc_delta is provided
    if (is.na(AICc_delta)) {
        # Issue a warning
        cat("\n\n\033[32mNo AICc_delta set. Considering all models in dredge object. Do you want to proceed? (y/n)\033[0m\n")
        cat("\033[34mType 'y' to proceed, or 'n' to cancel: \033[0m")
        # Prompt the user for input
        answer <- readline("\033[34mType 'y' to proceed, or 'n' to cancel: \033[0m")

        # Loop until a valid input ('y' or 'n') is given
        while(!(answer %in% c("y", "n"))) {
            answer <- readline("\033[34mInvalid input. Please type 'y' for yes or 'n' for no: \033[0m")
        }

        # Check the answer and act accordingly
        if (answer == "y") {
            cat("Proceeding with the operation...\n")
            # Add the code for the operation here
            model_subset <- dredge_object
        } else {
            cat("Operation canceled by user.\n")
            invokeRestart("abort")
        }
    }
    else {
        # Select models within AICc_delta
        model_subset <- get.models(dredge_object, subset = delta <= AICc_delta)
    }

    # check for singularity of all models
    singularity <- data.frame("model_name" = NA, "is_singular" = NA)  # Initialize an empty character vector to collect all variable names
    for (i in seq_along(model_subset)) {
        model <- model_subset[[i]]  # Get the model
        model_name <- paste0("Model_", names(model_subset)[i])  # Label each model

        # check for singularity and store in list
        singularity[[i, "model_name"]] <- model_name
        singularity[[i, "is_singular"]] <- lme4::isSingular(model)
    }
    return(singularity)
}
