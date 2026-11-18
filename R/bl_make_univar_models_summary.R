#' bl_make_univar_models_summary
#'
#' @description bl function to make quick overview tables of a set of univariable models (GLM or GLMM) using the same dependent variable and different independent variables. All models use the same family.
#'
#' @param data_table a dataframe with a dependent and at least one independent variables
#' @param dependent_var a character string of the dependent variable name
#' @param independent_vars a vector of character strings of the independent variable names
#' @param random_formula optional: if a GLMM should be calculated, a character string in form of a random formula
#' @param offset_var optional: a character string of the offset variable name
#' @param model_family the model family in GLM or GLMM, sse ?GLM and ?lme4::glmer. must be
#' @param null_model for R² calculation, a null model can be specified. Especially usefull for mixed models, where conditional R² computation may require a null model.
#' @param tc_off logical. if model_family == "binomial", tc_off turns off separate columns for treatment and control cases
#' @param do_scale logical. should numerical independent variables be scaled for Estimate calculation?
#' @param P_star logical. should P values be translated into "**" stars?
#' @param mean logical. If true, variable means ± standard errors are added to the output table
#' @param error_type either "SE" or "SD"
#' @param R_2 logical. should r-squared values be added to the output table?
#' @param AUC logical. should AIC values be added to the output table?
#' @param max_digits integer. indicates the maximum of digits (up to 4).
#' @examples
#' # check out the warning
#' univar_models_summary <-  bl_make_univar_models_summary(bl_ecology_data,
#'                                                         "grasshopper",
#'                                                         names(bl_ecology_data)[4:20],
#'                                                         "(1|sub_area)",
#'                                                         model_family = "poisson")
#'
#' # add null_model for R² calculation
#' the_null_model <- lme4::glmer(grasshopper ~ (1|sub_area),
#'                                                         data = bl_ecology_data,
#'                                                         family = "poisson")
#'
#' univar_models_summary <-  bl_make_univar_models_summary(bl_ecology_data,
#'                                                         "grasshopper",
#'                                                         names(bl_ecology_data)[4:20],
#'                                                         "(1|sub_area)",
#'                                                         model_family = "poisson",
#'                                                         null_model = the_null_model)
#'
#' # custom order of the result
#' my_order <- c ("elevation", "isolation", "area")
#' bl_order_by_reference(univar_models_summary, "Variable", my_order)
#'
#'
#' @export


bl_make_univar_models_summary <-
    bl_make_univar_models_summary <-
    function(data_table,
             dependent_var,
             independent_vars,
             random_formula = NA,
             offset_var = NA,
             family = NA,
             model_family = NA,
             null_model = NULL,
             tc_off = F,
             do_scale = T,
             P_star = T,
             mean = T,
             error_type = "SE",
             R_2 = T,
             AUC = F,
             debugging = F,
             max_digits = 3
    ) {

        # check if family (old version) or model_family are provided
        if (is.na (family) && is.na (model_family)) {
            stop ("model_family must be specified")
        } else if (is.na (model_family)) {
            cat ("\033[32m'family' is deprecated. Please use 'model_family' instead.\033[0m\n")
            model_family <- as.character(substitute(family))
            family <- NULL
        } else {
            model_family <- as.character(substitute(model_family))
            family <- NULL
        }

        # check for offset
        if (!is.na (offset_var)) {
            my_offset <- log (data_table [[offset_var]])
        } else {
            my_offset <- rep (0, length (data_table[,1]%>%unlist))
        }

        # debugging
        if (debugging == T) {
            cat ("section A\n")
            cat (class (model_family))
            cat ("\n")
            cat ("model_family:")
            print (model_family)
        }

        ## make a 0model ("reference fit")
        # if GLMM (random formula given)
        if (!is.na (random_formula)) {
            # Check if the random_formula starts with "(" and ends with ")"
            if (grepl("^\\(.*\\)$", random_formula)) {
                random_formula <- random_formula
            } else {
                random_formula <- paste0 ("(", random_formula, ")")
            }
            ref_fit <- lme4::glmer(data = data_table,
                                   formula = as.formula (paste (dependent_var, "~", random_formula)),
                                   offset =  my_offset,
                                   family = model_family)

            # if GLM (no random formula given)
        } else {
            ref_fit <- glm(data = data_table,
                           formula = as.formula (paste (dependent_var, "~", 1)),
                           offset =  my_offset,
                           family = model_family)
        }

        # debugging
        if (debugging == T) {
            invisible(cat("\n"))
            cat("\033[31mthe reference fit (or '0-model'):\033[0m\n")
            print (ref_fit)
        }


        # make a results data.frame
        results <- data.frame("Variable" = independent_vars)

        # Calculate variable means and
        # Fit the univariable models
        for (my_var in independent_vars) {

            # debugging
            if (debugging == T) {
                invisible(cat("\n"))
                invisible(cat("\n"))
                invisible(cat("\n"))
                cat("\033[31mVariable in process:\033[0m\n")
                print (my_var)
            }

            ## i) calculate mean values
            if (mean == T) {

                # binomiale modelle (nicht "proportional binomial"): zwei spalten ("präsenz" und "nullprobe") -- außer tc_off ist T
                if (model_family == "binomial" &&
                    all(data_table [[dependent_var]] %in% c(0, 1, NA) | is.na(data_table [[dependent_var]])) &&
                    tc_off == F) {

                    # make two data.frames ("treatment" and "control")
                    df_treatment <- data_table %>% filter(!!sym(dependent_var) == 1)
                    df_control <- data_table %>% filter(!!sym(dependent_var) == 0)

                    # nur numerische Variablen können gemittelt werden...
                    if (is.numeric(data_table[[my_var]])) {

                        ## treatment data
                        # get mean and SE from GLM
                        mean_model_treatment <- glm (df_treatment [[my_var]] ~ 1) %>% summary ()

                        # debugging
                        if (debugging == T) {
                            invisible(cat("\n"))
                            invisible(cat("\n"))
                            invisible(cat("\n"))
                            cat("\033[31mModel for treatment mean calculation:\033[0m\n")
                            print (summary (mean_model_treatment))

                            print("\n")
                            cat("\033[31mresults data frame:\033[0m\n")
                            print(names(results))
                            print(str(results))
                            print(my_var)
                        }

                        # get mean and SE (or SD) values
                        treatment_coef <- mean_model_treatment$coefficients
                        treatment_mean <- treatment_coef [1]
                        if(error_type == "SE") {
                            treatment_SE <- treatment_coef [2]
                            # put mean and SE values in results frame
                            results[results$Variable==my_var, "Treatment Mean ± SE"] <- paste0(treatment_mean %>% bl_round_beautifully (last = F),
                                                                                       " ± ",
                                                                                       treatment_SE %>% bl_round_beautifully (last = T, max_dgts = max_digits))
                        }
                        if(error_type == "SD"){
                            treatment_SD <- sd(df_treatment [[my_var]])
                            # put mean and SD values in results frame
                            results[results$Variable==my_var, "Treatment Mean ± SD"] <- paste0(treatment_mean %>% bl_round_beautifully (last = F),
                                                                                       " ± ",
                                                                                       treatment_SD %>% bl_round_beautifully (last = T, max_dgts = max_digits))
                        }

                        ## control data
                        # get mean and SE from GLM
                        mean_model_control <- glm (df_control [[my_var]] ~ 1) %>% summary ()

                        # debugging
                        if (debugging == T) {
                            invisible(cat("\n"))
                            invisible(cat("\n"))
                            invisible(cat("\n"))
                            cat("\033[31mModel for control mean calculation:\033[0m\n")
                            print (summary (mean_model_control))
                        }

                        # get mean and SE values
                        control_coef <- mean_model_control$coefficients
                        control_mean <- control_coef [1]
                        control_SE <- control_coef [2]

                        # put mean and SE values in results frame
                        results [results$Variable == my_var, "Control Mean ± SE"] <- paste0(control_mean %>% bl_round_beautifully (last = F),
                                                                                            " ± ",
                                                                                            control_SE %>% bl_round_beautifully (last = T))
                    } else {
                        results [results$Variable == my_var, "Treatment Mean ± SE"] <- "."
                        results [results$Variable == my_var, "Control Mean ± SE"] <- "."
                    }

                    # alle anderen modelle: eine spalte (Gesamtdatensatz)
                } else {

                    # nur numerische Variablen können gemittelt werden...
                    if (is.numeric(data_table[[my_var]])){

                        # get mean and SE from GLM
                        mean_model <- glm (data_table [[my_var]] ~ 1) %>% summary () #%>% arm::display()

                        # debugging
                        if (debugging == T) {
                            invisible(cat("\n"))
                            invisible(cat("\n"))
                            invisible(cat("\n"))
                            cat("\033[31mModel for mean calculation:\033[0m\n")
                            print (summary (mean_model))
                        }

                        # get mean and SE values
                        my_coef <- mean_model$coefficients
                        my_mean <- my_coef [1]
                        my_SE <- my_coef [2]
                        my_SD <- sd(data_table[[my_var]])

                        # put mean and SE values in results frame
                        if(error_type == "SE"){
                            results [results$Variable == my_var, "Mean ± SE"] <- paste0(my_mean %>% bl_round_beautifully (last = F, max_dgts = max_digits),
                                                                                        " ± ",
                                                                                        my_SE %>% bl_round_beautifully (last = T, max_dgts = max_digits))
                        }
                        # or put mean and SD values in results frame
                        if(error_type == "SD"){
                            results [results$Variable == my_var, "Mean ± SD"] <- paste0(my_mean %>% bl_round_beautifully (last = F, max_dgts = max_digits),
                                                                                        " ± ",
                                                                                        my_SD %>% bl_round_beautifully (last = T, max_dgts = max_digits))
                        }

                        # nicht numerische Variablen: kein Mittelwert, sondern "."
                    } else {
                        results [results$Variable == my_var, "Mean ± SE"] <- "."
                    }
                }

            }

            ## ii) calculate Estimates with SE, P, R² and AUC

            # if random formula given --> GLMM
            if (!is.na (random_formula)) {

                # check if variable is numeric, if so --> scale variable
                if (is.numeric(data_table[[my_var]]) & do_scale == TRUE) {

                    # create formula argument
                    my_formula <- as.formula (paste (dependent_var, "~ scale(", my_var, ") +",
                                                     random_formula))

                    # if variable not numeric, don't scale
                } else {

                    # create formula argument
                    my_formula <- as.formula (paste (dependent_var, "~", my_var, "+",
                                                     random_formula))
                }

                # debugging
                if (debugging == T) {
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    cat("\033[31mFormula for univariable GLMM with variable in process as independent variable:\033[0m\n")
                    print (my_formula)
                }

                # Fit the univariable model
                uv_fit <- lme4::glmer(data = data_table,
                                      formula = my_formula,
                                      offset = my_offset,
                                      family = model_family)

                # debugging
                if (debugging == T) {
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    cat("\033[31mUnivarible GLMM with variable in process as independent variable:\033[0m\n")
                    print (uv_fit)
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    cat("\033[31mSummary of univarible GLMM with variable in process as independent variable:\033[0m\n")
                    print (summary (uv_fit))
                }

                # if no random formula given --> GLM
            } else {

                # check if variable is numeric, if so --> scale variable
                if (is.numeric(data_table[[my_var]]) & do_scale == TRUE) {
                    # create formula argument
                    my_formula <- as.formula (paste (dependent_var, "~ scale(", my_var, ")"))

                    # if variable not numeric, don't scale
                } else {
                    # create formula argument
                    my_formula <- as.formula (paste (dependent_var, "~", my_var))
                }

                # debugging
                if (debugging == T) {
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    cat("\033[31mFormula for univariable GLM with variable in process as independent variable:\033[0m\n")
                    print (my_formula)
                }

                # Fit the univariable model
                uv_fit <- glm(data = data_table,
                              formula = my_formula,
                              offset = my_offset,
                              family = model_family)
                if (debugging == T) {
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    cat("\033[31mUnivarible GLM with variable in process as independent variable:\033[0m\n")
                    print (uv_fit)
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    cat("\033[31mSummary of univarible GLM with variable in process as independent variable:\033[0m\n")
                    print (summary (uv_fit))
                }
            }



            # get p value
            # check if random formula is given
            # if so --> GLMM
            if (!is.na (random_formula)) {
                p_value <- anova(ref_fit, uv_fit)$`Pr(>Chisq)`[2] %>% round (digits = 3)

            } else {
                p_value <- anova(uv_fit, ref_fit, test = "Chisq")$`Pr(>Chi)`[2] %>% round (digits = 3)
            }

            if (debugging == T) {
                invisible(cat("\n"))
                invisible(cat("\n"))
                invisible(cat("\n"))
                cat("\033[31mP value of the last model:\033[0m\n")
                print (p_value)
            }

            # convert p value to *stars*
            if (P_star == T){
                if (p_value < 0.001) {
                    p_value <- '***'
                } else if (p_value < 0.01) {
                    p_value <- '**'
                } else if (p_value < 0.05) {
                    p_value <- '*'
                } else {
                    p_value <- 'n.s.'
                }
            }

            # get estimate
            est <- summary(uv_fit)$coefficients[2, 1] %>% bl_round_beautifully (last = F)
            if (debugging == T) {
                invisible(cat("\n"))
                invisible(cat("\n"))
                invisible(cat("\n"))
                cat("\033[31mEstimate of last model:\033[0m\n")
                print (est)
            }
            # get se
            std_err <- summary(uv_fit)$coefficients[2, 2] %>% bl_round_beautifully (last = T)
            if (debugging == T) {
                invisible(cat("\n"))
                invisible(cat("\n"))
                invisible(cat("\n"))
                cat("\033[31mStandard Error of last model:\033[0m\n")
                print (std_err)
            }

            # put Estimate, se and p in results frame
            results [results$Variable == my_var, "Estimate ± SE"] <- paste0 (est," ± " , std_err)
            results [results$Variable == my_var, "P"] <- p_value


            # calculate R² values
            if (R_2 == T) {
                if (!is.null(null_model)) {
                    my_R2 <- performance::r2(uv_fit, null_model = null_model)
                    cat(c("null_model", " used to calculate R² \n")) # beter put the actual name of the null_model object instead of "null_model"
                } else {
                    my_R2 <- performance::r2(uv_fit)
                }
                if (debugging == T) {
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    invisible(cat("\n"))
                    cat("\033[31mR² of last model: \033[0m\n")
                    print (my_R2)
                }
                # check whether conditional an marginal or simply one R² value are calculated
                if (length (my_R2) == 1) {
                    # put single R² value in results table
                    results [results$Variable == my_var, "R²"] <- my_R2 [[1]]%>% round (2)
                    message (paste0 (names (my_R2), " was calculated"))
                } else if (length (my_R2) == 2) {
                    # put conditional and marginal R² in results table - if conditional == NA, put marginal twice
                    if (is.na (my_R2[[1]])) {
                        results [results$Variable == my_var, "Rc/Rm"] <- paste0 (my_R2[[2]]%>% round (2), "/", my_R2[[2]]%>% round (2))
                    } else {
                        results [results$Variable == my_var, "Rc/Rm"] <- paste0 (my_R2[[1]]%>% round (2), "/", my_R2[[2]]%>% round (2))
                    }
                }
            }
            if (AUC == T) {
                if (model_family == "binomial") {
                    # Predict probabilities
                    prob <- predict(uv_fit , type = "response")
                    if (debugging == T) {
                        invisible(cat("\n"))
                        invisible(cat("\n"))
                        invisible(cat("\n"))
                        cat("\033[31mpredicted probabilities of last model: \033[0m\n")
                        print (prob)
                    }

                    # Calculate AUC
                    roc_obj <- pROC::roc(response = dependent_var, predictor = prob)
                    if (debugging == T) {
                        invisible(cat("\n"))
                        invisible(cat("\n"))
                        invisible(cat("\n"))
                        cat("\033[31mROC object from predicted probabilities of last model:\033[0m\n")
                        print (roc_obj)
                        invisible(cat("\n"))
                        invisible(cat("\n"))
                        invisible(cat("\n"))
                        cat("\033[31mAUC of from ROC object from predicted probabilities of last model:\033[0m\n")
                        print (pROC::auc(roc_obj))
                    }
                    results [results$Variable == my_var, "AUC"] <- pROC::auc(roc_obj)

                } else {
                    warning ("AUC only implemented for binomial models. No AUC calculated")
                }

            }
        }
        if (debugging == T){
            invisible(cat("\n"))
            invisible(cat("\n"))
            invisible(cat("\n"))
            cat ("\033[31mResult table:\033[0m\n")
        }
        return (results)
    }


