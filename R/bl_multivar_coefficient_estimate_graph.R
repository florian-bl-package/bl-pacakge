#' bl_multivar_coefficient_estimate_graph
#'
#' @description takes one or a list of mutlivariable models and returns a Coefficient estimate point-whiskes plot, both in R and as a table suited for plotting in Sigmaplot.
#'
#' @param model a multivariable model or a list of multivariable models
#' @param sub_type For average models: "full" or "subset" for the full or subset model average.
#' @param varname_replacement a vector or a data.frame with the columns "reference" and "replacement". A vector probably only makes sense when desired_variable_order is not set. If a vector, all variables must see a replacement value in the vector and will be replaced accordingly. If a data.frame, all values in the reference column will be replaced by the respective values in the replacement column. Not specified values will be kept as is.
#' @param desired_variable_order a vector of characters. Sets the order of the variables in the plot. Note that if variable names are replaced by setting 'varname_replacement', the 'desired_variable_order' values must match the 'replacement' values.
#' @param y_group_offset a number or a vector of numbers of same length as the number of models. if multiple models shall be displayed in one graph, this changes the relative y-offset between these models.
#' @param sections a vector of characters (the section names)
#' @param section_positions a vector of integers of same length as 'sections'. Sets the position of the section headings (think of the corresponding "rows" in the resulting plot)
#' @param y_section_offset a number or a vector of numbers of same length as the number of sections. Determines the relative y-offset of the section headings.
#' @param plot_in_R logical. should a R plot be created? Useful at least as a preview
#' @param point_size set the size of the symbols in the plot in R. Defaults to 2.2.
#' @param point_fill_colors only for plotting in R. a single color or a vector of colors for the different models.
#' @param point_shapes only for plotting in R. a single number or a vector of numbers defining the point shapes for the different models. Useful numbers are in particular 21 ... 25.
#' @param plot_title only for plotting in R. A character string defining the title of the plot.
#' @param export_for_sigmaplot logical. should a .xlsx table be exported for use with Sigmaplot? Note that if TRUE, 'path_for_sigmaplot_table' must be set.
#' @param path_for_sigmaplot_table a character string. A path to store a table for further use with Sigmaplot.
#' @export
#' @examples
#' # make a list of three different multivariable models that share some variables - usually all models should be related to the same topic/study
#' model_list <- list ("grasshoppers" = bl_grasshopper_avg_model, "butterflies" = bl_butterfly_avg_model, "leafhoppers" = bl_leafhopper_best_model)
#'
#' # make a Coefficient-estimate plot, this time not exporting to Sigmaplot (you might consider this rather a preview)
#' bl_multivar_coefficient_estimate_graph(model_list,
#'                                        export_for_sigmaplot = F,
#'                                        sections = c ("SectionA", "SectionB"),
#'                                        section_positions = c (1,4),
#'                                        plot_title = "grasshopper coefficients")
#'






bl_multivar_coefficient_estimate_graph <- function (model,
                                                    sub_type = "full",
                                                    varname_replacement = NA,
                                                    desired_variable_order = NA,
                                                    y_group_offset = 1,
                                                    sections = NA,
                                                    section_positions = NA, # placement of the section headings, read from the top (1 is the first row)
                                                    y_section_offset = 1,
                                                    section_label_extra_offset = 10, # Abstand (pt), um den die Section-Labels weiter links beginnen als das längste Label
                                                    plot_in_R = T,
                                                    point_size = 2.2, # only needed for R plotting
                                                    point_fill_colors = "default", # only needed for R plotting
                                                    point_shapes = "default", # only needed for R plotting. more than 5 models in one plot not supported
                                                    error_height = 0.1,
                                                    aspect_ratio = "auto",
                                                    y_white_margin = 0.5,
                                                    y_variable_spacing = "auto",  # "auto" oder eine Zahl (1 = bisheriger Abstand)
                                                    y_min_gap = 0.8,              # nur bei "auto": minimale freie Lücke zwischen Variablen-Gruppen
                                                    plot_title = "",
                                                    legend_titel = "",
                                                    export_for_sigmaplot = T,
                                                    path_for_sigmaplot_table = NA,# only needed if exporting to sigmaplot is desired
                                                    debug = F # only for function debugging

){
    # check arguments
    if (0 == 0) {
        if (!is_model(model) && !is_list_of_models(model)) {
            stop ("'model' must be a model or a list of models")
        }

        if (is.na(path_for_sigmaplot_table) && isTRUE(export_for_sigmaplot)){
            stop ("Cannot export table for sigmaplot if path for sigmaplot is not specified.")
        }

        # if only one model --> put in a list to handle it in the same way as a list of models
        if (!is_list_of_models(model)){
            model <- list("_" = model)
        }

        # if list of models without names -- add names to list
        if (is.null (names(model))){
            names(model) <- letters [1:length(model)]
        }
        for (i in seq_along(model)) {
            if (names(model[i]) == "") {
                names(model)[i] <- LETTERS[i]
            }
        }

        #
        if (length(sections) != length(section_positions)) {
            stop ("sections and section positions must be vectors of same length!")
        }

        #
        if (!is.na(path_for_sigmaplot_table)&&!endsWith(path_for_sigmaplot_table, ".xlsx")) {
            path_for_sigmaplot_table <- paste0(path_for_sigmaplot_table, ".xlsx")
        }

        #
        if (!identical(y_variable_spacing, "auto") &&
            !(is.numeric(y_variable_spacing) && length(y_variable_spacing) == 1 && y_variable_spacing > 0)) {
            stop("'y_variable_spacing' must be \"auto\" or a single positive number")
        }
    }



    # preparations
    if (0 == 0) {
        # # varible names, numbers and order # #
        # get variable names from all models
        var_names_list <- lapply(model, get_var_names)
        # Combine the results into a single vector and delete duplicates
        all_var_names <- c (unlist(var_names_list)) %>% unique ()

        # get number of variables
        no_vars <- length(all_var_names)

        # create a tibble to replace the variable names in the average model with the variable names of your choice
        if (!identical(varname_replacement, NA)) {
            if (is.data.frame(varname_replacement)){
                varname_replacement_table <- generate_df(varname_replacement, all_var_names)
            } else if (length(varname_replacement)!=no_vars) {
                stop (paste ("varname_replacement does not match the length of the variable names in the input models. These are:", all_var_names))
            } else {
                varname_replacement_table <- tibble (reference = all_var_names)
                # put the desired variable names here
                varname_replacement_table$replacement <- varname_replacement
            }
        } else {
            varname_replacement_table <- tibble (reference = all_var_names, replacement = all_var_names)
        }

        # set order of variables if not specified in the arguments
        if (identical(desired_variable_order, NA)){
            desired_variable_order <- varname_replacement_table$replacement
        }
        if (length (desired_variable_order) != no_vars){

        }



        # # get model coefficients of each model and create a respecitve table for each model # #
        # make an empty list
        multivar_models_plotting_list <- list()
        # fill list with model coefficients
        for (i in seq_along(model)) {
            multivar_models_plotting_list[[names (model)[i]]] <- get_model_coefficients(the_model = model[[i]], subset_type = sub_type)
            colnames(multivar_models_plotting_list[[names (model)[i]]]) <- paste0 (names (model)[i], c ("_x_Estimate", "_x_2.5", "_x_97.5" ))
        }

        # set y_tick labels
        for (i in seq_along(multivar_models_plotting_list)) {
            multivar_models_plotting_list[[i]]$y_tick_label <- bl_replace_by_reference(row.names(multivar_models_plotting_list[[i]]), varname_replacement_table)
        }

        # order by reference


        ## define y positions using all variables and all sections
        # step 1: y offset for the different models (if provided)
        if (length(y_group_offset) == 1) {
            y_group_offset <- generate_offset_vector(length(model)) * y_group_offset
        }
        if (length(model) != length(y_group_offset)) {
            stop("y_group_offset must be a vector either of length one or of length == total number of models")
        }

        # step 2: spacing between variables (auto or manual)
        group_extent <- max(y_group_offset) - min(y_group_offset)
        if (identical(y_variable_spacing, "auto")) {
            y_spacing <- max(1, group_extent + y_min_gap)
        } else {
            y_spacing <- y_variable_spacing
        }
        if (isTRUE(debug)) {
            cat("group_extent:", group_extent, "| y_spacing:", y_spacing, "\n")
        }

        # step 3: integer positions for each variable and each section, scaled by spacing
        no_of_labels <- ifelse(identical(sections, NA), no_vars, no_vars + length(sections))
        if (!identical(section_positions, NA)) {
            y_label_positions <- rev(seq_len(no_vars + length(sections))) * y_spacing
        } else {
            y_label_positions <- rev(seq_len(no_vars)) * y_spacing
        }

        # step 4: consider sections (shift scaled along with the spacing)
        if (!identical(section_positions, NA)) {
            y_label_positions[section_positions] <-
                y_label_positions[section_positions] - y_section_offset * 0.4 * y_spacing
        }

        # step 5: y-Achsengrenzen (absolut, unabhängig vom Variablenabstand)
        # Rand oben/unten: Standardrand + (nur wenn dort Symbole sitzen) der Offset
        y_base_pad <- y_white_margin
        n_rows <- length(y_label_positions)
        top_is_section    <- !identical(section_positions, NA) && 1 %in% section_positions
        bottom_is_section <- !identical(section_positions, NA) && n_rows %in% section_positions

        y_pad_top    <- y_base_pad + if (top_is_section)    0 else max(y_group_offset)
        y_pad_bottom <- y_base_pad + if (bottom_is_section) 0 else abs(min(y_group_offset))

        y_limit_top    <- max(y_label_positions) + y_pad_top
        y_limit_bottom <- min(y_label_positions) - y_pad_bottom



        ## add empty rows for variables not included in every model
        # combine dataframes
        multivar_models_plotting_table <- Reduce(function(x, y) merge(x, y, by = intersect(names(x), names(y)), all = TRUE), multivar_models_plotting_list)
        # Order by desired order and split the result into original data frames + add y_tick_label_position
        multivar_models_plotting_list <- lapply(multivar_models_plotting_list, function(df) {
            cols <- intersect(names(df), names(multivar_models_plotting_table))
            df_out <- multivar_models_plotting_table[, cols, drop = FALSE]
            df_out <- bl_order_by_reference(df_out, "y_tick_label", desired_variable_order)
            # add rows for the sections
            if (!identical(section_positions, NA)){
                df_out <- add_section_rows(df_out, section_positions, sections)
            }
            df_out$y_tick_label_position <- y_label_positions
            y_pad <- (range(y_label_positions)[2] - range(y_label_positions)[1]) * 0.05 + max(abs(y_group_offset))

            #df_out$y_range <- c (range(y_label_positions)[2] + y_pad, range(y_label_positions)[1] - y_pad, rep(NA, length(df_out$y_tick_label_position)-2))
            df_out$y_range <- c(y_limit_top, y_limit_bottom,
                                rep(NA, length(df_out$y_tick_label_position) - 2))
            df_out$x_helper1 <- c (0, 0, rep(NA, length(df_out$y_tick_label_position)-2))
            df_out$x_helper2 <- df_out$x_helper1
            return(df_out)
        })



        # add y positions for the points and whiskers to the oputput table
        for (i in seq_along(multivar_models_plotting_list)) {
            multivar_models_plotting_list[[i]][[paste0(names(multivar_models_plotting_list[i]),"_y_point_position")]] <- multivar_models_plotting_list[[i]]$y_tick_label_position + y_group_offset[[i]]
        }







    }

    # plot in R --- preprations####
    if (plot_in_R == T) {
        # define point fill colors and shapes
        # point fill colors
        if (point_fill_colors[[1]] == "default") {
            if(debug == T) {
                cat("point_fill_colors == 'default' --> eigentlich sollten default colors gesetzt werden. \n")
            }
            point_fill_colors <- get_default_colors(length(model))
        } else if (length(point_fill_colors) == 1) {
            if(debug == T) {
                cat("length(point_fill_colors == 1) --> es soll der eine color wert wiederholt werden. \n")
            }
            point_fill_colors <- rep(point_fill_colors, length(model))
        } else {
            if(debug == T) {
                cat("point_fill_colors sind ein vektor; die farben sollen entsprechend ausgegeben werden. \n")
            }
        }



        # point sizes
        if (length(point_size) == 1) {
            point_size <- rep(point_size, length(model))
        } else if (length(point_size) != length(model)) {
            stop("'point_size' must be either of length 1 or of length == number of models")
        }
        names(point_size) <- names(model)   # eindeutige Zuordnung zu den Faktorstufen von Data_Frame




        # point shapes
        if (debug == T) {
            cat("Ergebnis der Prüfung 'length(point_shapes) > 1': \n")
            cat(length(point_shapes) > 1)
        }
        if (length(point_shapes) > 1) {

        } else {
            if (point_shapes[[1]] == "default") {
                point_shapes <- (rep(1:5, length.out = length(model)))+20
            } else if (length(point_shapes)==1) {
                point_shapes <- rep(point_shapes, length(model))
            }
        }


        # aspect ratio
        if (aspect_ratio == "auto"){
            aspect_ratio <- 1.5 * y_spacing * (1 + no_of_labels/10)
        } else {
            aspect_ratio <- aspect_ratio
        }

        # delete columns only needed for Sigmaplot
        multivar_models_plotting_list_R <- lapply(multivar_models_plotting_list, function(df) {
            df <- df %>% dplyr::select (-c ("y_range", "x_helper1", "x_helper2"))
            return(df)
        })

        # Combine all data frames into one, adding a column to indicate the original data frame
        # Desired column names
        new_column_names <- c("x_Estimate", "x_2.5", "x_97.5", "y_tick_label", "y_tick_label_position", "y_point_position")  # Replace with your desired column names
        # Update column names for each data frame in the list
        multivar_models_plotting_list_R <- lapply(multivar_models_plotting_list_R, function(df) {
            names(df) <- new_column_names
            return(df)
        })
        # combine data.frames
        multivar_models_plotting_table_R <- bind_rows(multivar_models_plotting_list_R, .id = "Data_Frame")
        multivar_models_plotting_table_R$Data_Frame <- factor(multivar_models_plotting_table_R$Data_Frame, levels = names (model))

        # # #
        # # #
        # # #
        # # #
        # create plot in R ####
        # --- Sections: Labels separat zeichnen (kursiv, linksbündig, ohne Tick) ---
        has_sections <- !identical(sections, NA) && !identical(section_positions, NA)

        if (has_sections) {
            is_section   <- multivar_models_plotting_table_R$y_tick_label %in% sections
            axis_data    <- multivar_models_plotting_table_R[!is_section, ]
            section_data <- unique(multivar_models_plotting_table_R[is_section,
                                                                    c("y_tick_label", "y_tick_label_position")])

            # Breite des längsten "normalen" Labels in pt (gleiche Schrift/Größe wie axis.text.y)
            label_width_pt <- max(sapply(unique(as.character(axis_data$y_tick_label)), function(l) {
                grid::convertWidth(
                    grid::grobWidth(grid::textGrob(l, gp = grid::gpar(fontfamily = "Chantilly", fontsize = 12))),
                    "pt", valueOnly = TRUE)
            }))

            # Startposition der Section-Labels (relativ zum linken Panelrand, in pt):
            # längstes Label + Tick-Länge/Abstand (ca. 5 pt) + zusätzlicher Abstand
            section_x_pt <- -(label_width_pt + 5 + section_label_extra_offset)

            section_layers <- lapply(seq_len(nrow(section_data)), function(i) {
                annotation_custom(
                    grob = grid::textGrob(
                        label = section_data$y_tick_label[i],
                        x = grid::unit(section_x_pt, "pt"),
                        y = grid::unit(0, "npc"),
                        hjust = 0,                       # linksbündig
                        gp = grid::gpar(fontfamily = "Chantilly",
                                        fontsize = 12,
                                        fontface = "italic")),
                    xmin = -Inf, xmax = -Inf,
                    ymin = section_data$y_tick_label_position[i],
                    ymax = section_data$y_tick_label_position[i])
            })
            # Platz links schaffen, damit nichts abgeschnitten wird
            section_theme <- theme(plot.margin = margin(5.5, 5.5, 5.5,
                                                        section_label_extra_offset + 5.5, "pt"))
        } else {
            axis_data      <- multivar_models_plotting_table_R
            section_layers <- NULL
            section_theme  <- NULL
        }

        multivar_model_grouped_plot <-
            ggplot (data = multivar_models_plotting_table_R,
                    mapping = aes(x = x_Estimate, y = y_point_position , xmin = x_2.5, xmax = x_97.5, fill = Data_Frame, shape = Data_Frame)
            ) +

            # 0-Linie
            geom_vline (xintercept = 0, linetype = 2) +

            # errorbars
            geom_errorbarh (height = error_height) +

            # points
            geom_point (aes(size = Data_Frame)) +
            scale_size_manual(values = point_size) +
            scale_shape_manual(values = point_shapes) +
            scale_fill_manual (values = point_fill_colors) +

            # axis labels
            labs (x = "Coefficient estimate", y = "") +

            # y-Achse: Breaks/Labels nur für normale Variablen (-> keine Ticks bei Sections)
            # scale_y_continuous(breaks = axis_data$y_tick_label_position,
            #                    labels = axis_data$y_tick_label) +
            scale_y_continuous(breaks = axis_data$y_tick_label_position,
                               labels = axis_data$y_tick_label,
                               limits = c(y_limit_bottom, y_limit_top),
                               expand = expansion(0)) +

            # change legend title
            # legend title (kursiv)
            labs(fill = legend_titel, shape = legend_titel, size = legend_titel) +
            # labs (fill = "") +
            # labs (shape = "") +

            # theme
            bl_theme(axis.text.y = element_text(
                family = "Chantilly",
                size = 12),
                axis.text.x = element_text(size = 12),
                axis.title.x.bottom = element_text(size = 13),
                plot.title = element_text(family = "Chantilly", size = 14),
                legend.title = element_text(family = "Chantilly", face = "italic", size = 12),
                aspect.ratio = aspect_ratio
            ) +

            # Section-Labels (außerhalb des Panels zeichnen erlauben)
            section_layers +
            coord_cartesian(clip = "off") +
            section_theme +

            # add a title
            ggtitle(plot_title)
        # multivar_model_grouped_plot <-
        #     ggplot (data = multivar_models_plotting_table_R,
        #             mapping = aes(x = x_Estimate, y = y_point_position , xmin = x_2.5, xmax = x_97.5, fill = Data_Frame, shape = Data_Frame)
        #     ) +
        #
        #     # 0-Linie
        #     geom_vline (xintercept = 0, linetype = 2) +
        #
        #     # errorbars
        #     # errorbars
        #     geom_errorbarh (height = 0.1) +
        #
        #     # points
        #     geom_point (size = point_size) +
        #     scale_shape_manual(values = point_shapes) +
        #     scale_fill_manual (values = point_fill_colors) +
        #
        #
        #     # axis labels
        #     labs (x = "Coefficient estimate", y = "") +
        #
        #     # change y axis to add correct tick labels
        #     scale_y_continuous(breaks = multivar_models_plotting_table_R$y_tick_label_position,
        #                        labels = multivar_models_plotting_table_R$y_tick_label) +
        #
        #     # change legend title
        #     labs (fill = "") +
        #     labs (shape = "") +
        #
        #
        #     # theme
        #     bl_theme(axis.text.y = element_text(
        #         family = "Chantilly",
        #         #hjust = multivar_model_grouped_plotting_R$justification,
        #         size = 12), # this gives a warning - ignore it (until it might stop working one day?)
        #         axis.text.x = element_text(size = 12),
        #         axis.title.x.bottom = element_text(size = 13),
        #         plot.title = element_text(family = "Chantilly", size = 14),
        #         aspect.ratio = 1.5
        #     ) +
        #
        #     # add a title
        #     ggtitle(plot_title)
    }

    # export for sigmaplot ####
    multivar_models_sigmaplot <- Reduce(function(x, y) merge(x, y, by = intersect(names(x), names(y)), all = TRUE), multivar_models_plotting_list)
    multivar_models_sigmaplot <- multivar_models_sigmaplot %>% dplyr::arrange(-y_tick_label_position)
    multivar_models_sigmaplot[c("y_range", "x_helper1", "x_helper2")] <- multivar_models_sigmaplot[c("y_range", "x_helper1", "x_helper2")] %>% dplyr::arrange(-y_range)
    # re-arrange a bit more for Sigmaplot
    # store variables
    if (!identical(sections, NA)) {
        helper_values <- multivar_models_sigmaplot[c(1,2), c("y_range", "x_helper1", "x_helper2")]
        y_section_positions <- multivar_models_sigmaplot[section_positions, "y_tick_label_position"]
        y_section_labels <- multivar_models_sigmaplot[section_positions, "y_tick_label"]
        multivar_models_sigmaplot <- multivar_models_sigmaplot[-section_positions,]

        y_section_positions <- c (y_section_positions, rep(NA, length(multivar_models_sigmaplot$y_tick_label) - length(y_section_positions)))
        y_section_labels <- c (y_section_labels, rep(NA, length(multivar_models_sigmaplot$y_tick_label) - length(y_section_labels)))

        multivar_models_sigmaplot <- cbind(multivar_models_sigmaplot[c(1:5)], y_section_labels, y_section_positions, multivar_models_sigmaplot[c(6:length(multivar_models_sigmaplot[1,]))])
        multivar_models_sigmaplot[c(1:2), c("y_range", "x_helper1", "x_helper2")] <- helper_values
    } else {
        y_section_positions <- rep(NA, length(multivar_models_sigmaplot$y_tick_label))
        y_section_labels <- y_section_positions
        multivar_models_sigmaplot <- cbind(multivar_models_sigmaplot[c(1:5)], y_section_labels, y_section_positions, multivar_models_sigmaplot[c(6:length(multivar_models_sigmaplot[1,]))])
    }



    sigmaplot_message <- "Table for Sigmaplot created but not exportet. If you want to export it for further use in Sigmaplot, set 'export_for_sigmaplot' TRUE. Here is how it looks:  \n"
    if (export_for_sigmaplot == T) {
        bl_save_xlsx(multivar_models_sigmaplot, file = path_for_sigmaplot_table)
        sigmaplot_message <- paste ("Table for Sigmaplot created and exported to:  \n", path_for_sigmaplot_table, "\n \n", "and here is how it looks: \n")
    }





    # # # function outputs # # #
    if (!identical(varname_replacement, NA)){
        cat(crayon::blue("Overview of input and output variable names \n"))
        print (varname_replacement_table)
    }


    cat ("\n \n ")
    cat (crayon::blue(sigmaplot_message))
    print (multivar_models_sigmaplot)
    # return the R plot
    if (plot_in_R == T) {
        cat (crayon::blue("\n \n ggplot has been created. Use 'bl_draw_plot()' to draw a customized version of it, e.g. by setting 'plot_width = 200'."))
        return(multivar_model_grouped_plot)
    }
}











# secondary functions ####
# Check if object is a model
is_model <- function(obj) {
    inherits(obj, "lm") || inherits(obj, "glm") || inherits(obj, "nls") ||
        inherits(obj, "randomForest") || inherits(obj, "glmerMod") ||
        inherits(obj, "lme") || inherits(obj, "gls") || inherits(obj, "averaging")
}

# Check if object is a list of models
is_list_of_models <- function(obj) {
    is.list(obj) && all(sapply(obj, is_model))
}


# make a function to get all variable names of a model
get_var_names <- function(model) {
    if (class(model)[1] == "averaging") {
        independent_vars <- model$coefficients%>%colnames()
    } else if (inherits(model, c("lm", "glm", "glmm", "lmm", "glmerMod"))) {
        independent_vars <- summary(model)$coefficients %>% row.names()#[-1]  # Exclude intercept
    } else {
        stop("Unsupported model type. Please provide a supported model.")
    }
    independent_vars <- subset(independent_vars, independent_vars != "(Intercept)")
    return(independent_vars)
}


# generate an offset vector for multiple model plotting
generate_offset_vector <- function(no_models) {
    if (no_models == 1) {
        offset <- c(0)
    } else {
        max_offset <- 0.1
        offset <- seq(max_offset, -max_offset, length.out = no_models)
    }
    return(offset)
}

# get model coefficients function
get_model_coefficients <- function(the_model, subset_type) {
    if (class(the_model)[1] == "averaging") {
        if (subset_type == "full") {
            the_model_estimate <- summary(the_model)$coefmat.full[,1] %>% as.data.frame()
            the_model_confint <- confint (the_model, full = T)
        } else if (subset_type == "subset") {
            the_model_estimate <- summary (the_model)$coefmat.subset[,1] %>% as.data.frame()
            the_model_confint <- confint (the_model, full = F)
        } else {
            stop ("sub_type must be either 'full' or 'subset'")
        }
        independent_vars <- the_model$coefficients%>%colnames()
    } else if (inherits(the_model, c("lm", "glm", "glmm", "lmm", "glmerMod"))) {
        the_model_estimate <- c(summary(the_model)$coefficients[,"Estimate"]) %>% as.data.frame()
        # average the_models use the "wald" method. I implemented it for "normal" the_models, too - this omits the calculation of CIs for the random effects. Could be checked is this is statistically valid (but practically seems to make no difference for plotting purposes)
        the_model_confint <- confint (the_model, method = "Wald", parm = c("beta_", "fixef"))
    } else {
        stop("Unsupported the_model type. Please provide a supported the_model.")
    }
    # remove intercept row
    the_model_estimate <- the_model_estimate[row.names(the_model_estimate) != "(Intercept)",]
    the_model_confint <- the_model_confint[row.names(the_model_confint) != "(Intercept)",]
    # remove .sig rows (in glmer the_models, .sig01 [...] .sigN rows are computed as CIs for the random effects)
    the_model_confint <- the_model_confint[!grepl("^\\.sig", row.names(the_model_confint)), , drop = FALSE]

    the_model_coeffs <- data.frame ("Estimate" = the_model_estimate)
    the_model_coeffs <- cbind (the_model_coeffs, the_model_confint)
    return(the_model_coeffs)
}


# add empty rows at defined positions
add_section_rows <- function(df, additional_positions, names_vector) {
    for (i in seq_along(additional_positions)) {
        pos <- additional_positions[i]
        name <- names_vector[i]

        df <- df %>%
            dplyr::add_row(.before = pos) %>%
            dplyr::mutate(y_tick_label = replace(y_tick_label, pos, name))
    }
    return(df)
}

# get default colors
get_default_colors <- function (no_models){
    if (no_models==1){
        point_colors <- bl_blue(3)
    } else if (no_models==2){
        point_colors <- c(bl_blue(3), bl_green(3))
    } else if (no_models==3){
        point_colors <- c (bl_blue(1), bl_blue(3), bl_blue(5))
    } else if (no_models==4){
        point_colors <- c (bl_blue(1), bl_green(1), bl_blue(5), bl_green(5))
    } else if (no_models==5){
        point_colors <- c (bl_blue(1), bl_blue(2), bl_blue(3), bl_blue(4), bl_blue(5))
    } else if (no_models==6){
        point_colors <- c (bl_blue(1), bl_green(1), bl_blue(3), bl_green(3), bl_blue(5), bl_green(5))
    } else if (no_models==7){
        point_colors <- c (bl_blue(1), bl_green(2), bl_blue(2), bl_blue(3), bl_blue(4), bl_green(4), bl_blue(5))
    } else if (no_models==8){
        point_colors <- c (bl_blue(1), bl_green(1), bl_blue(2), bl_green(2), bl_blue(4), bl_green(4), bl_blue(5), bl_green(5))
    } else if (no_models==9){
        point_colors <- c (bl_blue(1), bl_green(1), bl_blue(2), bl_green(2), bl_blue(3), bl_green(4), bl_blue(4), bl_green(5), bl_blue(5))
    } else if (no_models==10){
        point_colors <- c (bl_blue(1), bl_green(1), bl_blue(2), bl_green(2), bl_blue(3), bl_green(3), bl_blue(4), bl_green(4), bl_blue(5), bl_green(5))
    } else {
        cat(crayon::silver("defalut colors for more than 10 models not supported, all models set to bl_blue(3)"))
        point_colors <- rep (bl_blue(3), no_models)
    }
    return(point_colors)
}



# vervollständige variable names replacement data.frame
generate_df <- function(df, char_vec) {
    # Identify matching values
    matched_values <- df[df$reference %in% char_vec, ]

    # Find values in char_vec not present in reference column
    missing_values <- setdiff(char_vec, matched_values$reference)

    # Create new rows for missing values
    new_rows <- data.frame(
        reference = missing_values,
        replacement = missing_values
    )

    # Combine matched rows and new rows
    result_df <- rbind(matched_values, new_rows)

    # Sort the result according to char_vec
    result_df <- result_df[match(char_vec, result_df$reference), ]

    return(result_df)
}
