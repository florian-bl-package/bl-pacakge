#' bl_check_distribution
#'
#' @description enables graphical inspection of the distribution of a variable by comparing it to possible distribution types
#'
#' @param variable the variable to be inspected
#' @param discrete Logical. Has the variable a discrete distribution?
#' @param distributions vector of distributions to be checked. Will be ignored for continuous variables (when "discrete == F")
#' @param size sets the standard error that should be displayed, the non-adjusted ("raw") or the adjusted ("adjusted"; the default) standard error.
#' @export
#' @examples
#'
#'
#' bl_check_distribution(variable = bl_ecology_data$grasshopper, discrete = T)
#' bl_check_distribution(variable = bl_ecology_data$butterflies, discrete = T, distributions = "norm")
#' bl_check_distribution(variable = bl_ecology_data$winter_temperature, discrete = F)



bl_check_distribution <- function (variable = NA,
                                   discrete = "NA",
                                   distributions = c ("nbinom", "pois", "norm"),
                                   size = 1.5
){
    # check whether discrete is set
    if (discrete == "NA"){
        stop ("'discrete' must be set either TRUE or FALSE")
    }

    # Extract the name of the variable
    variable_name <- deparse(substitute(variable))
    cat (paste0 ("Here, we inspect the variable \n", variable_name, "\n \n"))




    # if discrete == T, use fitdistrplus cullen and frey graph and fit distribution plot
    if (discrete == T){

        # make cullen and frey graph
        windows(4.5, 4.5, xpos = 0.5, ypos = 5)
        cullen_frey_graph <- fitdistrplus::descdist(variable, discrete = T, boot = 1000)
        cullen_frey_title <- mtext (variable_name, side = 3, line = 3, cex = 1.2)


        # make the fitdist plots
        # get number of plots
        num_plots <- length (distributions)

        # Calculate the number of rows and columns for the grid layout
        num_rows <- ceiling(sqrt(num_plots))
        num_cols <- ceiling(num_plots / num_rows)

        # Set the initial position for the first plot
        x_start <- 0.5
        y_start <- 5

        # loop over plots and create them
        for (i in seq_along(distributions)) {
            # Calculate the position for the current plot
            x <- x_start + ((i) %% num_cols) * 785
            y <- y_start + floor((i) / num_cols) * 490

            # Open a new window and plot
            #windows (10, 5, xpos = 0.5+100*i, ypos = 0.5+100*i)
            windows (8, 4.5, xpos = x, ypos = y)
            fitdist_plot <- fitdistrplus::fitdist(variable, distributions[i])
            plot (fitdist_plot)
            mtext (paste0 (variable_name, ": ", distributions[i], " distribution"), side = 3, line = 3, cex = 1.2)
            cat ("Check all new windows that have been created!")
        }
    }

    # if discrete == F, use fitur addin
    if (!discrete){
        windows(width = 6, height = 6)
        cullen_frey_graph <- fitdistrplus::descdist(variable, discrete = F, boot = 1000)
        mtext (variable_name, side = 3, line = 3, cex = 1.2)
        cullen_frey_graph
        # Create a temporary environment
        custom_env <- new.env()

        # Populate the custom environment with variables of your choice
        custom_env[[variable_name]] <- variable


        # Run the bl_fit_dist_addin() function
        bl_fit_dist_addin(custom_env, size = size)


       # rm (custom_env)
    }

}




# custom addin function ####
bl_fit_dist_addin <- function (environment = .GlobalEnv, size = 1)
{
    numericObjects <- ls(envir = environment)[sapply(ls(envir = environment),
        function(xChar) {
            is.numeric(get(xChar, envir = environment))
        })]
    discreteDists <- c("geom", "nbinom", "pois", "dunif")
    continuousDists <- c(Exponential = "exp", Cauchy = "cauchy",
        Gamma = "gamma", LogNormal = "lnorm", Normal = "norm",
        Uniform = "unif", Weibull = "weibull", LogLogistic = "llogis",
        Logistic = "logis", InverseWeibull = "invweibull", InverseGamma = "invgamma")
    ui <- miniUI::miniPage(miniUI::miniContentPanel(shiny::fillCol(flex = c(NA,
        NA, 1), miniUI::gadgetTitleBar("Fit Distributions", right = NULL),
        shiny::fillRow(flex = c(NA, NA, NA, NA), shiny::selectizeInput("distributions",
            "Distributions", choices = continuousDists, selected = "norm",
            multiple = TRUE), shiny::selectInput("data", label = "Data",
            choices = numericObjects), shiny::numericInput("nbins",
            "No. of Bins", value = 30, min = 5, max = 100, step = 1)),
        shiny::fillRow(shiny::fillCol(flex = c(1, 1), DT::DTOutput("gofTable",
            height = "100%"), shiny::plotOutput("ppPlot", height = "100%")),
            shiny::fillCol(flex = c(1, 1), shiny::plotOutput("densityPlot",
                height = "100%"), shiny::plotOutput("qqPlot",
                height = "100%"))))))
    server <- function(input, output, session) {
        x <- shiny::reactive({
            get(input$data, envir = environment)
        })
        fits <- shiny::reactive({
            x <- x()
            dists <- input$distributions
            fits <- lapply(dists, fitur::fit_univariate, x = x)
        })
        output$gofTable <- DT::renderDT({
            gof <- fitur::gof_tests(fits(), x())
            DT::formatRound(DT::datatable(stats::setNames(gof,
                gsub("_", " ", names(gof))), options = list(searching = FALSE,
                lengthMenu = -1, lengthChange = FALSE, paging = FALSE)),
                columns = 2:7)
        })
        output$densityPlot <- shiny::renderPlot({
            fitur::plot_density(x(), fits(), input$nbins) + ggplot2::labs(title = "Density Plot") +
                ggplot2::theme_bw()
        })
        output$ppPlot <- shiny::renderPlot({
            fitur::plot_pp(x(), fits()) + ggplot2::labs(title = "PP-Plot") +
                ggplot2::theme_bw()
        })
        output$qqPlot <- shiny::renderPlot({
            fitur::plot_qq(x(), fits()) + ggplot2::labs(title = "QQ-Plot") +
                ggplot2::theme_bw()
        })
    }
    shiny::runGadget(ui, server, viewer = shiny::dialogViewer("bl continuous distribution comparison", width = 1600*size, height = 900*size))
}
