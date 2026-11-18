#' bl convert .svg to .emf
#'
#' takes a .svg file and converts it using Inkscape. This function will not work when Inkscape is not installed on ypur PC!
#' @param inkscape_path Path to the inkscape.exe
#' @param input_svg Path to the .svg file to be converted to .emf
#' @param output_emf Path where the .emf file shall be stored at.
#' @examples
#' # create plot
#' library (ggplot2)    ## Required for plotting.
#' my_plot <- ggplot (iris, aes (Sepal.Length, Sepal.Width, color = Species)) +
#'         geom_point () +
#'         bl_theme ()
#'
#' # save .svg file to drive
#' bl_save_svg (my_plot, "path/to/drive/plotName.svg")
#'
#' # load this svg and convert it to .emf
#' bl_convert_svg_to_emf(input_svg = "path/to/drive/plotName.svg", output_emf = "path/to/drive/plotName.emf")
#' @export


bl_convert_svg_to_emf <- function(input_svg = stop("Cannot convert if no .svg is provided."),
                                  output_emf = stop("Cannot convert if no path to store the .emf is provided",
                                  inkscape_path = 'C:/Program Files/Inkscape/bin/inkscape.exe')){
        if (!file.exists(inkscape_path)) {
            warning("Could not find Inkscape, will not convert to .emf")
        } else {
            command <- paste0('"', inkscape_path, '" "', input_file, '" --export-filename="', output_file, '"')
            print(command)  # Check the constructed command
            system(command)
        }

}

