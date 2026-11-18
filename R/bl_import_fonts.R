#' bl font import
#'
#' @description Installs bl-relevant fonts for R. Note that this needs only to be run once: once installed, the fonts are always available as long as the package 'bl' or 'extrafont' is loaded (start your next R session with # > library (bl) or # > library (extrafont))
#' @export

bl_import_fonts <- function() {
    font_dirs <- file.path(.libPaths(), "bl", "fonts")
    existing_dirs <- font_dirs[dir.exists(font_dirs)]

    extrafont::font_import(paths = existing_dirs, prompt = FALSE)
    extrafont::loadfonts(device = "all")

    # Manuelles PostScript-Mapping für ChantillyLH
    ft <- extrafont::fonttable()
    get_afm <- function(name) {
        file.path(system.file("afm", package = "extrafontdb"),
                  ft$afmfile[ft$afmfile == paste0(name, ".afm.gz")])
    }

    grDevices::postscriptFonts(
        ChantillyLH = grDevices::Type1Font(
            family  = "ChantillyLH",
            metrics = c(
                get_afm("CHANLH__"),
                get_afm("CHANLH__"),
                get_afm("CHANLHI_"),
                get_afm("CHANLHI_")
            )
        )
    )

    cat("\033[38;5;171m Fonts installed successfully. \033[0m")


    cat("\033[38;5;171m NOTE: run this function once to install the relevant fonts for R. The fonts will be available in the future whenever package 'extrafont' is loaded (which automatically happens when you load bl) \033[0m")
}
