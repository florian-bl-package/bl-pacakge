#' bl_save_xlsx
#'
#' @description export basic styled tables as .xlsx files. very nice.
#' @param table_to_write A data.frame or a (named) list of objects that can be handled by openxlsx::writeData() or openxlsx::writeDataTable() to write to file
#' @param file A file path to save the xlsx file
#' @param overwrite Overwrite existing file (Defaults to TRUE)
#' @param header_width how many rows should be in the header?
#' @param number_format Number format used in Excel. `"auto"` (default) shows
#'   whole numbers without a decimal point and uses an en dash as minus sign.
#'   Alternatively, a custom Excel number format string.
#' @param italic_cols columns (specified by their name) to be italic
#' @param ... Further arguments passed to [openxlsx::buildWorkbook()].
#' @return The workbook object (invisibly).
#' @export
#' @examples
#' \dontrun{
#' bl_save_xlsx(table_to_write = iris, file = "my_path/iris_on_disk.xlsx")
#' }
bl_save_xlsx <- function(table_to_write, file, overwrite = TRUE, header_width = 1,
                         number_format = "auto", italic_cols = NULL, ...) {

    # replace hyphen-minus with en dash in all character columns
    table_to_write[] <- lapply(table_to_write, function(col) {
        if (is.character(col)) gsub("-", "\u2013", col, fixed = TRUE)
        else col
    })

    num_fmt <- if (number_format == "auto") 'General;"\u2013"General' else number_format

    wb <- openxlsx::buildWorkbook(table_to_write, asTable = FALSE, colWidths = "auto", ...)

    n_rows <- nrow(table_to_write)
    sheet  <- "Sheet 1"

    # wendet alle Styles auf eine Spaltengruppe an
    style_cols <- function(st, cols) {
        # body
        openxlsx::addStyle(wb, sheet, st$body,
                           rows = (header_width + 1):n_rows, cols = cols, gridExpand = TRUE)
        # header
        if (header_width == 1) {
            openxlsx::addStyle(wb, sheet, st$header, rows = 1, cols = cols, gridExpand = TRUE)
        } else {
            openxlsx::addStyle(wb, sheet, st$header_top, rows = 1, cols = cols, gridExpand = TRUE)
            if (header_width >= 3) {
                openxlsx::addStyle(wb, sheet, st$header_middle,
                                   rows = 2:(header_width - 1), cols = cols, gridExpand = TRUE)
            }
            openxlsx::addStyle(wb, sheet, st$header_bottom,
                               rows = header_width, cols = cols, gridExpand = TRUE)
        }
        # bottom row
        openxlsx::addStyle(wb, sheet, st$bottom,
                           rows = n_rows + 1, cols = cols, gridExpand = TRUE)
    }

    # zentriert: alle Spalten außer der ersten; linksbündig: erste Spalte
    if (ncol(table_to_write) > 1) {
        style_cols(bl_xlsx_styles(num_fmt, "center"), 2:ncol(table_to_write))
    }
    style_cols(bl_xlsx_styles(num_fmt, "left"),   1)

    # kursive Spalten: nur Body und letzte Zeile, Header bleibt normal
    if (!is.null(italic_cols)) {
        if (is.character(italic_cols)) {
            italic_cols <- match(italic_cols, names(table_to_write))
            if (anyNA(italic_cols)) stop("italic_cols: Spalte nicht gefunden.")
        }
        for (j in italic_cols) {
            halign <- if (j == 1) "left" else "center"
            st <- bl_xlsx_styles(num_fmt, halign, italic = TRUE)
            openxlsx::addStyle(wb, sheet, st$body,
                               rows = (header_width + 1):(n_rows),
                               cols = j, gridExpand = TRUE)
            openxlsx::addStyle(wb, sheet, st$bottom,
                               rows = n_rows + 1, cols = j, gridExpand = TRUE)
        }
    }

    openxlsx::saveWorkbook(wb, file = file, overwrite = overwrite)
    invisible(wb)
}
#### old ####
# bl_save_xlsx <- function (table_to_write, file, overwrite = T, header_width = 1,
#                           number_format = "auto",
#                            ...) {
#
#     # replace hyphen-minus with en dash in all character columns
#     table_to_write[] <- lapply(table_to_write, function(col) {
#         if (is.character(col)) gsub("-", "\u2013", col, fixed = TRUE)
#         else col
#     })
#
#     if (number_format == "auto"){
#         # format für Zahlen
#         num_fmt <- 'General;"–"General'
#     } else {
#         num_fmt <- number_format
#     }
#
#     # build workbook
#     wb <- openxlsx::buildWorkbook(table_to_write, asTable = F, colWidths = "auto", ...)
#
#     ## add centered stlyes to first row, body of the table, last row (all but first column)
#     # body of the table
#         openxlsx::addStyle(wb, sheet = "Sheet 1", style = ts, rows = ((header_width+1):length (table_to_write[,1])), cols = 2:length (table_to_write), gridExpand = T)
#     # header
#         ## normal: header_width == 1
#         if (header_width == 1) {
#             openxlsx::addStyle(wb, sheet = "Sheet 1", style = hs, rows = 1, cols = 2:length (table_to_write), gridExpand = T)
#         ## "large" header (header_width > 1)
#         } else {
#             # a) top line
#             openxlsx::addStyle(wb, sheet = "Sheet 1", style = hs_top, rows = 1, cols = 2:length (table_to_write), gridExpand = T)
#             # b) middle lines: only to be drawn if header width is
#             if (header_width >= 3) {
#                 openxlsx::addStyle(wb, sheet = "Sheet 1",
#                                    style = hs_middle,
#                                    rows = 2:(header_width-1), cols = 2:length (table_to_write), gridExpand = T)
#             }
#             # c) header bottom line
#             openxlsx::addStyle(wb, sheet = "Sheet 1",
#                                style = hs_bottom,
#                                rows = header_width, cols = 2:length (table_to_write), gridExpand = T)
#
#         }
#     # bottom row
#         openxlsx::addStyle(wb, sheet = "Sheet 1", style = bs, rows = length (table_to_write[,1])+1, cols = 2:length (table_to_write), gridExpand = T)
#
#
#     ## add left-aligned stlyes to first row, body of the table, last row (only first column)
#     # body of the table
#         openxlsx::addStyle(wb, sheet = "Sheet 1", style = ts1, rows = ((header_width+1):length (table_to_write[,1])), cols = 1, gridExpand = T)
#     # header
#         ## normal: header_width == 1
#         if (header_width == 1) {
#             openxlsx::addStyle(wb, sheet = "Sheet 1", style = hs1, rows = 1, cols = 1, gridExpand = T)
#         ## "large" header (header_width > 1)
#         } else {
#             # a) top line
#             openxlsx::addStyle(wb, sheet = "Sheet 1", style = hs1_top, rows = 1, cols = 1, gridExpand = T)
#             # b) middle lines: only to be drawn if header width is
#             if (header_width >= 3) {
#                 openxlsx::addStyle(wb, sheet = "Sheet 1",
#                                    style = hs1_middle,
#                                    rows = 2:(header_width-1), cols = 1, gridExpand = T)
#             }
#             # c) header bottom line
#             openxlsx::addStyle(wb, sheet = "Sheet 1",
#                                style = hs1_bottom,
#                                rows = header_width, cols = 1, gridExpand = T)
#
#         }
#     # bottom row
#         openxlsx::addStyle(wb, sheet = "Sheet 1", style = bs1, rows = length (table_to_write[,1])+1, cols = 1, gridExpand = T)
#
#     # save workbook to drive
#     openxlsx::saveWorkbook(wb,
#                  file = file,
#                  overwrite = overwrite)
#     invisible(wb)
# }
#
#
#
# ## create styles 1: centered (most of the time, all columns but the first one)
# # table style - the body of the table. no borders
# ts <- openxlsx::createStyle(
#   textDecoration = "", fontColour = "black", fontSize = 11,
#   fontName = "Garamond", fgFill = "white",
#   halign = "center",
#   #numFmt = '0.##;"–"0.##' #prints "-" as "–"
#   numFmt = num_fmt
# )
#
# # header style - the header of the table. double border at the top, thin border at the bottom
# hs <- openxlsx::createStyle(
#   textDecoration = "", fontColour = "black", fontSize = 11,
#   fontName = "Garamond", fgFill = "white",
#   border = "TopBottom", borderStyle = c ("double", "thin"),
#   halign = "center",
#   #numFmt = '0.##;"–"0.##'
#   numFmt = num_fmt
# )
#
# # sonderfall: "big" header
#     # a) the top header row
#     hs_top <- openxlsx::createStyle(
#       textDecoration = "", fontColour = "black", fontSize = 11,
#       fontName = "Garamond", fgFill = "white",
#       border = "Top", borderStyle = "double",
#       halign = "center",
#       #numFmt = '0.##;"–"0.##'
#       numFmt = num_fmt
#     )
#
#     # b) middle header rows (?)
#     hs_middle <- openxlsx::createStyle(
#       textDecoration = "",
#       fontColour = "black", fontSize = 11,
#       fontName = "Garamond", fgFill = "white",
#       # border = "TopBottom", borderStyle = c("double", "thin"),
#       halign = "center",
#       #numFmt = '0.##;"–"0.##'
#       numFmt = num_fmt
#     )
#
#     # c) header bottom rows
#     hs_bottom <- openxlsx::createStyle(
#       textDecoration = "",
#       fontColour = "black", fontSize = 11,
#       fontName = "Garamond", fgFill = "white",
#       border = "Bottom", borderStyle = "thin",
#       halign = "center",
#       #numFmt = '0.##;"–"0.##'
#       numFmt = num_fmt
#     )
#
# # bottom row style. the last row of the table. double border at the bottom
# bs <- openxlsx::createStyle(
#   textDecoration = "", fontColour = "black", fontSize = 11,
#   fontName = "Garamond", fgFill = "white",
#   border = "Bottom", borderStyle = c("double"),
#   halign = "center",
#   #numFmt = '0.##;"–"0.##'
#   numFmt = num_fmt
# )
#
#
# ## create styles 2: left-aligned (most of the time, the first column)
# ts1 <- openxlsx::createStyle(
#   textDecoration = "", fontColour = "black", fontSize = 11,
#   fontName = "Garamond", fgFill = "white",
#   halign = "left",
#   #numFmt = '0.##;"–"0.##'
#   numFmt = num_fmt
# )
#
# hs1 <- openxlsx::createStyle(
#   textDecoration = "", fontColour = "black", fontSize = 11,
#   fontName = "Garamond", fgFill = "white",
#   border = "TopBottom", borderStyle = c("double", "thin"),
#   halign = "left",
#   #numFmt = '0.##;"–"0.##'
#   numFmt = num_fmt
# )
#
# # sonderfall: "big" header
#     # a) the top header row
#     hs1_top <- openxlsx::createStyle(
#       textDecoration = "", fontColour = "black", fontSize = 11,
#       fontName = "Garamond", fgFill = "white",
#       border = "Top", borderStyle = "double",
#       halign = "left",
#       #numFmt = '0.##;"–"0.##'
#       numFmt = num_fmt
#     )
#
#     # b) middle header rows (?)
#     hs1_middle <- openxlsx::createStyle(
#       textDecoration = "",
#       fontColour = "black", fontSize = 11,
#       fontName = "Garamond", fgFill = "white",
#       # border = "TopBottom", borderStyle = c("double", "thin"),
#       halign = "left",
#       #numFmt = '0.##;"–"0.##'
#       numFmt = num_fmt
#     )
#
#     # c) header bottom rows
#     hs1_bottom <- openxlsx::createStyle(
#       textDecoration = "",
#       fontColour = "black", fontSize = 11,
#       fontName = "Garamond", fgFill = "white",
#       border = "Bottom", borderStyle = "thin",
#       halign = "left",
#       #numFmt = '0.##;"–"0.##'
#       numFmt = num_fmt
#     )
#
# bs1 <- openxlsx::createStyle(
#   textDecoration = "", fontColour = "black", fontSize = 11,
#   fontName = "Garamond", fgFill = "white",
#   border = "Bottom", borderStyle = c("double"),
#   halign = "left",
#   #numFmt = '0.##;"–"0.##'
#   numFmt = num_fmt
# )



# Hilfsfunktion ####
#' @noRd
# Hilfsfunktion: erzeugt alle Styles für ein bestimmtes Zahlenformat
bl_xlsx_styles <- function(num_fmt, halign, italic = FALSE) {
    mk <- function(border = NULL, borderStyle = "thin") {
        openxlsx::createStyle(
            textDecoration = if (italic) "italic" else "",
            fontColour = "black", fontSize = 11,
            fontName = "Garamond", fgFill = "white",
            border = border, borderStyle = borderStyle,
            halign = halign, numFmt = num_fmt
        )
    }
    list(
        body          = mk(),
        header        = mk(border = "TopBottom", borderStyle = c("double", "thin")),
        header_top    = mk(border = "Top",       borderStyle = "double"),
        header_middle = mk(),
        header_bottom = mk(border = "Bottom",    borderStyle = "thin"),
        bottom        = mk(border = "Bottom",    borderStyle = "double")
    )
}
# not needed at the moment... ####
# function to modify a style; set the font to italic if wanted
modifyStyle <- function(style, font_style = "italic", font_size = 11) {

    # check for possible font styles and modify font decoration accordingly
    if (font_style %in% c ("bold", "strikeout", "italic", "underline", "underline2", "accounting", "accounting2")) {
        style$fontDecoration <- font_style
    } else {
        # if none of the text styling possibilities are matched, add "no font decoration" (hence "normal" style)
        style$fontDecoration <- ""
        if (! (font_style %in% c ("bold", "strikeout", "italic", "underline", "underline2", "accounting", "accounting2", ""))) {
            warning ("font_style can be one of 'bold', 'strikeout', 'italic', 'underline', 'underline2', 'accounting' or 'accounting2'. Anything else will be coerced to '', resulting in 'normal font style'.")
        }
    }
    style$fontSize <- font_size
    #print (style)

  return(style)
}

