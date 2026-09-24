# =============================================================================
# Figure and panel geometry. The key describes the FIGURE.
# Part of journalfig. See the package README for the pipeline this belongs to.
# =============================================================================

#' Width of a finished figure
#'
#' The key describes the FIGURE, never a panel. A figure is one column, one and
#' a half, or the full page, and everything inside it is derived from that, so
#' nobody types a panel width by hand and no two panels in one figure disagree
#' about how wide the figure is.
#'
#' @param key One of the keys in the configuration, by default `"1col"`,
#'   `"1.5col"` or `"2col"`.
#' @return Width in millimetres.
#' @seealso [panel_width()]
#' @examples
#' fig_width("2col")
#' @export
fig_width <- function(key = "1col") {
  w <- jf()$widths_mm[[key]]
  if (is.null(w)) stop("width key must be one of: ",
                       paste(names(jf()$widths_mm), collapse = ", "), call. = FALSE)
  w
}

#' Width of one panel among several
#'
#' What one panel gets when `n` of them sit side by side in a figure of that
#' key, after the gutters between them are taken out.
#'
#' @param key Figure width key, see [fig_width()].
#' @param n Number of panels sharing the row.
#' @param gutter Space between panels, in millimetres.
#' @return Width of a single panel in millimetres.
#' @examples
#' panel_width("2col", n = 2)
#' @export
panel_width <- function(key = "2col", n = 1, gutter = jf()$gutter_mm)
  (fig_width(key) - gutter * (n - 1)) / n
