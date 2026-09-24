# =============================================================================
# Figure and panel geometry. The key describes the FIGURE.
# Part of journalfig. See the package README for the pipeline this belongs to.
# =============================================================================

#' Width of a FINISHED figure, in mm. The key describes the figure, never a
#' panel: panel widths are derived so nobody types one by hand.
fig_width <- function(key = "1col") {
  w <- jf()$widths_mm[[key]]
  if (is.null(w)) stop("width key must be one of: ",
                       paste(names(jf()$widths_mm), collapse = ", "), call. = FALSE)
  w
}

#' Width of ONE panel when n sit side by side in a figure of that key.
panel_width <- function(key = "2col", n = 1, gutter = jf()$gutter_mm)
  (fig_width(key) - gutter * (n - 1)) / n

mm_to_in <- function(mm) mm / 25.4
