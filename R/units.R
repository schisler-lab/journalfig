# =============================================================================
# Unit helpers.
#
# ggplot is inconsistent about units and it catches everyone:
#   theme element_text size  -> POINTS
#   theme element_line width -> MILLIMETRES
#   geom_* linewidth         -> MILLIMETRES
#   geom_text/label size     -> MILLIMETRES
#
# So a spec written in points has to be converted for every layer except
# element_text. These three do it.
# =============================================================================

#' Convert points to millimetres
#'
#' For any `linewidth`, in a theme or in a geom. The spec is written in points
#' because that is how journals state their minimums, and ggplot wants
#' millimetres for everything that draws a line.
#'
#' @param pt Size in points.
#' @return The same size in millimetres.
#' @seealso [pt_to_size()] for text, [mm_to_in()] for devices.
#' @examples
#' pt_to_mm(0.9)
#' @export
pt_to_mm   <- function(pt) pt * 25.4 / 72

#' Convert points to ggplot's text size unit
#'
#' For [ggplot2::geom_text()] and [ggplot2::geom_label()], whose `size` is in
#' millimetres rather than the points an `element_text` takes. Text set inside
#' a theme needs no conversion; text drawn as a layer does.
#'
#' @param pt Size in points.
#' @return The value to pass as a geom's `size`.
#' @examples
#' pt_to_size(6.5)
#' @export
pt_to_size <- function(pt) pt / ggplot2::.pt

#' Convert millimetres to inches
#'
#' Graphics devices take inches. Figure geometry is specified in millimetres,
#' so this is the last conversion before a device opens.
#'
#' @param mm Length in millimetres.
#' @return The same length in inches.
#' @examples
#' mm_to_in(183)
#' @export
mm_to_in   <- function(mm) mm / 25.4
