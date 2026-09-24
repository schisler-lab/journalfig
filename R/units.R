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
# element_text. These two do it.
# =============================================================================

#' Points to millimetres, for linewidth.
pt_to_mm   <- function(pt) pt * 25.4 / 72

#' Points to ggplot's text size unit, for geom_text and geom_label.
pt_to_size <- function(pt) pt / ggplot2::.pt

#' Millimetres to inches, for the device.
mm_to_in   <- function(mm) mm / 25.4
