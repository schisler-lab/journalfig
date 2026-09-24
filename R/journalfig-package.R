#' journalfig: manuscript figures at final print size, with live text
#'
#' Figures are built at the size they will print, in millimetres, and exported
#' with live text so Illustrator can assemble and annotate them without
#' anything being re-rendered. Nothing is designed large and shrunk, because
#' shrinking takes the type down with it.
#'
#' The pipeline: code, live-text SVG, Illustrator assembly, outline, submit.
#'
#' Three things are worth knowing before the first figure.
#'
#' Set the face once with [jf_font()], before building any layer. Every
#' function here defaults to it, and R evaluates default arguments lazily, so
#' anything that sets the font while a plot is being built arrives too late for
#' the layers beside it.
#'
#' ggplot is inconsistent about units. Text in a theme is in points; linewidths
#' and [ggplot2::geom_text()] sizes are in millimetres. Use [pt_to_mm()] and
#' [pt_to_size()] rather than typing a converted number.
#'
#' [save_journal()] reopens what it wrote and refuses anything that misses the
#' spec. Every rule it checks existed in prose first and got broken anyway.
#'
#' @keywords internal
#' @importFrom ggplot2 ggplot aes annotate element_blank element_line
#'   element_text ggsave labs margin scale_x_continuous scale_y_continuous
#'   theme theme_classic theme_void expansion
#' @importFrom stats setNames
#' @importFrom utils combn
"_PACKAGE"
