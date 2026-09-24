# =============================================================================
# Slots for art that code cannot generate.
# Part of journalfig. See the package README for the pipeline this belongs to.
# =============================================================================

#' An empty, correctly sized slot for a panel that code cannot generate: a
#' design schematic, a pathway diagram, a microscopy montage.
#'
#' It composes like any other panel, so the figure carries its true final
#' dimensions from the first day rather than from assembly, and Illustrator gets
#' an exact box to drop art into. save_journal() warns on every export that
#' still contains one, so an unfilled slot cannot quietly become the submitted
#' version.
panel_placeholder <- function(note = "art to be placed", font = jf_font(),
                              allow_fallback = FALSE) {
  family <- resolve_font(font, allow_fallback = allow_fallback, tabular = FALSE)
  p <- ggplot() +
    annotate("rect", xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = NA,
             colour = jf()$colour$structure, linewidth = pt_to_mm(jf()$stroke_pt$floor),
             linetype = "22") +
    annotate("text", x = 0.5, y = 0.55, label = note, family = family,
             size = pt_to_size(jf()$type_pt$axis_text), colour = jf()$colour$annotation) +
    annotate("text", x = 0.5, y = 0.40, label = "PLACEHOLDER", family = family,
             size = pt_to_size(jf()$type_pt$floor), colour = jf()$colour$structure) +
    scale_x_continuous(limits = c(0, 1), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_void(base_family = family)
  attr(p, "jf_placeholder") <- TRUE
  p
}

.collect_plots <- function(x) {
  if (inherits(x, "patchwork")) {
    kids <- tryCatch(x$patches$plots, error = function(e) list())
    self <- x; class(self) <- setdiff(class(self), "patchwork")
    return(c(unlist(lapply(kids, .collect_plots), recursive = FALSE), list(self)))
  }
  list(x)
}
.count_placeholders <- function(x)
  sum(vapply(.collect_plots(x),
             function(k) isTRUE(attr(k, "jf_placeholder")), logical(1)))
