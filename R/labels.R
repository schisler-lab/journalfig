# =============================================================================
# Panel tags. Journals disagree, so it is a parameter.
# Part of journalfig. See the package README for the pipeline this belongs to.
# =============================================================================

#' Journals disagree: Nature wants bold lowercase a, Science and Cell want
#' uppercase A, some want a trailing period or a parenthesis. So it is a
#' parameter set once per figure, not a constant.
panel_tag <- function(i, style = jf()$default_labels) {
  ch <- if (grepl("^upper", style)) LETTERS[i] else letters[i]
  if (grepl("paren", style)) paste0("(", ch, ")")
  else if (grepl("period", style)) paste0(ch, ".")
  else ch
}

#' Tag a panel. Applied through patchwork's tag system at assembly, or directly
#' on a single plot.
annotate_panel <- function(p, i, style = jf()$default_labels,
                           font = jf_font(), allow_fallback = FALSE) {
  b <- resolve_font_bold(font, allow_fallback = allow_fallback)
  p + labs(tag = panel_tag(i, style)) +
    theme(plot.tag = element_text(
            size = jf()$type_pt$panel_label, face = b$face, family = b$family,
            hjust = 0, vjust = 1),
          plot.tag.position = c(0, 1))
}
