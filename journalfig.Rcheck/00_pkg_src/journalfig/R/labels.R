# =============================================================================
# Panel tags. Journals disagree, so it is a parameter.
# Part of journalfig. See the package README for the pipeline this belongs to.
# =============================================================================

#' Format a panel tag
#'
#' Journals disagree. Nature wants bold lowercase a, Science and Cell want
#' uppercase A, some want a trailing period or a parenthesis. So the style is a
#' parameter set once per figure rather than a constant.
#'
#' @param i Panel number, counting from 1.
#' @param style One of `"lower"`, `"upper"`, optionally suffixed `"paren"` or
#'   `"period"`, for example `"upper-paren"`.
#' @return The tag as a single string.
#' @examples
#' panel_tag(3, "upper-paren")
#' @export
panel_tag <- function(i, style = jf()$default_labels) {
  ch <- if (grepl("^upper", style)) LETTERS[i] else letters[i]
  if (grepl("paren", style)) paste0("(", ch, ")")
  else if (grepl("period", style)) paste0(ch, ".")
  else ch
}

#' Tag a single panel
#'
#' For a plot standing on its own. A multi-panel figure is usually tagged
#' through patchwork's own tag system at assembly instead, so the tags are
#' numbered by the layout rather than by hand.
#'
#' @param p A ggplot.
#' @param i Panel number, counting from 1.
#' @param style Tag style, see [panel_tag()].
#' @param font Face to use, defaulting to the active one.
#' @param allow_fallback Permit the safe face if `font` is not installed.
#'   `FALSE` stops instead, which is the point.
#' @return The plot with a tag.
#' @seealso [panel_tag()]
#' @export
annotate_panel <- function(p, i, style = jf()$default_labels,
                           font = jf_font(), allow_fallback = FALSE) {
  b <- resolve_font_bold(font, allow_fallback = allow_fallback)
  p + labs(tag = panel_tag(i, style)) +
    theme(plot.tag = element_text(
            size = jf()$type_pt$panel_label, face = b$face, family = b$family,
            hjust = 0, vjust = 1),
          plot.tag.position = c(0, 1))
}
