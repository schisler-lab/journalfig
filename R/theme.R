# =============================================================================
# The journal theme.
# Part of journalfig. See the package README for the pipeline this belongs to.
# =============================================================================

#' Journal theme. `width` is the FIGURE key, and every panel in one figure gets
#' the same theme, so type size is uniform across the figure rather than varying
#' panel by panel.
#'
#' @param ticks which axes carry ticks. Categorical axes get none.
theme_journal <- function(width = "1col", font = jf()$default_font,
                          ticks = c("none", "x", "y", "both"),
                          allow_fallback = FALSE) {
  ticks  <- match.arg(ticks)
  # Tabular figures only where numbers STACK and the eye reads them as a column,
  # which is the y axis. Measured: Open Sans and DejaVu ship tabular digits
  # already, so the variant is a no-op there; Liberation Sans (the Arial metric
  # clone) is proportional and carries no tnum to switch on, so an Arial figure
  # cannot be fixed; Concourse advertises tnum, meaning its defaults are
  # proportional and this is where the variant earns its keep.
  #
  # Everything else stays PROPORTIONAL. In a string like "rho = +0.50, p <
  # 0.001" tabular pads every narrow digit out to a zero's width and the line
  # reads gappy. Centered x-axis labels are indifferent either way.
  family     <- resolve_font(font, allow_fallback = allow_fallback, tabular = FALSE)
  family_tab <- resolve_font(font, allow_fallback = allow_fallback, tabular = TRUE)
  bold       <- resolve_font_bold(font, allow_fallback = allow_fallback)
  ty <- jf()$type_pt; st <- jf()$stroke_pt
  # Structure is GRAY, content is BLACK. Axis lines and ticks recede; tick
  # labels are black regular; axis titles are black bold. From the
  # schisler-lab-figures skill. journalfig owns structure colour only; data colour
  # belongs to the project palette and this package never touches it.
  AXIS_GRAY <- jf()$colour$structure

  t <- theme_classic(base_size = ty$axis_text, base_family = family) +
    theme(
      plot.title    = element_blank(),
      plot.subtitle = element_blank(),
      axis.title    = element_text(colour = "black", size = ty$axis_title,
                                   face = bold$face, family = bold$family),
      axis.text     = element_text(colour = "black", size = ty$axis_text),
      axis.text.y   = element_text(colour = "black", size = ty$axis_text,
                                   family = family_tab),
      axis.line     = element_line(colour = AXIS_GRAY,
                                   linewidth = pt_to_mm(st$axis)),
      axis.ticks    = element_blank(),
      panel.grid    = element_blank(),
      panel.border  = element_blank(),
      legend.text   = element_text(size = ty$legend),
      legend.title  = element_text(size = ty$legend),
      legend.key.size = grid::unit(2.6, "mm"),
      legend.margin   = margin(0, 0, 0, 0),
      legend.background = element_blank(),
      strip.text      = element_text(size = ty$axis_text, colour = "black"),
      strip.background = element_blank(),
      plot.margin = margin(1, 1, 1, 1, "mm")
    )
  tk <- element_line(colour = AXIS_GRAY, linewidth = pt_to_mm(st$tick))
  if (ticks %in% c("x", "both")) t <- t + theme(axis.ticks.x = tk)
  if (ticks %in% c("y", "both")) t <- t + theme(axis.ticks.y = tk)
  t
}

#' In-panel statistic. Points in, mm out.
annotate_stat <- function(label, x = -Inf, y = Inf, hjust = -0.1, vjust = 1.3,
                          colour = jf()$colour$annotation, font = jf()$default_font,
                          allow_fallback = FALSE) {
  # family must be set explicitly: annotate() does NOT inherit the theme font,
  # so without this the stat renders in the device default and everything else
  # renders in Open Sans.
  annotate("text", x = x, y = y, label = label, hjust = hjust, vjust = vjust,
           size = pt_to_size(jf()$type_pt$annotation), colour = colour,
           family = resolve_font(font, allow_fallback = allow_fallback,
                                 tabular = FALSE),
           lineheight = 0.95)
}
