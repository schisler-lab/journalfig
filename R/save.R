# =============================================================================
# Export. Live text, final print size, no scale factor.
# Part of journalfig. See the package README for the pipeline this belongs to.
# =============================================================================

#' Save a finished figure at final print size, with live text
#'
#' Exports once, at the size the figure will print. There is no scale factor
#' and there is nothing to shrink afterwards.
#'
#' SVG is the Illustrator handoff, written by svglite with
#' `fix_text_size = FALSE`. That flag is not optional: by default every string
#' is pinned to a fixed `textLength`, and editing that text in Illustrator then
#' stretches or squeezes it back to the old width.
#'
#' PDF via cairo embeds a font SUBSET. The text is live, but you are editing
#' against a subset, so typing a glyph it does not carry is where people get
#' stuck. Use it when a PDF is required and prefer SVG.
#'
#' PNG is work in progress only and never a deliverable.
#'
#' With `check = TRUE` the written files are reopened and asserted against the
#' spec, so a figure that misses it fails here rather than at proof.
#'
#' @param plot A ggplot or patchwork composition.
#' @param name File name without extension.
#' @param width Figure width key, see [fig_width()].
#' @param height_mm Height in millimetres. Capped by the configuration's page
#'   maximum.
#' @param fmt Which formats to write.
#' @param outdir Directory to write into, created if needed.
#' @param font Face to check against, defaulting to the active one.
#' @param check Reopen and verify what was written.
#' @return The paths written, invisibly.
#' @seealso [check_journal()], [check_pdf_fonts()]
#' @export
save_journal <- function(plot, name, width = "1col", height_mm,
                         fmt = c("svg", "pdf", "png"), outdir = "figures",
                         font = jf_font(), check = TRUE) {
  fmt <- match.arg(fmt, several.ok = TRUE)
  w_mm <- fig_width(width)
  if (height_mm > jf()$max_height_mm)
    stop(sprintf("height %.0f mm exceeds the %.0f mm page maximum.",
                 height_mm, jf()$max_height_mm), call. = FALSE)

  np <- .count_placeholders(plot)
  if (np > 0)
    warning(sprintf(paste0("'%s' still contains %d placeholder panel(s). ",
                           "This figure is NOT submission ready."), name, np),
            call. = FALSE)

  # showtext outlines text. Off for every export, restored afterwards.
  had_showtext <- "showtext" %in% loadedNamespaces()
  if (had_showtext) showtext::showtext_auto(FALSE)
  on.exit(if (had_showtext) showtext::showtext_auto(TRUE), add = TRUE)

  dir.create(outdir, showWarnings = FALSE, recursive = TRUE)
  w_in <- mm_to_in(w_mm); h_in <- mm_to_in(height_mm)
  out <- character(0)

  if ("svg" %in% fmt) {
    f <- file.path(outdir, paste0(name, ".svg"))
    ggsave(f, plot, width = w_in, height = h_in, units = "in",
           device = function(filename, ...)
             svglite::svglite(filename, ..., fix_text_size = FALSE))
    out <- c(out, f)
  }
  if ("pdf" %in% fmt) {
    f <- file.path(outdir, paste0(name, ".pdf"))
    ggsave(f, plot, width = w_in, height = h_in, units = "in",
           device = grDevices::cairo_pdf)
    out <- c(out, f)
  }
  if ("png" %in% fmt) {
    f <- file.path(outdir, paste0(name, ".png"))
    ggsave(f, plot, width = w_in, height = h_in, units = "in",
           dpi = jf()$wip_png_dpi, device = ragg::agg_png)
    out <- c(out, f)
  }

  message(sprintf("saved %s  (%s = %.0f x %.0f mm)",
                  paste(basename(out), collapse = ", "), width, w_mm, height_mm))
  if (check) {
    if (any(grepl("\\.svg$", out)))
      check_journal(grep("\\.svg$", out, value = TRUE), font = font)
    if (any(grepl("\\.pdf$", out)))
      check_pdf_fonts(grep("\\.pdf$", out, value = TRUE), font = font)
  }
  invisible(out)
}
