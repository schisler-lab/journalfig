# =============================================================================
# Font resolution. Never substitutes silently.
# Part of journalfig. See the package README for the pipeline this belongs to.
# =============================================================================

#' What is actually installed, so family strings are copied rather than guessed.
list_fonts <- function(pattern = "") {
  f <- systemfonts::system_fonts()
  f <- f[grepl(pattern, f$family, ignore.case = TRUE), c("family", "style")]
  f <- unique(f[order(f$family, f$style), ])
  print(f, row.names = FALSE)
  invisible(f)
}

#' Are this family's digits already the same width?
#'
#' Measured, not assumed. Open Sans and DejaVu ship tabular digits; Arial and
#' its clones do not and offer no tnum to switch on; MB Type ships a separate
#' " Tab" family instead of a feature.
digits_are_tabular <- function(family) {
  w <- function(s) systemfonts::shape_string(s, family = family, size = 40)$metrics$width
  isTRUE(all.equal(w("111"), w("000"), tolerance = 1e-6))
}

#' Resolve a font key (or a literal family name) to an installed family.
#'
#' Never substitutes silently. A missing font stops the render by default; the
#' old register_open_sans() had the same refusal and it is worth keeping,
#' because a silent fall back to Helvetica is not noticed until proof.
#'
#' Licensed faces (Concourse, Triplicate) are safe to use here because the
#' pipeline outlines in Illustrator before anything ships, so the font itself
#' never travels. Only a collaborator who must edit the LIVE file needs it
#' installed, which is what font = "safe" is for.
resolve_font <- function(font = jf()$default_font, allow_fallback = FALSE,
                         tabular = TRUE) {
  family <- if (!is.null(jf()$fonts[[font]])) jf()$fonts[[font]] else font
  have   <- family %in% systemfonts::system_fonts()$family
  if (!have) {
    msg <- sprintf(paste0("font '%s' is not installed on this machine. ",
                          "list_fonts() shows what is. Not rendering in a ",
                          "substitute face."), family)
    if (!allow_fallback) stop(msg, call. = FALSE)
    warning(msg, " Falling back to '", jf()$fonts$safe, "'.", call. = FALSE)
    family <- jf()$fonts$safe
    if (!(family %in% systemfonts::system_fonts()$family))
      stop("fallback font '", family, "' is not installed either.", call. = FALSE)
  }
  # Tabular (monospaced) figures keep stacked axis numbers from jittering.
  #
  # Two routes, and which one applies depends on the foundry. MB Type ships
  # tabular figures as a SEPARATE FAMILY sitting beside the base one, so
  # "Concourse 3 Tab" is its own installed family and no OpenType feature is
  # involved. Everyone else exposes tnum on the base family, if at all. Prefer
  # the sibling family when it exists, because a tnum variant registered over a
  # font that has no such feature succeeds silently and changes nothing, which
  # is how this went unnoticed for Open Sans.
  if (tabular) {
    # 1. A real installed sibling family. MB Type ships these, and because the
    #    name resolves through the OS every device honours it.
    sibling <- paste(family, "Tab")
    if (sibling %in% systemfonts::system_fonts()$family) return(sibling)

    # 2. Already tabular, so asking for it again buys nothing. Returning the
    #    base family here is not an optimisation, it is a CORRECTNESS fix:
    #    a registered variant is a systemfonts alias that svglite understands
    #    and cairo_pdf does not, because cairo resolves fonts through
    #    fontconfig, which has never heard of the alias and silently falls back
    #    to DejaVu Sans. That is why one figure came out with Open Sans
    #    everywhere in the SVG and DejaVu on the axis numbers in the PDF.
    if (digits_are_tabular(family)) return(family)

    # 3. Genuinely proportional with no sibling. Register the variant, and say
    #    out loud that the PDF will substitute, because nothing downstream will.
    variant <- paste0(family, " (tnum)")
    ok <- try(systemfonts::register_variant(
      name = variant, family = family,
      features = systemfonts::font_feature(numbers = "tabular")), silent = TRUE)
    if (!inherits(ok, "try-error")) {
      warning("'", family, "' needs a registered tnum variant for tabular ",
              "figures. svglite honours it; cairo_pdf does NOT and will ",
              "substitute. Prefer a face with a real ' Tab' family, or accept ",
              "proportional digits.", call. = FALSE)
      return(variant)
    }
    warning("no tabular figures available for '", family,
            "'; using proportional.", call. = FALSE)
  }
  family
}

#' Does this face already have tabular digits, and does tnum change anything?
#' Answers the question directly instead of leaving you to wonder whether the
#' variant did something. Widths are at 20 pt for readability.
font_report <- function(font = jf()$default_font) {
  fam <- if (!is.null(jf()$fonts[[font]])) jf()$fonts[[font]] else font
  if (!(fam %in% systemfonts::system_fonts()$family))
    stop("'", fam, "' is not installed. list_fonts() shows what is.", call. = FALSE)
  w <- function(s, f) systemfonts::shape_string(s, family = f, size = 20)$metrics$width
  tab <- resolve_font(font, tabular = TRUE)
  d <- c(prop_1 = w("111", fam), prop_0 = w("000", fam),
         tab_1  = w("111", tab), tab_0  = w("000", tab))
  already <- isTRUE(all.equal(unname(d[["prop_1"]]), unname(d[["prop_0"]])))
  fixed   <- isTRUE(all.equal(unname(d[["tab_1"]]),  unname(d[["tab_0"]])))
  message(sprintf("%s: default 111=%.1f 000=%.1f | tnum 111=%.1f 000=%.1f",
                  fam, d[["prop_1"]], d[["prop_0"]], d[["tab_1"]], d[["tab_0"]]))
  # Report the route resolve_font ACTUALLY takes, not the one it might. This
  # said "the tnum feature" for a face whose digits are already tabular, where
  # resolve_font correctly returns the base family and registers nothing.
  route <- if (identical(tab, paste(fam, "Tab")))
             sprintf("a separate '%s Tab' family", fam)
           else if (identical(tab, fam))
             "none needed, the base family is used as is"
           else
             sprintf("a registered variant '%s', which cairo_pdf CANNOT see", tab)
  message(sprintf("  tabular route: %s", route))
  message(if (already) "  base digits are ALREADY tabular; nothing to switch on"
          else if (fixed) "  base digits are proportional; the tabular route fixes them"
          else "  base digits are proportional and NOTHING fixes them here")
  invisible(d)
}

#' The family to use for BOLD text.
#'
#' Concourse's three lighter weights use weight 6 as their bold rather than
#' carrying one internally, so ggplot's face = "bold" would synthesize a smeared
#' fake bold. Where jf()$bold_for names a real bold partner, use that family and
#' leave face alone. Because weights 2, 3, 4 and 6 are duplexed to identical
#' character widths, swapping in the bold never reflows anything.
resolve_font_bold <- function(font = jf()$default_font, allow_fallback = FALSE) {
  fam  <- resolve_font(font, allow_fallback = allow_fallback, tabular = FALSE)
  pair <- jf()$bold_for[[fam]]
  if (!is.null(pair) && pair %in% systemfonts::system_fonts()$family)
    list(family = pair, face = "plain")
  else list(family = fam, face = "bold")
}
