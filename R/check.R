# =============================================================================
# Enforcement. A rule nobody can violate beats one everybody agrees with.
# Part of journalfig. See the package README for the pipeline this belongs to.
# =============================================================================

#' Reopen an exported SVG and assert it matches the spec
#'
#' Checks five things: that the text is live rather than outlined, that every
#' family is the one you asked for, that nothing sits below the type floor or
#' the stroke floor, and that the figure is a standard column width.
#'
#' Type or strokes landing exactly ON a floor are reported as such. A floor is
#' the boundary a journal rejects at, so type parked on it has no headroom and
#' one late rescale turns a pass into a reject.
#'
#' Every rule here existed in prose first and was broken anyway, which is why
#' it is checked rather than documented.
#'
#' @param file Path to an SVG written by [save_journal()].
#' @param font Face to check against, defaulting to the active one.
#' @param verbose Print what was measured.
#' @return `TRUE` invisibly. Stops on any failure.
#' @seealso [check_pdf_fonts()]
#' @export
check_journal <- function(file, font = jf_font(), verbose = TRUE) {
  svg <- paste(readLines(file, warn = FALSE), collapse = "\n")
  fail <- character(0); note <- character(0)

  # 1. live text, not outlines
  n_text <- length(gregexpr("<text", svg)[[1]])
  if (!grepl("<text", svg)) fail <- c(fail, "no <text> elements: text was outlined")
  else note <- c(note, sprintf("%d live text elements", n_text))

  # 2. the font you asked for
  want <- if (!is.null(jf()$fonts[[font]])) jf()$fonts[[font]] else font
  fams <- unique(gsub('font-family: *"?([^";]*)"?.*', "\\1",
                      regmatches(svg, gregexpr('font-family: *[^;]*', svg))[[1]]))
  fams <- trimws(gsub('"', "", fams))
  ok_fams <- c(want, paste(want, "Tab"), paste0(want, " (tnum)"))
  if (!is.null(jf()$bold_for[[want]]))
    ok_fams <- c(ok_fams, jf()$bold_for[[want]], paste(jf()$bold_for[[want]], "Tab"))
  bad  <- setdiff(fams, ok_fams)
  if (length(bad)) fail <- c(fail, paste0("unexpected font(s): ",
                                          paste(bad, collapse = ", ")))
  else note <- c(note, paste0("font: ", paste(fams, collapse = ", ")))

  # 3. nothing below the type floor
  sz <- as.numeric(gsub("[^0-9.]", "",
         regmatches(svg, gregexpr("font-size: *[0-9.]+", svg))[[1]]))
  if (length(sz) && min(sz) < jf()$type_pt$floor - 0.01)
    fail <- c(fail, sprintf("text at %.2f pt is below the %.1f pt floor",
                            min(sz), jf()$type_pt$floor))
  else if (length(sz)) {
    # A floor is a rejection boundary, not a target. Type sitting on it has no
    # headroom: any late rescale in Illustrator, or a journal that rounds the
    # other way, turns a pass into a reject. Say so rather than reporting a
    # clean number.
    at_floor <- min(sz) < jf()$type_pt$floor + 0.05
    note <- c(note, sprintf("smallest text %.2f pt%s", min(sz),
                            if (at_floor) " (AT THE FLOOR, no headroom)" else ""))
  }

  # 4. nothing below the stroke floor
  # svglite writes stroke-width in px at 96 per inch, while font-size is already
  # in points. Measured against known inputs: 0.7 pt lands as 0.53, 0.55 as
  # 0.41, 0.9 as 0.68, a constant 0.75 factor. Convert back before comparing, or
  # the floor rejects every correctly built figure.
  sw <- as.numeric(gsub("[^0-9.]", "",
         regmatches(svg, gregexpr("stroke-width: *[0-9.]+", svg))[[1]])) * (96 / 72)
  sw <- sw[sw > 0]
  if (length(sw) && min(sw) < jf()$stroke_pt$floor - 0.01)
    fail <- c(fail, sprintf("stroke at %.2f pt is below the %.1f pt floor",
                            min(sw), jf()$stroke_pt$floor))
  else if (length(sw)) note <- c(note, sprintf("thinnest stroke %.2f pt", min(sw)))

  # 5. a standard width
  wpx <- suppressWarnings(as.numeric(gsub("[^0-9.]", "",
           regmatches(svg, regexpr("width='[0-9.]+pt'", svg)))))
  if (length(wpx) && !is.na(wpx)) {
    w_mm <- wpx * 25.4 / 72
    if (min(abs(unlist(jf()$widths_mm) - w_mm)) > 0.5)
      fail <- c(fail, sprintf("width %.1f mm is not a standard column (%s)",
                              w_mm, paste(unlist(jf()$widths_mm), collapse = "/")))
    else note <- c(note, sprintf("width %.0f mm", w_mm))
  }

  if (verbose) message("  check: ", paste(note, collapse = " | "))
  if (length(fail)) {
    for (f in fail) message("  FAIL: ", f)
    stop(sprintf("%s does not meet the journal spec (%d problem%s).",
                 basename(file), length(fail), if (length(fail) > 1) "s" else ""),
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Which fonts did the PDF actually embed?
#'
#' The SVG check cannot see this. cairo_pdf resolves fonts through fontconfig
#' rather than systemfonts, so a family systemfonts knows about but the
#' operating system does not gets silently swapped, and the SVG from the same
#' render still looks perfect. That is how a figure passed with the right face
#' throughout its SVG while its PDF carried DejaVu Sans on every axis number.
#'
#' Cairo names the embedded FACE rather than the family, so the comparison is
#' on the stem: a real bold is not mistaken for a substitution.
#'
#' @param file Path to a PDF written by [save_journal()].
#' @param font Face to check against, defaulting to the active one.
#' @param verbose Print the families found.
#' @return `TRUE` invisibly. Stops if anything was substituted.
#' @seealso [check_journal()]
#' @export
check_pdf_fonts <- function(file, font = jf_font(), verbose = TRUE) {
  # Read as RAW and search with grepRaw. The earlier version did
  # rawToChar(raw[raw != as.raw(0)]) first, which mangles a binary PDF badly
  # enough to lose /BaseFont entries entirely: a figure rendered in Concourse
  # embedded four faces, two of them the wrong ones, and this returned nothing
  # and passed it clean.
  raw <- readBin(file, "raw", file.info(file)$size)
  pos <- grepRaw("/BaseFont", raw, all = TRUE, fixed = TRUE)
  fonts <- character(0)
  for (i in pos) {
    chunk <- rawToChar(raw[i:min(i + 80L, length(raw))])
    m <- regmatches(chunk, regexpr("/BaseFont\\s*/[A-Za-z0-9+#.-]+", chunk))
    if (length(m))
      fonts <- c(fonts, sub("^[A-Z]{6}\\+", "", sub("/BaseFont\\s*/", "", m)))
  }
  fonts <- unique(fonts)
  if (!length(fonts)) return(invisible(TRUE))

  # cairo names the embedded FACE ("OpenSans-Bold"), not the family, so compare
  # on the stem rather than for equality or a real bold trips the check.
  want <- if (!is.null(jf()$fonts[[font]])) jf()$fonts[[font]] else font
  ok   <- c(want, paste(want, "Tab"), jf()$bold_for[[want]])
  ok   <- gsub("[^A-Za-z0-9]", "", ok[!vapply(ok, is.null, logical(1))])
  norm <- gsub("[^A-Za-z0-9]", "", fonts)
  bad  <- fonts[!vapply(norm, function(f)
            any(startsWith(f, ok) | startsWith(ok, f)), logical(1))]

  if (verbose) message("  pdf fonts: ", paste(fonts, collapse = ", "))
  if (length(bad))
    stop(sprintf(paste0("%s embedded a substituted font: %s. cairo_pdf resolves ",
                        "through fontconfig, so a systemfonts-only alias (a ",
                        "registered variant) is NOT visible to it. Use a real ",
                        "installed family."),
                 basename(file), paste(bad, collapse = ", ")), call. = FALSE)
  invisible(TRUE)
}
