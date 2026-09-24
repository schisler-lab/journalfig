# =============================================================================
# Configuration. Package defaults ship in inst/extdata/journal_fig.json and a
# project overrides them by putting its own journal_fig.json at its root.
#
# The split matters for a lab: geometry and type are LAB-WIDE and should not
# vary between projects, while the font registry is PER-MACHINE, because
# licensed families are installed on some machines and not others. Shipping the
# licensed names in the package would make it fail on a collaborator's machine
# for no reason.
# =============================================================================

.jf_env <- new.env(parent = emptyenv())

.jf_merge <- function(base, over) {
  for (k in names(over)) {
    base[[k]] <- if (is.list(base[[k]]) && is.list(over[[k]]))
      .jf_merge(base[[k]], over[[k]]) else over[[k]]
  }
  base
}

#' Point journalfig at a project override file
#'
#' Geometry and type are lab-wide and ship with the package. The font registry
#' is per-machine, because licensed families are installed on some machines and
#' not others, so only the collaborator-safe face ships. A project overrides by
#' putting `journal_fig.json` at its root, naming only what differs; it is
#' deep-merged over the defaults.
#'
#' Call with no argument to reload, or to let it find the file itself.
#'
#' @param path Path to an override JSON, or `NULL` to look for
#'   `journal_fig.json` in the working directory and then in `R/`.
#' @return The merged configuration, invisibly.
#' @seealso [jf()] to read it, [jf_font()] to choose a face.
#' @examples
#' \dontrun{
#' jf_config("journal_fig.json")
#' }
#' @export
jf_config <- function(path = NULL) {
  base <- jsonlite::fromJSON(
    system.file("extdata", "journal_fig.json", package = "journalfig"))
  if (is.null(path)) {
    guess <- c("journal_fig.json", file.path("R", "journal_fig.json"))
    hit   <- guess[file.exists(guess)]
    path  <- if (length(hit)) hit[1] else NULL
  }
  if (!is.null(path) && file.exists(path)) {
    base <- .jf_merge(base, jsonlite::fromJSON(path))
    base$`_override` <- normalizePath(path)
  }
  .jf_env$cfg <- base
  invisible(base)
}

#' The active configuration
#'
#' Loads on first use, so a script that needs no overrides never has to call
#' [jf_config()].
#'
#' @return The configuration as a nested list: `widths_mm`, `type_pt`,
#'   `stroke_pt`, `colour`, `fonts`, `bold_for` and the rest.
#' @examples
#' jf()$type_pt$floor
#' @export
jf <- function() {
  if (is.null(.jf_env$cfg)) jf_config()
  .jf_env$cfg
}

#' Get or set the face this figure is using
#'
#' The face is a decision about a FIGURE, not about a call, so it is set once
#' and every function in the package defaults to it.
#'
#' Call it FIRST, before building any layer. It is deliberately not a side
#' effect of [theme_journal()]. R evaluates a default argument lazily, at the
#' moment the function first needs it, so in `p + annotate_stat(...) +
#' theme_journal(...)` the annotation resolves its font before the theme is
#' ever evaluated. Anything that sets the face while a plot is being built
#' arrives too late for the layers next to it, and the figure comes out mixed
#' according to the order somebody happened to type.
#'
#' Skipping it is not loud. Every call site independently falls back to the
#' safe face, so a figure comes out mostly in the default with a few strings in
#' the family you thought you had asked for. [check_journal()] and
#' [check_pdf_fonts()] catch that afterwards; this prevents it.
#'
#' @param font A key in the configuration's font registry, or a family name
#'   outright. Omit to read the active face rather than set it.
#' @return The active face. Invisibly when setting.
#' @seealso [resolve_font()], [font_report()]
#' @examples
#' \dontrun{
#' jf_font("concourse")
#' p <- ggplot(df, aes(x, y)) + annotate_stat("rho = +0.50") + theme_journal("2col")
#' }
#' @export
jf_font <- function(font = NULL) {
  if (!is.null(font)) {
    .jf_env$font <- font
    return(invisible(font))
  }
  if (is.null(.jf_env$font)) jf()$default_font else .jf_env$font
}
