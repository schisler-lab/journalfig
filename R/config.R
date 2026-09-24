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

#' Point journalfig at a project override file.
#'
#' Call with no argument to reload. Looks for `journal_fig.json` beside the
#' working directory by default, so a project that wants overrides needs no
#' setup and one that does not needs no file.
#'
#' @param path override JSON, or NULL to search for one.
#' @return the merged configuration, invisibly.
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

#' The active configuration. Loads on first use.
jf <- function() {
  if (is.null(.jf_env$cfg)) jf_config()
  .jf_env$cfg
}

#' The font this figure is using.
#'
#' Font is a FIGURE-level decision, but every call site used to default to the
#' configuration default independently, so a figure built in Concourse still
#' drew its axes, ticks and in-panel statistics in the safe face.
#'
#' CALL THIS FIRST, before building any layer. It is deliberately NOT a side
#' effect of theme_journal(), because R evaluates a default argument lazily at
#' the point of use: annotate_stat() called before theme_journal() in a `+`
#' chain would resolve its font before the theme ever ran, and the figure would
#' come out mixed depending on the order somebody happened to type. An explicit
#' call at the top of the script has no such ordering trap.
#'
#'   jf_font("concourse")
#'   p <- ggplot(...) + annotate_stat(...) + theme_journal("2col")
#'
#' @param font a key or family name to set, or nothing to read.
jf_font <- function(font = NULL) {
  if (!is.null(font)) {
    .jf_env$font <- font
    return(invisible(font))
  }
  if (is.null(.jf_env$font)) jf()$default_font else .jf_env$font
}
