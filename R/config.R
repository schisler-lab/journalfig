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
