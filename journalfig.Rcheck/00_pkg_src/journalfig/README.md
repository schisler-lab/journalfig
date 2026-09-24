# journalfig

Manuscript figures built at final print size with live text, so Illustrator can
assemble and annotate them without anything being re-rendered.

```
code -> live-text SVG -> Illustrator assemble -> outline -> submit
```

## Install

```r
remotes::install_github("schisler-lab/journalfig")
```

Once per machine, and again after an update. Open Sans must be installed on the
machine (`brew install --cask font-open-sans` on macOS); the package refuses to
render in a substitute face rather than quietly producing a Helvetica figure you
notice at proof.

## Use

```r
library(journalfig)                       # attaches ggplot2 and patchwork too
jf_config("journal_fig.json")             # this project's font registry
jf_font("concourse")                      # FIRST, before any plot is built

p <- ggplot(df, aes(x, y)) +
  geom_line(linewidth = pt_to_mm(jf()$stroke_pt$data)) +
  labs(x = "Time (h)", y = "Value (a.u.)") +
  theme_journal("1col", ticks = "both") +
  annotate_stat("rho = +0.50, p < 0.001")

save_journal(p, "fig1", width = "1col", height_mm = 60, fmt = c("svg", "pdf"))
```

## What it enforces

`save_journal()` reopens what it just wrote and fails if anything is off:
live text rather than outlines, the font you asked for, nothing under the type
floor, nothing under the stroke floor, a standard column width, and no
substituted font in the PDF. Every one of those rules existed in prose first and
was broken anyway, which is why they are checked rather than documented.

Figures still holding a `panel_placeholder()` warn on every export, so a figure
with art not yet dropped in cannot quietly become the submitted version.

## Units

ggplot is inconsistent and it catches everyone. `element_text` size is POINTS;
`element_line` linewidth, `geom_*` linewidth and `geom_text` size are all
MILLIMETRES. Use `pt_to_mm()` for anything that draws a line and `pt_to_size()`
for anything that draws text outside the theme.

## Configuration

Geometry and type are lab-wide and ship with the package. The font registry is
per-machine, because licensed families are installed on some machines and not
others, so only the collaborator-safe face ships.

A project overrides by putting `journal_fig.json` at its root. It is merged over
the defaults, so name only what differs:

```json
{
  "fonts":    { "concourse": "Concourse 3", "mono": "Triplicate A Code" },
  "bold_for": { "Concourse 3": "Concourse 6" }
}
```

`bold_for` exists because some families carry no internal bold and use a
separate weight as their bold. Without it ggplot synthesizes a smeared fake one.

## Choosing the font

The face is a decision about a FIGURE, not about a call, so it is set once:

```r
jf_font("concourse")   # a key in the registry, or a family name outright
jf_font()              # what is active now
```

Call it before anything that draws. `theme_journal()`, `annotate_stat()`,
`panel_tag()`, `panel_placeholder()`, `save_journal()` and `check_journal()` all
default to `jf_font()`, and R evaluates a default argument lazily, at the moment
the function first needs it. In `p + annotate_stat(...) + theme_journal(...)` the
annotation resolves its font before the theme is ever evaluated, so anything that
sets the font as a side effect of building a layer arrives too late for the
layers next to it. Setting it up front is the only ordering that holds.

Skipping it is not loud. Every call site independently falls back to the safe
face, so a figure comes out mostly Open Sans with a few strings in the family you
thought you had asked for. `check_journal()` and `check_pdf_fonts()` catch it
after the fact; `jf_font()` prevents it.

## Three traps worth knowing

A `systemfonts` registered variant is an alias that `svglite` understands and
`cairo_pdf` does not, because cairo resolves through fontconfig. The SVG comes
out perfect and the PDF silently substitutes DejaVu Sans. `resolve_font()`
prefers a real installed family and warns when it cannot.

`on.exit()` at the top level of a sourced file fires as soon as THAT expression
finishes, not when the script ends, because `source()` evaluates each expression
in its own frame.

R evaluates default arguments lazily, in the frame of the call, at first use. A
function cannot set state that its siblings in a `+` chain will read, because
they may already have resolved theirs. See `jf_font()` above.

## Diagnostics

```r
list_fonts("concourse")   # exact family names this machine reports
font_report("concourse")  # are its digits tabular, and by which route
check_journal("fig1.svg") # live text, font, type and stroke floors, width
check_pdf_fonts("fig1.pdf")  # every family actually embedded
```

`check_pdf_fonts()` scans the raw bytes. A PDF is binary, so anything that turns
it into a character vector first silently drops most of it and reports a clean
subset of the truth.
