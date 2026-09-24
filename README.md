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
library(journalfig)   # attaches ggplot2 and patchwork too

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

## Two traps worth knowing

A `systemfonts` registered variant is an alias that `svglite` understands and
`cairo_pdf` does not, because cairo resolves through fontconfig. The SVG comes
out perfect and the PDF silently substitutes DejaVu Sans. `resolve_font()`
prefers a real installed family and warns when it cannot.

`on.exit()` at the top level of a sourced file fires as soon as THAT expression
finishes, not when the script ends, because `source()` evaluates each expression
in its own frame.

## Diagnostics

```r
list_fonts("concourse")   # exact family names this machine reports
font_report("concourse")  # are its digits tabular, and by which route
```
