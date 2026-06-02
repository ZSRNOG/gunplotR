# gunplotR

[![R-CMD-check](https://github.com/zsrnog/gunplotR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/zsrnog/gunplotR/actions/workflows/R-CMD-check.yaml)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![gnuplot](https://img.shields.io/badge/gnuplot-6.x-blue.svg)](http://www.gnuplot.info/)
[![R](https://img.shields.io/badge/R-%3E%3D%203.5.0-276DC3.svg)](https://www.r-project.org/)

`gunplotR` is an R interface to the external `gnuplot` plotting program. It
writes R data to temporary files, generates gnuplot scripts, and renders 2D,
3D, and 4D graphics with R-style arguments.

The package is designed for users who want the power of gnuplot without
leaving R:

- R-like aesthetics: `col`, `lwd`, `lty`, `pch`, `cex`, fonts, labels, and legends.
- Layered plotting with `gp_layer()` and `gp_multi()`.
- 3D and 4D wrappers for surfaces, pm3d maps, point clouds, vectors, polygons,
  voxels, isosurfaces, fence plots, waterfall plots, and boxes3d-style bars.
- Broad gnuplot 6 mapping through `gp_options()`, `gp_terminal()`,
  `gp_linetype()`, `gp_linestyle()`, and `gp_arrowstyle()`.
- Default behavior previews plots interactively and does not save files unless
  `output =` is supplied.

## Installation

Install from GitHub after publishing:

```r
install.packages("remotes")
remotes::install_github("zsrnog/gunplotR")
```

Or install the local source package:

```r
install.packages(
  "C:/Users/zsr/Documents/gunplotR/gunplotR_0.1.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

`gunplotR` requires an external gnuplot executable. On Windows:

```powershell
winget install -e --id gnuplot.gnuplot
```

If gnuplot is not on `PATH`, configure it in R:

```r
options(gunplotR.bin = "C:/Program Files/gnuplot/bin/gnuplot.exe")
```

## Quick Start

```r
library(gunplotR)

x <- seq(0, 2 * pi, length.out = 300)

gp_line(
  x,
  sin(x),
  col = 4,
  lwd = 2,
  main = "Sine wave",
  xlab = "x",
  ylab = "sin(x)"
)
```

Save a file only when requested:

```r
gp_line(x, sin(x), output = "sine.png", preview = FALSE)
```

## R-Style Access to Gnuplot 6

`gp_options()` maps common gnuplot `set` commands into R-friendly arguments.

```r
gp_line(
  1:100,
  (1:100)^2,
  settings = gp_options(
    xlog = TRUE,
    ylog = TRUE,
    xtics = c(1, 10, 100),
    yformat = "%.0e",
    key = "outside right",
    style_fill = "solid 0.4 border lc black"
  ),
  terminal = gp_terminal("pngcairo", width = 1200, height = 800,
                         font = "Arial", font_size = 12)
)
```

## Gallery

The gallery below was generated with
[`inst/examples/deep_gallery.R`](inst/examples/deep_gallery.R). The images are
saved in [`gallery/deep_potential`](gallery/deep_potential).

![pm3d landscape](gallery/deep_potential/04_pm3d_landscape_contours.png)

### Compact, R-Style Gnuplot Control

```r
gp_surface(
  surf_fun,
  type = "pm3d",
  settings = gp_options(
    view = c(60, 34, 0.88, 1.05),
    palette = "cubehelix start 0.35 cycles -1 saturation 0.9",
    contour = "base",
    colorbox = TRUE
  ),
  terminal = gp_terminal("pngcairo", width = 1400, height = 860)
)
```

| Layered 2D uncertainty | Polar signal plot |
| --- | --- |
| ![Layered uncertainty](gallery/deep_potential/01_layered_uncertainty_fit.png) | ![Polar signal](gallery/deep_potential/02_polar_signal_rosette.png) |

| Violin distribution | Waterfall ridgelines |
| --- | --- |
| ![Violin distribution](gallery/deep_potential/03_violin_distribution_lab.png) | ![Waterfall ridgelines](gallery/deep_potential/05_waterfall_ridgeline_scans.png) |

| Density ridgelines | Candlestick chart |
| --- | --- |
| ![Density ridgelines](gallery/deep_potential/07_density_ridgeline_bands.png) | ![Candlestick chart](gallery/deep_potential/08_finance_candlestick_ma.png) |

| 4D surface | 4D helix |
| --- | --- |
| ![4D surface](gallery/deep_potential/00_readme_hero_surface4d.png) | ![4D helix](gallery/deep_potential/06_points4d_signal_helix.png) |

| 3D zerrorfill ribbons | 3D boxes with pm3d lighting |
| --- | --- |
| ![3D zerrorfill ribbons](gallery/deep_potential/09_zerrorfill_ribbon_fences.png) | ![3D boxes with pm3d lighting](gallery/deep_potential/10_boxes3d_pm3d_lighting.png) |

## Main Functions

- `gp_plot()`, `gp_line()`, `gp_scatter()`: base 2D plotting.
- `gp_function()`: plot gnuplot expressions.
- `gp_style()`, `gp_layer()`, `gp_multi()`: layered graphics with gnuplot styles.
- `gp_options()`, `gp_terminal()`: gnuplot 6 settings using R-style arguments.
- `gp_surface()`, `gp_surface4d()`, `gp_map3d()`: 3D and 4D surfaces.
- `gp_points3d()`, `gp_points4d()`: 3D and 4D point clouds.
- `gp_bar3d()`, `gp_fenceplot()`, `gp_waterfall()`: specialized 3D demos.
- `gp_gantt()`, `gp_violinplot()`: complex 2D statistical and schedule plots.
- `gp_run()`: run raw gnuplot scripts when you need total control.

## Documentation

- [USAGE_GUIDE.md](USAGE_GUIDE.md): user guide in Chinese.
- [PLOT_AESTHETICS.md](PLOT_AESTHETICS.md): R-style aesthetics.
- [GNUPLOT_FEATURE_MAP.md](GNUPLOT_FEATURE_MAP.md): gnuplot 6 to R argument mapping.
- [THREE_D_FOUR_D_PLOTS.md](THREE_D_FOUR_D_PLOTS.md): 3D and 4D examples.
- [COMPLEX_PLOTS.md](COMPLEX_PLOTS.md): complex layered examples.

## Author

Zhou Shirong  
<zsrnog@yeah.net>

## License

MIT.
