this_file <- tryCatch(
  normalizePath(sys.frame(1)$ofile, winslash = "/", mustWork = TRUE),
  error = function(err) NA_character_
)
repo <- if (!is.na(this_file)) {
  normalizePath(file.path(dirname(this_file), "..", ".."),
                winslash = "/", mustWork = FALSE)
} else {
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}
if (!file.exists(file.path(repo, "DESCRIPTION"))) {
  repo <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

source_order <- c(
  "utils.R", "theme.R", "executable.R", "run.R", "render.R",
  "plots3d_advanced.R", "options.R", "styles.R", "plot.R", "plots2d.R",
  "plots3d.R", "demo_plots.R", "zzz.R"
)
for (file in file.path(repo, "R", source_order)) {
  source(file, chdir = TRUE)
}

bin <- "C:/Program Files/gnuplot/bin/gnuplot.exe"
if (file.exists(bin)) {
  options(gunplotR.bin = bin)
}

outdir <- file.path(repo, "gallery", "deep_potential")
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
unlink(file.path(outdir, "*.png"))

png_path <- function(name) file.path(outdir, paste0(name, ".png"))

record <- list()
rendered <- function(id, title, path) {
  info <- file.info(path)
  record[[length(record) + 1L]] <<- data.frame(
    id = id,
    title = title,
    file = normalizePath(path, winslash = "/", mustWork = FALSE),
    bytes = info$size,
    stringsAsFactors = FALSE
  )
}

term_wide <- gp_terminal("pngcairo", width = 1400, height = 860,
                         font = "Arial", font_size = 12,
                         background = "white")
term_square <- gp_terminal("pngcairo", width = 1080, height = 1080,
                           font = "Arial", font_size = 12,
                           background = "white")
term_card <- gp_terminal("pngcairo", width = 1160, height = 820,
                         font = "Arial", font_size = 12,
                         background = "white")

base_2d <- gp_options(
  border = "3 lw 1.25 lc rgb '#444444'",
  grid = "ytics lc rgb '#e6e6e6'",
  key = "outside right top",
  style_fill = "transparent solid 0.25 noborder"
)

set.seed(20260602)

# 1. README hero: 4D surface with contour base and clean composition.
x <- seq(-4.5, 4.5, length.out = 130)
y <- seq(-4.5, 4.5, length.out = 130)
z <- outer(y, x, function(y, x) {
  r <- sqrt(x^2 + y^2)
  cos(2.2 * r) * exp(-0.06 * r^2) + 0.18 * sin(2 * x) * cos(1.4 * y)
})
phase <- outer(y, x, function(y, x) atan2(y, x))
out <- png_path("00_readme_hero_surface4d")
gp_surface4d(
  z,
  phase,
  x = x,
  y = y,
  main = "gunplotR: 4D surface rendered by gnuplot",
  xlab = "x",
  ylab = "y",
  zlab = "height",
  cblabel = "phase",
  palette = "model HSV defined (0 0 1 1, 1 1 1 1)",
  colorbox = "vertical user origin .88,.24 size .025,.48",
  view = c(57, 34, 0.88, 1.0),
  pm3d = TRUE,
  extra = gp_options(
    contour = "base",
    xyplane = -0.9,
    hidden3d = FALSE,
    ticslevel = 0,
    border = "4095 lw 1 lc rgb '#444444'",
    grid = FALSE,
    raw = c("set cntrparam levels 12",
            "set format cb '%.1f'",
            "set lmargin 7",
            "set rmargin 12",
            "set tmargin 3",
            "set bmargin 4")
  ),
  terminal = term_wide,
  output = out,
  preview = FALSE
)
rendered("00", "README hero 4D surface", out)

# 2. Layered statistical story: uncertainty band + observations + smooth fit.
n <- 220
sx <- sort(runif(n, 0, 12))
sy <- 2.5 + 0.36 * sx + 1.2 * sin(1.15 * sx) +
  rnorm(n, sd = 0.45 + 0.04 * sx)
fit <- loess(sy ~ sx, span = 0.32)
xg <- seq(min(sx), max(sx), length.out = 320)
yg <- predict(fit, newdata = data.frame(sx = xg))
resid_sd <- sqrt(stats::filter((sy - predict(fit))^2, rep(1 / 25, 25),
                               sides = 2))
sigma <- approx(sx[!is.na(resid_sd)], resid_sd[!is.na(resid_sd)],
                xg, rule = 2)$y
band <- data.frame(x = xg, upper = yg + 1.6 * sigma, lower = yg - 1.6 * sigma)
line <- data.frame(x = xg, y = yg)
raw <- data.frame(x = sx, y = sy)
out <- png_path("01_layered_uncertainty_fit")
gp_multi(
  gp_layer(data = band, using = "1:2:3", style = "filledcurves",
           title = "uncertainty", fill = "transparent solid 0.28 noborder",
           fill_col = "#7fc7d9"),
  gp_layer(data = raw, using = "1:2", style = "points",
           title = "observations", pch = 7, cex = 0.55,
           col = "#2f4858"),
  gp_layer(data = line, using = "1:2", style = "lines",
           title = "loess fit", col = "#d62828", lwd = 2.8),
  main = "Layered uncertainty plot",
  subtitle = "points, transparent filledcurves, and fitted trend",
  xlab = "time",
  ylab = "response",
  settings = base_2d,
  terminal = term_card,
  output = out,
  preview = FALSE
)
rendered("01", "Layered uncertainty plot", out)

# 3. Polar curve with a quieter, centered composition.
out <- png_path("02_polar_signal_rosette")
gp_function(
  "1.05 + 0.32*cos(5*t) + 0.22*sin(8*t) + 0.12*cos(13*t)",
  type = "lines",
  legend = FALSE,
  col = "#5a189a",
  lwd = 2.6,
  main = "Polar signal rosette",
  settings = gp_options(
    polar = TRUE,
    angles = "degrees",
    trange = c(0, 360),
    samples = 1800,
    square = TRUE,
    grid = "polar lc rgb '#dddddd'",
    border = FALSE,
    xtics = FALSE,
    ytics = FALSE,
    raw = c("set rrange [0:1.7]",
            "set size ratio -1",
            "set lmargin at screen 0.06",
            "set rmargin at screen 0.94",
            "set tmargin at screen 0.93",
            "set bmargin at screen 0.06")
  ),
  terminal = term_square,
  output = out,
  preview = FALSE
)
rendered("02", "Polar signal rosette", out)

# 4. Violin plot from simulated grouped distributions.
groups <- rep(c("baseline", "model A", "model B", "model C"), each = 120)
values <- c(rnorm(120, 4.8, 0.55), rnorm(120, 5.4, 0.48),
            c(rnorm(80, 5.9, 0.35), rnorm(40, 6.7, 0.25)),
            rnorm(120, 6.2, 0.72))
scores <- data.frame(group = groups, score = values)
out <- png_path("03_violin_distribution_lab")
gp_violinplot(
  scores,
  group = "group",
  value = "score",
  points = TRUE,
  boxplot = TRUE,
  fill = 0.5,
  col = "#2a9d8f",
  seed = 42,
  main = "Distribution lab",
  subtitle = "violin density with jittered observations and boxplot overlay",
  xlab = "experiment",
  ylab = "score",
  extra = gp_options(grid = "ytics lc rgb '#e6e6e6'",
                     border = "2 lw 1.2 lc rgb '#444444'"),
  terminal = term_card,
  output = out,
  preview = FALSE
)
rendered("03", "Distribution lab violin plot", out)

# 5. 3D pm3d surface: fewer empty margins, stronger shape.
surf_fun <- function(x, y) {
  r1 <- sqrt((x + 1.3)^2 + (y - 0.8)^2)
  r2 <- sqrt((x - 1.1)^2 + (y + 1.2)^2)
  1.4 * exp(-0.22 * r1^2) - 1.15 * exp(-0.18 * r2^2) +
    0.18 * sin(2.5 * x) * cos(1.7 * y)
}
out <- png_path("04_pm3d_landscape_contours")
gp_surface(
  surf_fun,
  x = seq(-5.5, 5.5, length.out = 120),
  y = seq(-5.5, 5.5, length.out = 120),
  type = "pm3d",
  main = "pm3d landscape with contour base",
  xlab = "x",
  ylab = "y",
  zlab = "height",
  settings = gp_options(
    view = c(60, 34, 0.88, 1.05),
    palette = "cubehelix start 0.35 cycles -1 saturation 0.9",
    colorbox = "vertical user origin .88,.27 size .025,.44",
    contour = "base",
    xyplane = -1.2,
    samples = 120,
    isosamples = 120,
    grid = FALSE,
    raw = c("set cntrparam levels 14",
            "set lmargin 7",
            "set rmargin 12",
            "set tmargin 3",
            "set bmargin 4")
  ),
  terminal = term_wide,
  output = out,
  preview = FALSE
)
rendered("04", "pm3d landscape with contour base", out)

# 6. Waterfall: ridgelines in 3D.
xw <- seq(0, 1, length.out = 150)
yw <- seq(1, 42)
zw <- outer(yw, xw, function(y, x) {
  center <- 0.16 + y / 110 + 0.025 * sin(y / 3)
  stats::dnorm(x, center, 0.035 + y / 2200) *
    (1 + 0.15 * cos(y / 5))
})
out <- png_path("05_waterfall_ridgeline_scans")
gp_waterfall(
  zw,
  x = xw,
  y = yw,
  fill = "background",
  col = "#212529",
  main = "Waterfall ridgeline scans",
  xlab = "position",
  ylab = "scan",
  zlab = "density",
  view = c(45, 28, 1, 1.16),
  xyplane = 0,
  extra = gp_options(grid = FALSE,
                     raw = c("set border 4095 lw 1 lc rgb '#555555'",
                             "set tics out")),
  terminal = term_card,
  output = out,
  preview = FALSE
)
rendered("05", "Waterfall ridgeline scans", out)

# 7. 4D point cloud: clean helix with palette color.
theta <- seq(0, 10 * pi, length.out = 950)
radius <- 1 + 0.22 * sin(4 * theta)
out <- png_path("06_points4d_signal_helix")
gp_points4d(
  radius * cos(theta),
  radius * sin(theta),
  theta / 4,
  sin(theta) + 0.5 * cos(3 * theta),
  pch = 7,
  cex = 0.68,
  main = "4D signal helix",
  xlab = "x",
  ylab = "y",
  zlab = "time",
  cblabel = "signal",
  palette = "rgbformulae 33,13,10",
  colorbox = "vertical user origin .88,.28 size .025,.42",
  view = c(63, 34, 1, 1.05),
  extra = gp_options(grid = FALSE,
                     raw = c("set border 4095 lw 1 lc rgb '#555555'",
                             "set lmargin at screen 0.05",
                             "set rmargin at screen 0.86")),
  terminal = term_card,
  output = out,
  preview = FALSE
)
rendered("06", "4D signal helix", out)

# 8. Advanced 2D: density ridgelines with transparent filledcurves.
ridge_groups <- c("control", "low dose", "mid dose", "high dose", "recovery")
xr <- seq(-3.5, 4.7, length.out = 360)
ridge_rows <- list()
for (i in seq_along(ridge_groups)) {
  shift <- -0.9 + 0.42 * i
  density <- 0.95 * dnorm(xr, shift, 0.48 + 0.035 * i) +
    0.42 * dnorm(xr, shift + 1.05, 0.28 + 0.02 * i)
  density <- density / max(density) * 0.78
  ridge_rows[[i]] <- data.frame(x = xr, base = i, top = i + density,
                                group = i)
}
ridge <- do.call(rbind, ridge_rows)
ridge_file <- write_plot_table(ridge, workdir = tempdir(),
                               prefix = "gunplotR-ridge-")
ridge_cols <- c("#264653", "#2a9d8f", "#e9c46a", "#f4a261", "#e76f51")
ridge_terms <- character()
for (i in seq_along(ridge_groups)) {
  select <- paste0("(($4==", i, ")?$1:1/0)")
  ridge_terms <- c(
    ridge_terms,
    paste(gp_quote(ridge_file), "using", paste0(select, ":2:3"),
          "with filledcurves fc rgb", gp_quote(ridge_cols[i]),
          "fs transparent solid 0.46 noborder notitle"),
    paste(gp_quote(ridge_file), "using", paste0(select, ":3"),
          "with lines lc rgb", gp_quote(ridge_cols[i]),
          "lw 2.2 title", gp_quote(ridge_groups[i]))
  )
}
out <- png_path("07_density_ridgeline_bands")
gp_render(
  plot_body = paste("plot", paste(ridge_terms, collapse = ", \\\n     ")),
  setup = c(
    gp_setup(main = "Density ridgelines",
             subtitle = "transparent filledcurves with group-specific peaks",
             xlab = "standardized measurement", ylab = NULL,
             grid = FALSE, key = TRUE,
             xrange = c(min(xr), max(xr)),
             yrange = c(0.65, length(ridge_groups) + 1.05),
             theme = gp_theme(legend_position = "outside right top",
                              legend_box = FALSE)),
    paste0("set ytics (",
           paste(paste(gp_quote(ridge_groups), seq_along(ridge_groups)),
                 collapse = ", "), ")"),
    "set border 3 lw 1.25 lc rgb '#444444'",
    "set tics out nomirror",
    "set grid xtics lc rgb '#e9ecef'",
    "set style fill transparent solid 0.46 noborder"
  ),
  terminal = term_card,
  output = out,
  preview = FALSE
)
unlink(ridge_file)
rendered("07", "Density ridgeline bands", out)

# 9. Advanced 2D: candlesticks with split colors, moving average, and band.
days <- seq_len(80)
close <- 102 + cumsum(rnorm(length(days), 0.08, 1.15)) +
  4 * sin(days / 9)
open <- c(close[1] - 0.8, close[-length(close)] +
            rnorm(length(days) - 1L, 0, 0.75))
high <- pmax(open, close) + runif(length(days), 0.35, 1.8)
low <- pmin(open, close) - runif(length(days), 0.35, 1.8)
ma <- as.numeric(stats::filter(close, rep(1 / 10, 10), sides = 1))
ma <- approx(days[!is.na(ma)], ma[!is.na(ma)], days, rule = 2)$y
band <- data.frame(day = days, upper = ma + 2.4, lower = ma - 2.4)
ohlc_up <- data.frame(day = days, open = ifelse(close >= open, open, NA),
                      low = ifelse(close >= open, low, NA),
                      high = ifelse(close >= open, high, NA),
                      close = ifelse(close >= open, close, NA))
ohlc_down <- data.frame(day = days, open = ifelse(close < open, open, NA),
                        low = ifelse(close < open, low, NA),
                        high = ifelse(close < open, high, NA),
                        close = ifelse(close < open, close, NA))
ma_data <- data.frame(day = days, ma = ma)
out <- png_path("08_finance_candlestick_ma")
gp_multi(
  gp_layer(data = band, using = "1:2:3", style = "filledcurves",
           title = "MA band", fill = "transparent solid 0.2 noborder",
           fill_col = "#8ecae6"),
  gp_layer(data = ohlc_up, using = "1:2:3:4:5", style = "candlesticks",
           title = "up day", col = "#2a9d8f",
           fill = "solid 0.65 border"),
  gp_layer(data = ohlc_down, using = "1:2:3:4:5", style = "candlesticks",
           title = "down day", col = "#e76f51",
           fill = "solid 0.65 border"),
  gp_layer(data = ma_data, using = "1:2", style = "lines",
           title = "MA(10)", col = "#1d3557", lwd = 2.6),
  main = "Candlestick chart",
  subtitle = "finance style, split colors, moving average, and band",
  xlab = "trading day",
  ylab = "price",
  settings = gp_options(
    border = "3 lw 1.25 lc rgb '#444444'",
    grid = "ytics lc rgb '#e6e6e6'",
    key = "outside right top",
    boxwidth = 0.62,
    raw = c("set tics out nomirror",
            "set style fill solid 0.65 border")
  ),
  terminal = term_card,
  output = out,
  preview = FALSE
)
rendered("08", "Candlestick chart with moving average", out)

# 10. 3D zerrorfill: transparent ribbons on a log-scaled z axis.
xze <- seq(1, 620, length.out = 150)
zerr_rows <- list()
for (k in 1:5) {
  center <- 930 * (0.42 + 0.11 * k) * exp(-xze / (120 + 26 * k)) *
    (1 + 0.14 * cos(xze / 38 + k / 2)) + 2.5 * k
  spread <- center * (0.13 + 0.025 * k) *
    (1 + 0.25 * sin(xze / 55 + k))
  zerr_rows[[k]] <- data.frame(
    x = xze,
    y = k,
    z = center,
    zlow = pmax(1, center - abs(spread)),
    zhigh = center + abs(spread),
    group = k
  )
}
zerr <- do.call(rbind, zerr_rows)
zerr_cols <- c("#1b9aaa", "#5a4fcf", "#d81b60", "#f2a900", "#2d6a4f")
zerr_files <- character()
zerr_terms <- vapply(seq_along(zerr_cols), function(i) {
  chunk <- zerr[zerr$group == i, c("x", "y", "z", "zlow", "zhigh")]
  file <- write_plot_table(chunk, workdir = tempdir(),
                           prefix = paste0("gunplotR-zerrorfill-", i, "-"))
  zerr_files <<- c(zerr_files, file)
  paste(gp_quote(file), "using 1:2:3:4:5 title", gp_quote(paste("k =", i)),
        "with zerrorfill lc rgb", gp_quote(zerr_cols[i]),
        "fs transparent solid 0.34")
}, character(1))
out <- png_path("09_zerrorfill_ribbon_fences")
gp_render(
  plot_body = paste("splot", paste(zerr_terms, collapse = ", \\\n     ")),
  setup = gp_3d_setup(
    main = "3D zerrorfill ribbons",
    subtitle = "log-scaled z axis with depth-sorted transparent fills",
    xlab = "x", ylab = "series", zlab = "value",
    xrange = c(0, 640),
    yrange = c(0.6, 5.6),
    zrange = c(1, 1200),
    view = c(68, 28, 1, 1.08),
    grid = FALSE,
    key = TRUE,
    pm3d = "depthorder",
    extra = gp_options(
      zlog = TRUE,
      zformat = "10^{%T}",
      ytics = setNames(seq_len(5), paste0("k", 1:5)),
      border = "4095 lw 1 lc rgb '#555555'",
      colorbox = FALSE,
      raw = c("set key inside top right box opaque",
              "set tics out nomirror",
              "set xyplane at 1")
    )
  ),
  terminal = term_card,
  output = out,
  preview = FALSE
)
unlink(zerr_files)
rendered("09", "3D zerrorfill ribbon fences", out)

# 11. 3D boxes: pm3d palette, depth sorting, walls, and lighting.
xb <- seq(0.5, 8.5, length.out = 9)
yb <- seq(0.5, 6.5, length.out = 7)
zbox <- outer(yb, xb, function(y, x) {
  4.6 + 2.9 * exp(-((x - 5.2)^2 + (y - 3.5)^2) / 7) +
    1.6 * sin(1.1 * x) * cos(0.8 * y) + 0.28 * x + 0.18 * y
})
zbox[zbox < 0.4] <- 0.4
out <- png_path("10_boxes3d_pm3d_lighting")
gp_bar3d(
  zbox,
  x = xb,
  y = yb,
  color = zbox,
  color_mode = "palette",
  fill = "solid 0.95 border lc rgb '#222222'",
  width = 0.58,
  depth = 0.5,
  xyplane = 0,
  depthorder = TRUE,
  lighting = TRUE,
  walls = TRUE,
  pm3d_border = "lc rgb '#222222'",
  palette = "viridis",
  cblabel = "height",
  colorbox = "vertical user origin .88,.27 size .025,.46",
  main = "3D boxes with pm3d lighting",
  subtitle = "numeric grid rendered with boxes3d-style depth sorting",
  xlab = "x",
  ylab = "y",
  zlab = "height",
  view = c(64, 34, 1, 1.08),
  extra = gp_options(grid = FALSE,
                     raw = c("set tics out nomirror",
                             "set border 4095 lw 1 lc rgb '#555555'",
                             "set xrange [0:9]",
                             "set yrange [0:7]",
                             "set zrange [0:12]",
                             "set lmargin 7",
                             "set rmargin 12")),
  terminal = term_card,
  output = out,
  preview = FALSE
)
rendered("10", "3D boxes with pm3d lighting", out)

manifest <- do.call(rbind, record)
utils::write.csv(manifest, file.path(outdir, "_manifest.csv"), row.names = FALSE)
cat("Saved", nrow(manifest), "plots to",
    normalizePath(outdir, winslash = "/"), "\n")
print(manifest)
