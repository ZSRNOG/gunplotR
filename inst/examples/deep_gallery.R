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

png_path <- function(name) {
  file.path(outdir, paste0(name, ".png"))
}

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

set.seed(20260602)

# 1. Layered scatter, confidence band, and fit line.
n <- 180
x <- runif(n, 0, 10)
y <- 1.8 + 0.55 * x + sin(x * 1.4) + rnorm(n, sd = 0.55)
fit <- lm(y ~ poly(x, 3, raw = TRUE))
xg <- seq(min(x), max(x), length.out = 260)
pred <- predict(fit, newdata = data.frame(x = xg), interval = "confidence")
band <- data.frame(x = xg, upper = pred[, "upr"], lower = pred[, "lwr"])
line <- data.frame(x = xg, y = pred[, "fit"])
raw <- data.frame(x = x, y = y)
out <- png_path("01_layered_regression_band")
gp_multi(
  gp_layer(data = band, using = "1:2:3", style = "filledcurves",
           title = "95% CI", fill = 0.24, fill_col = "#8ecae6",
           border = FALSE),
  gp_layer(data = raw, using = "1:2", style = "points", title = "samples",
           pch = 7, cex = 0.65, col = "#4a4a4a"),
  gp_layer(data = line, using = "1:2", style = "lines", title = "cubic fit",
           col = "#d62828", lwd = 2.4),
  main = "Layered regression with confidence band",
  subtitle = "points + filledcurves + fitted line",
  xlab = "x",
  ylab = "response",
  settings = gp_options(key = "outside right", grid = "xtics ytics",
                        border = "3 lw 1.2"),
  terminal = gp_terminal("pngcairo", width = 1200, height = 800,
                         font = "Arial", font_size = 11),
  output = out,
  preview = FALSE
)
rendered("01", "Layered regression with confidence band", out)

# 2. Polar rose curve.
out <- png_path("02_polar_rose_curve")
gp_function(
  "1 + 0.42*cos(7*t) + 0.18*sin(13*t)",
  type = "lines",
  legend = FALSE,
  col = "#7b2cbf",
  lwd = 2.2,
  main = "Polar harmonic rose",
  settings = gp_options(
    polar = TRUE,
    angles = "degrees",
    trange = c(0, 360),
    samples = 1440,
    square = TRUE,
    grid = "polar",
    border = FALSE,
    xtics = FALSE,
    ytics = FALSE
  ),
  terminal = gp_terminal("pngcairo", width = 1000, height = 1000,
                         font = "Arial", font_size = 11),
  output = out,
  preview = FALSE
)
rendered("02", "Polar harmonic rose", out)

# 3. Violin plot with jittered points and boxplots.
out <- png_path("03_violin_box_jitter")
gp_violinplot(
  iris,
  group = "Species",
  value = "Sepal.Length",
  points = TRUE,
  boxplot = TRUE,
  fill = 0.45,
  col = "#2a9d8f",
  main = "Violin plot with observations",
  subtitle = "density + jitter + boxplot",
  xlab = "Species",
  ylab = "Sepal length",
  extra = gp_options(grid = "ytics", border = "2 lw 1.2"),
  terminal = gp_terminal("pngcairo", width = 1100, height = 750,
                         font = "Arial", font_size = 11),
  output = out,
  preview = FALSE
)
rendered("03", "Violin plot with observations", out)

# 4. Gantt chart with time axis.
tasks <- data.frame(
  task = c("Spec", "Data model", "2D API", "3D API", "Docs",
           "Gallery", "Release"),
  start = as.Date(c("2026-01-02", "2026-01-12", "2026-02-01",
                    "2026-02-18", "2026-03-10", "2026-03-24",
                    "2026-04-15")),
  end = as.Date(c("2026-01-20", "2026-02-08", "2026-02-25",
                  "2026-03-18", "2026-04-05", "2026-04-18",
                  "2026-04-30"))
)
out <- png_path("04_gantt_project_plan")
gp_gantt(
  tasks,
  task = "task",
  start = "start",
  end = "end",
  main = "gunplotR project plan",
  xlab = "Date",
  grid = TRUE,
  extra = gp_options(grid = "xtics", border = "3 lw 1.2"),
  terminal = gp_terminal("pngcairo", width = 1200, height = 720,
                         font = "Arial", font_size = 11),
  output = out,
  preview = FALSE
)
rendered("04", "Gantt project plan", out)

# 5. 3D pm3d surface with contour base.
surf_fun <- function(x, y) {
  r <- sqrt(x^2 + y^2)
  cos(r) * exp(-0.04 * (x^2 + y^2)) + 0.18 * sin(2 * x)
}
out <- png_path("05_pm3d_surface_contour")
gp_surface(
  surf_fun,
  x = seq(-8, 8, length.out = 90),
  y = seq(-8, 8, length.out = 90),
  type = "pm3d",
  main = "pm3d surface with contour base",
  xlab = "x",
  ylab = "y",
  zlab = "z",
  settings = gp_options(view = c(58, 32), palette = "viridis",
                        contour = "base", colorbox = TRUE,
                        xyplane = 0, samples = 90, isosamples = 90),
  terminal = gp_terminal("pngcairo", width = 1200, height = 850,
                         font = "Arial", font_size = 11),
  output = out,
  preview = FALSE
)
rendered("05", "pm3d surface with contour base", out)

# 6. 4D surface: z plus palette color.
x4 <- seq(-3.5, 3.5, length.out = 90)
y4 <- seq(-3.5, 3.5, length.out = 90)
z4 <- outer(y4, x4, function(y, x) sin(x * y) / (1 + 0.12 * (x^2 + y^2)))
c4 <- outer(y4, x4, function(y, x) atan2(y, x))
out <- png_path("06_surface4d_phase_color")
gp_surface4d(
  z4,
  c4,
  x = x4,
  y = y4,
  main = "4D surface: height + phase color",
  xlab = "x",
  ylab = "y",
  zlab = "z",
  cblabel = "phase",
  palette = "model HSV defined (0 0 1 1, 1 1 1 1)",
  colorbox = TRUE,
  view = c(57, 36),
  pm3d = TRUE,
  extra = gp_options(xyplane = 0, hidden3d = FALSE),
  terminal = gp_terminal("pngcairo", width = 1200, height = 850,
                         font = "Arial", font_size = 11),
  output = out,
  preview = FALSE
)
rendered("06", "4D surface with phase color", out)

# 7. Fence plot.
xf <- seq(-7, 7, length.out = 80)
yf <- seq(-5, 5, length.out = 13)
zf <- outer(yf, xf, function(y, x) {
  r <- sqrt((x / 1.2)^2 + y^2)
  ifelse(r == 0, 1, sin(r) / r) + 0.08 * cos(2 * y)
})
out <- png_path("07_fenceplot_zerrorfill")
gp_fenceplot(
  zf,
  x = xf,
  y = yf,
  fill = 0.58,
  col = "#1d4e89",
  main = "Fence plot with zerrorfill",
  xlab = "x",
  ylab = "scan",
  zlab = "height",
  view = c(70, 25),
  depthorder = TRUE,
  terminal = gp_terminal("pngcairo", width = 1200, height = 800,
                         font = "Arial", font_size = 11),
  output = out,
  preview = FALSE
)
rendered("07", "Fence plot with zerrorfill", out)

# 8. Waterfall plot.
xw <- seq(0, 1, length.out = 120)
yw <- seq(1, 48)
zw <- outer(yw, xw, function(y, x) {
  stats::dnorm(x, 0.16 + y / 175, 0.045 + y / 1800) *
    (1 + 0.18 * sin(y / 4))
})
out <- png_path("08_waterfall_density_scans")
gp_waterfall(
  zw,
  x = xw,
  y = yw,
  fill = "background",
  col = "#1f1f1f",
  main = "Waterfall density scans",
  xlab = "position",
  ylab = "scan",
  zlab = "density",
  view = c(42, 27, 1, 1.18),
  xyplane = 0,
  terminal = gp_terminal("pngcairo", width = 1200, height = 850,
                         font = "Arial", font_size = 11),
  output = out,
  preview = FALSE
)
rendered("08", "Waterfall density scans", out)

# 9. 4D helix point cloud.
theta <- seq(0, 12 * pi, length.out = 1200)
radius <- 1 + 0.28 * sin(5 * theta)
out <- png_path("09_points4d_twisted_helix")
gp_points4d(
  radius * cos(theta),
  radius * sin(theta),
  theta / 5,
  sin(theta) + cos(3 * theta),
  pch = 7,
  cex = 0.75,
  main = "4D twisted helix",
  xlab = "x",
  ylab = "y",
  zlab = "t",
  cblabel = "signal",
  palette = "rgbformulae 33,13,10",
  view = c(62, 34),
  terminal = gp_terminal("pngcairo", width = 1100, height = 850,
                         font = "Arial", font_size = 11),
  output = out,
  preview = FALSE
)
rendered("09", "4D twisted helix", out)

# 10. 3D bars using boxes3d-style controls.
zbar <- outer(1:6, 1:5, function(y, x) round(2 + y * x + 3 * sin(x + y), 1))
rownames(zbar) <- paste0("R", 1:nrow(zbar))
colnames(zbar) <- paste0("C", 1:ncol(zbar))
bar_cols <- matrix(rep(c("tomato", "gold", "forestgreen", "royalblue",
                         "orchid"), each = nrow(zbar)), nrow = nrow(zbar))
out <- png_path("10_boxes3d_colored_bars")
gp_bar3d(
  zbar,
  color = bar_cols,
  color_mode = "rgb_variable",
  fill = "solid 0.82 border lc black",
  depth = 0.62,
  depthorder = TRUE,
  lighting = TRUE,
  walls = "x0 y0 z0",
  pm3d_border = "lc black",
  main = "boxes3d-style colored bars",
  xlab = "column",
  ylab = "row",
  zlab = "value",
  view = c(62, 36),
  terminal = gp_terminal("pngcairo", width = 1200, height = 850,
                         font = "Arial", font_size = 11),
  output = out,
  preview = FALSE
)
rendered("10", "boxes3d-style colored bars", out)

manifest <- do.call(rbind, record)
utils::write.csv(manifest, file.path(outdir, "_manifest.csv"), row.names = FALSE)
cat("Saved", nrow(manifest), "plots to", normalizePath(outdir, winslash = "/"), "\n")
print(manifest)
