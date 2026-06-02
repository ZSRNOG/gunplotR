#' 3D Surface Plot
#'
#' @param z Numeric matrix, data frame with x/y/z columns, R function
#'   `function(x, y)`, or a character Gnuplot expression such as
#'   `"sin(sqrt(x**2+y**2))"`.
#' @param x Optional x grid values for matrix/function input.
#' @param y Optional y grid values for matrix/function input.
#' @param n Grid size used when `z` is a function and `x` or `y` is omitted.
#' @param type Surface style.
#' @param hidden3d If `TRUE`, enable hidden line removal for line surfaces.
#' @param legend Legend label. Use `FALSE` for no legend.
#' @param xrange Optional x-axis range.
#' @param yrange Optional y-axis range.
#' @param zrange Optional z-axis range.
#' @param zlab Optional z-axis label.
#' @param palette Optional Gnuplot palette specification.
#' @param settings Optional character vector returned by `gp_options()`.
#' @inheritParams gp_plot
#' @inheritParams gp_3d_setup
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' f <- function(x, y) sin(sqrt(x^2 + y^2))
#' gp_surface(f, type = "lines", hidden3d = TRUE,
#'            main = "Hidden-line surface", view = c(60, 35))
#' gp_surface("sin(sqrt(x**2+y**2))", type = "pm3d",
#'            xrange = c(-10, 10), yrange = c(-10, 10),
#'            samples = 80, isosamples = 80, cblabel = "z")
#' }
gp_surface <- function(z,
                       x = NULL,
                       y = NULL,
                       n = 60,
                       type = c("surface", "pm3d", "lines", "points"),
                       hidden3d = TRUE,
                       legend = NULL,
                       xrange = NULL,
                       yrange = NULL,
                       zrange = NULL,
                       main = NULL,
                       subtitle = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       zlab = NULL,
                       grid = TRUE,
                       col = NULL,
                       color = NULL,
                       line_col = NULL,
                       point_col = NULL,
                       lwd = NULL,
                       lty = NULL,
                       pch = NULL,
                       cex = NULL,
                       palette = NULL,
                       cbrange = NULL,
                       cblabel = NULL,
                       colorbox = NULL,
                       view = NULL,
                       view_map = FALSE,
                       samples = NULL,
                       isosamples = NULL,
                       ticslevel = NULL,
                       xyplane = NULL,
                       pm3d = NULL,
                       pm3d_at = NULL,
                       surface = NULL,
                       contour = NULL,
                       dgrid3d = NULL,
                       theme = NULL,
                       settings = NULL,
                       extra = NULL,
                       output = NULL,
                       terminal = NULL,
                       preview = TRUE,
                       preview_terminal = NULL,
                       path = NULL,
                       workdir = tempdir(),
                       echo = FALSE,
                       cleanup = TRUE) {
  type <- match.arg(type)
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  style_name <- if (type == "surface") "lines" else type
  style <- gp_style(style_name, title = legend, col = col, color = color,
                    line_col = line_col, point_col = point_col, lwd = lwd,
                    lty = lty, pch = pch, cex = cex)

  if (is.character(z)) {
    if (length(z) != 1L || is.na(z) || !nzchar(z)) {
      stop("Character `z` must be a single Gnuplot expression.",
           call. = FALSE)
    }
    style$title <- if (is.null(legend)) z else legend
    body <- paste("splot", z, "with", style_clause(style),
                  title_clause(style$title))
  } else {
    data <- if (is.function(z)) {
      function_to_xyz(z, x = x, y = y, n = n)
    } else {
      matrix_to_xyz(z, x = x, y = y)
    }
    data_file <- write_grid_table(data, workdir = workdir)
    if (isTRUE(cleanup)) {
      on.exit(unlink(data_file), add = TRUE)
    }
    body <- paste("splot", gp_quote(data_file), "using 1:2:3 with",
                  style_clause(style), title_clause(style$title))
  }

  gp_render(
    plot_body = body,
    setup = c(
      gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab, ylab = ylab,
                  zlab = zlab, grid = grid, xrange = xrange, yrange = yrange,
                  zrange = zrange, palette = palette, cbrange = cbrange,
                  cblabel = cblabel, colorbox = colorbox, view = view,
                  view_map = view_map, samples = samples,
                  isosamples = isosamples, ticslevel = ticslevel,
                  xyplane = xyplane,
                  pm3d = if (type == "pm3d" && is.null(pm3d)) TRUE else pm3d,
                  pm3d_at = pm3d_at,
                  hidden3d = if (type == "pm3d" && isTRUE(hidden3d)) {
                    NULL
                  } else {
                    hidden3d
                  },
                  surface = surface, contour = contour, dgrid3d = dgrid3d,
                  theme = theme, settings = settings, extra = extra)
    ),
    output = output,
    terminal = terminal,
    preview = preview,
    preview_terminal = preview_terminal,
    path = path,
    workdir = workdir,
    echo = echo,
    cleanup = cleanup
  )
}

#' 3D Points Plot
#'
#' @param x Numeric x values or a data frame with x/y/z columns.
#' @param y Optional numeric y values.
#' @param z Optional numeric z values.
#' @param type Gnuplot 3D plot style.
#' @param legend Legend label. Use `FALSE` for no legend.
#' @inheritParams gp_surface
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' theta <- seq(0, 8 * pi, length.out = 400)
#' gp_points3d(cos(theta), sin(theta), theta, type = "lines",
#'             col = 2, lwd = 2, main = "3D spiral",
#'             view = c(65, 30))
#' }
gp_points3d <- function(x,
                        y = NULL,
                        z = NULL,
                        type = c("points", "lines", "linespoints",
                                 "impulses"),
                        legend = NULL,
                        xrange = NULL,
                        yrange = NULL,
                        zrange = NULL,
                        main = NULL,
                        subtitle = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        zlab = NULL,
                        grid = TRUE,
                        col = NULL,
                        color = NULL,
                        line_col = NULL,
                        point_col = NULL,
                        lwd = NULL,
                        lty = NULL,
                        pch = NULL,
                        cex = NULL,
                        palette = NULL,
                        cbrange = NULL,
                        cblabel = NULL,
                        colorbox = NULL,
                        view = NULL,
                        view_map = FALSE,
                        samples = NULL,
                        isosamples = NULL,
                        ticslevel = NULL,
                        xyplane = NULL,
                        hidden3d = NULL,
                        dgrid3d = NULL,
                        theme = NULL,
                        settings = NULL,
                        extra = NULL,
                        output = NULL,
                        terminal = NULL,
                        preview = TRUE,
                        preview_terminal = NULL,
                        path = NULL,
                        workdir = tempdir(),
                        echo = FALSE,
                        cleanup = TRUE) {
  type <- match.arg(type)
  data <- xyz_data(x, y, z)
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- write_plot_table(data, workdir = workdir)
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }

  style <- gp_style(type, title = legend, col = col, color = color,
                    line_col = line_col, point_col = point_col, lwd = lwd,
                    lty = lty, pch = pch, cex = cex)
  body <- paste("splot", gp_quote(data_file), "using 1:2:3 with",
                style_clause(style), title_clause(style$title))

  gp_render(
    plot_body = body,
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid,
                        xrange = xrange, yrange = yrange, zrange = zrange,
                        palette = palette, cbrange = cbrange,
                        cblabel = cblabel, colorbox = colorbox,
                        view = view, view_map = view_map,
                        samples = samples, isosamples = isosamples,
                        ticslevel = ticslevel, xyplane = xyplane,
                        hidden3d = hidden3d, dgrid3d = dgrid3d,
                        theme = theme, settings = settings, extra = extra),
    output = output,
    terminal = terminal,
    preview = preview,
    preview_terminal = preview_terminal,
    path = path,
    workdir = workdir,
    echo = echo,
    cleanup = cleanup
  )
}

#' Heatmap
#'
#' @param z Numeric matrix or data frame with x/y/z columns.
#' @param x Optional x grid values for matrix input.
#' @param y Optional y grid values for matrix input.
#' @param palette Optional Gnuplot palette specification.
#' @inheritParams gp_surface
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- seq(-3, 3, length.out = 80)
#' y <- seq(-3, 3, length.out = 80)
#' z <- outer(y, x, function(y, x) sin(x) * cos(y))
#' gp_heatmap(z, x = x, y = y, palette = "rgbformulae 33,13,10",
#'            cblabel = "value", main = "Heatmap")
#' }
gp_heatmap <- function(z,
                       x = NULL,
                       y = NULL,
                       palette = NULL,
                       main = NULL,
                       subtitle = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       grid = FALSE,
                       cbrange = NULL,
                       cblabel = NULL,
                       colorbox = NULL,
                       theme = NULL,
                       extra = NULL,
                       output = NULL,
                       terminal = NULL,
                       preview = TRUE,
                       preview_terminal = NULL,
                       path = NULL,
                       workdir = tempdir(),
                       echo = FALSE,
                       cleanup = TRUE) {
  data <- matrix_to_xyz(z, x = x, y = y)
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- write_plot_table(data, workdir = workdir)
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }

  body <- paste("plot", gp_quote(data_file), "using 1:2:3 with image notitle")

  gp_render(
    plot_body = body,
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, grid = grid, key = FALSE,
                        palette = palette, cbrange = cbrange,
                        cblabel = cblabel, colorbox = colorbox,
                        theme = theme, extra = extra),
    output = output,
    terminal = terminal,
    preview = preview,
    preview_terminal = preview_terminal,
    path = path,
    workdir = workdir,
    echo = echo,
    cleanup = cleanup
  )
}

#' Contour Plot
#'
#' @param z Numeric matrix, data frame with x/y/z columns, R function
#'   `function(x, y)`, or character Gnuplot expression.
#' @param x Optional x grid values for matrix/function input.
#' @param y Optional y grid values for matrix/function input.
#' @param n Grid size used when `z` is a function and `x` or `y` is omitted.
#' @param levels Number of contour levels.
#' @param filled If `TRUE`, draw a filled pm3d map instead of contour lines
#'   only.
#' @param palette Optional Gnuplot palette specification.
#' @param col,color,line_col R-like contour color aliases.
#' @param lwd,lty R-like contour line width and line type aliases.
#' @inheritParams gp_surface
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- seq(-3, 3, length.out = 80)
#' y <- seq(-3, 3, length.out = 80)
#' z <- outer(y, x, function(y, x) x * exp(-x^2 - y^2))
#' gp_contour(z, x = x, y = y, levels = 12, col = 4, lwd = 1.5)
#' gp_contour(z, x = x, y = y, filled = TRUE, levels = 12,
#'            palette = "rgbformulae 22,13,-31")
#' }
gp_contour <- function(z,
                       x = NULL,
                       y = NULL,
                       n = 80,
                       levels = 10,
                       filled = FALSE,
                       palette = NULL,
                       xrange = NULL,
                       yrange = NULL,
                       main = NULL,
                       subtitle = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       grid = TRUE,
                       col = NULL,
                       color = NULL,
                       line_col = NULL,
                       lwd = NULL,
                       lty = NULL,
                       cbrange = NULL,
                       cblabel = NULL,
                       colorbox = NULL,
                       view = NULL,
                       samples = NULL,
                       isosamples = NULL,
                       ticslevel = NULL,
                       xyplane = NULL,
                       dgrid3d = NULL,
                       theme = NULL,
                       extra = NULL,
                       output = NULL,
                       terminal = NULL,
                       preview = TRUE,
                       preview_terminal = NULL,
                       path = NULL,
                       workdir = tempdir(),
                       echo = FALSE,
                       cleanup = TRUE) {
  if (!is.numeric(levels) || length(levels) != 1L || is.na(levels)) {
    stop("`levels` must be a single number.", call. = FALSE)
  }

  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  style_name <- if (isTRUE(filled)) "pm3d" else "lines"
  style <- gp_style(style_name, col = col, color = color, line_col = line_col,
                    lwd = lwd, lty = lty)

  if (is.character(z)) {
    if (length(z) != 1L || is.na(z) || !nzchar(z)) {
      stop("Character `z` must be a single Gnuplot expression.",
           call. = FALSE)
    }
    body <- paste("splot", z, "with", style_clause(style), "notitle")
  } else {
    data <- if (is.function(z)) {
      function_to_xyz(z, x = x, y = y, n = n)
    } else {
      matrix_to_xyz(z, x = x, y = y)
    }
    data_file <- write_grid_table(data, workdir = workdir)
    if (isTRUE(cleanup)) {
      on.exit(unlink(data_file), add = TRUE)
    }
    body <- paste("splot", gp_quote(data_file), "using 1:2:3 with",
                  style_clause(style), "notitle")
  }

  gp_render(
    plot_body = body,
    setup = c(
      gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab, ylab = ylab,
                  grid = grid, key = FALSE, xrange = xrange, yrange = yrange,
                  palette = palette, cbrange = cbrange, cblabel = cblabel,
                  colorbox = colorbox, view = view, samples = samples,
                  isosamples = isosamples, ticslevel = ticslevel,
                  xyplane = xyplane, dgrid3d = dgrid3d,
                  theme = theme, extra = extra),
      "set view map",
      "set contour base",
      paste("set cntrparam levels auto", format_gnuplot_number(levels)),
      if (!isTRUE(filled)) "unset surface",
      if (isTRUE(filled)) "set pm3d map"
    ),
    output = output,
    terminal = terminal,
    preview = preview,
    preview_terminal = preview_terminal,
    path = path,
    workdir = workdir,
    echo = echo,
    cleanup = cleanup
  )
}

xyz_data <- function(x, y = NULL, z = NULL) {
  if (is.null(y) && is.null(z) && is.data.frame(x)) {
    if (ncol(x) < 3L) {
      stop("A data frame must have at least three columns: x, y, z.",
           call. = FALSE)
    }
    out <- data.frame(x = x[[1L]], y = x[[2L]], z = x[[3L]])
  } else {
    if (is.null(y) || is.null(z)) {
      stop("Supply `x`, `y`, and `z`, or a data frame with three columns.",
           call. = FALSE)
    }
    if (length(x) != length(y) || length(x) != length(z)) {
      stop("`x`, `y`, and `z` must have the same length.", call. = FALSE)
    }
    out <- data.frame(x = x, y = y, z = z)
  }

  if (!is.numeric(out$x) || !is.numeric(out$y) || !is.numeric(out$z)) {
    stop("x, y, and z values must be numeric.", call. = FALSE)
  }
  if (nrow(out) == 0L) {
    stop("Plot data must contain at least one row.", call. = FALSE)
  }

  out
}
