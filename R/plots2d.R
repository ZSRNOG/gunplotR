#' Plot a Gnuplot expression
#'
#' @param expr Character vector of Gnuplot expressions, for example
#'   `"sin(x)"`.
#' @param xrange Optional x-axis range.
#' @param yrange Optional y-axis range.
#' @param type Gnuplot plot style.
#' @param legend Legend labels. Use `FALSE` for no legend. By default the
#'   expression text is used.
#' @param settings Optional character vector returned by `gp_options()`.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_function <- function(expr,
                        xrange = NULL,
                        yrange = NULL,
                        type = c("lines", "points", "linespoints", "dots",
                                 "impulses"),
                        legend = TRUE,
                        main = NULL,
                        subtitle = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        grid = TRUE,
                        col = NULL,
                        color = NULL,
                        line_col = NULL,
                        point_col = NULL,
                        lwd = NULL,
                        lty = NULL,
                        pch = NULL,
                        cex = NULL,
                        linecolor = NULL,
                        linetype = NULL,
                        linewidth = NULL,
                        dashtype = NULL,
                        pointtype = NULL,
                        pointsize = NULL,
                        fill = NULL,
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
  if (!is.character(expr) || length(expr) == 0L || anyNA(expr)) {
    stop("`expr` must be a non-empty character vector.", call. = FALSE)
  }

  labels <- function_labels(expr, legend)
  n <- length(expr)
  style_args <- list(
    col = recycle_gp_arg(col, n, "col"),
    color = recycle_gp_arg(color, n, "color"),
    line_col = recycle_gp_arg(line_col, n, "line_col"),
    point_col = recycle_gp_arg(point_col, n, "point_col"),
    lwd = recycle_gp_arg(lwd, n, "lwd"),
    lty = recycle_gp_arg(lty, n, "lty"),
    pch = recycle_gp_arg(pch, n, "pch"),
    cex = recycle_gp_arg(cex, n, "cex"),
    linecolor = recycle_gp_arg(linecolor, n, "linecolor"),
    linetype = recycle_gp_arg(linetype, n, "linetype"),
    linewidth = recycle_gp_arg(linewidth, n, "linewidth"),
    dashtype = recycle_gp_arg(dashtype, n, "dashtype"),
    pointtype = recycle_gp_arg(pointtype, n, "pointtype"),
    pointsize = recycle_gp_arg(pointsize, n, "pointsize"),
    fill = recycle_gp_arg(fill, n, "fill")
  )
  terms <- vapply(seq_len(n), function(i) {
    style <- gp_style(
      type,
      title = labels[[i]],
      col = style_args$col[[i]],
      color = style_args$color[[i]],
      line_col = style_args$line_col[[i]],
      point_col = style_args$point_col[[i]],
      lwd = style_args$lwd[[i]],
      lty = style_args$lty[[i]],
      pch = style_args$pch[[i]],
      cex = style_args$cex[[i]],
      linecolor = style_args$linecolor[[i]],
      linetype = style_args$linetype[[i]],
      linewidth = style_args$linewidth[[i]],
      dashtype = style_args$dashtype[[i]],
      pointtype = style_args$pointtype[[i]],
      pointsize = style_args$pointsize[[i]],
      fill = style_args$fill[[i]]
    )
    paste(expr[[i]], "with", style_clause(style), title_clause(style$title))
  }, character(1))

  gp_render(
    plot_body = paste("plot", paste(terms, collapse = ", ")),
    setup = gp_setup(main = main, subtitle = subtitle, xlab = xlab,
                     ylab = ylab, grid = grid,
                     key = !identical(legend, FALSE), xrange = xrange,
                     yrange = yrange, theme = theme, settings = settings,
                     extra = extra),
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

#' @rdname gp_plot
#' @export
gp_line <- function(x, y = NULL, ...) {
  gp_plot(x = x, y = y, type = "lines", ...)
}

#' @rdname gp_plot
#' @export
gp_scatter <- function(x, y = NULL, ...) {
  gp_plot(x = x, y = y, type = "points", ...)
}

#' Bar Plot
#'
#' @param height Numeric bar heights.
#' @param names Optional bar labels.
#' @param width Box width passed to Gnuplot.
#' @param fill Fill opacity between 0 and 1.
#' @param col Bar or histogram color. Numeric values use R's current palette.
#' @param lwd Bar or histogram outline line width.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_bar <- function(height,
                   names = NULL,
                   width = 0.8,
                   fill = 0.7,
                   main = NULL,
                   subtitle = NULL,
                   xlab = NULL,
                   ylab = NULL,
                   grid = TRUE,
                   col = NULL,
                   lwd = NULL,
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
  if (!is.numeric(height) || length(height) == 0L) {
    stop("`height` must be a non-empty numeric vector.", call. = FALSE)
  }
  if (is.null(names)) {
    names <- base::names(height)
  }
  if (is.null(names)) {
    names <- seq_along(height)
  }
  if (length(names) != length(height)) {
    stop("`names` must have the same length as `height`.", call. = FALSE)
  }

  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data <- data.frame(x = seq_along(height), y = height,
                     label = as.character(names))
  data_file <- write_plot_table(data, workdir = workdir, quote = TRUE)
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }

  style <- gp_style("boxes", col = col, lwd = lwd, fill = fill)
  body <- paste(
    "plot",
    gp_quote(data_file),
    "using 1:2:xtic(3) with",
    style_clause(style),
    "notitle"
  )

  gp_render(
    plot_body = body,
    setup = c(
      gp_setup(main = main, subtitle = subtitle, xlab = xlab, ylab = ylab,
               grid = grid, key = FALSE, theme = theme, extra = extra),
      paste("set boxwidth", format_gnuplot_number(width))
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

#' Histogram
#'
#' @param x Numeric values.
#' @param bins Breaks specification passed to `graphics::hist()`.
#' @param frequency If `TRUE`, plot counts; otherwise plot density.
#' @inheritParams gp_bar
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_histogram <- function(x,
                         bins = "Sturges",
                         frequency = TRUE,
                         main = NULL,
                         subtitle = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         grid = TRUE,
                         col = NULL,
                         lwd = NULL,
                         fill = 0.65,
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
  if (!is.numeric(x) || length(x) == 0L) {
    stop("`x` must be a non-empty numeric vector.", call. = FALSE)
  }
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    stop("`x` must contain at least one non-missing value.", call. = FALSE)
  }
  h <- graphics::hist(x, breaks = bins, plot = FALSE)
  y <- if (isTRUE(frequency)) h$counts else h$density
  ylab <- if (is.null(ylab)) {
    if (isTRUE(frequency)) "Count" else "Density"
  } else {
    ylab
  }

  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data <- data.frame(x = h$mids, y = y, width = diff(h$breaks))
  data_file <- write_plot_table(data, workdir = workdir)
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }

  style <- gp_style("boxes", col = col, lwd = lwd, fill = fill)
  body <- paste(
    "plot",
    gp_quote(data_file),
    "using 1:2:3 with",
    style_clause(style),
    "notitle"
  )

  gp_render(
    plot_body = body,
    setup = c(
      gp_setup(main = main, subtitle = subtitle, xlab = xlab, ylab = ylab,
               grid = grid, key = FALSE, theme = theme, extra = extra)
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

#' Density Plot
#'
#' @param x Numeric values.
#' @param adjust Bandwidth adjustment passed to `stats::density()`.
#' @param n Number of density points.
#' @param ... Additional arguments passed to `gp_plot()`, including style and
#'   theme options.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_density <- function(x,
                       adjust = 1,
                       n = 512,
                       main = NULL,
                       xlab = NULL,
                       ylab = "Density",
                       ...) {
  if (!is.numeric(x) || length(x) == 0L) {
    stop("`x` must be a non-empty numeric vector.", call. = FALSE)
  }
  x <- x[!is.na(x)]
  if (length(x) < 2L) {
    stop("`x` must contain at least two non-missing values.", call. = FALSE)
  }
  density <- stats::density(x, adjust = adjust, n = n)
  gp_plot(density$x, density$y, type = "lines", main = main, xlab = xlab,
          ylab = ylab, ...)
}

function_labels <- function(expr, legend) {
  if (identical(legend, FALSE)) {
    return(rep(list(FALSE), length(expr)))
  }
  if (identical(legend, TRUE) || is.null(legend)) {
    return(as.list(expr))
  }
  if (length(legend) != length(expr)) {
    stop("`legend` must have one label per expression.", call. = FALSE)
  }
  as.list(legend)
}
