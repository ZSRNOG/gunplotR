GP_PLOTTING_STYLES <- c(
  "arrows", "boxerrorbars", "boxes", "boxplot", "boxxyerror",
  "boxxyerrorbars", "candlesticks", "circles", "contourfill", "dots",
  "ellipses", "errorbars", "errorlines", "filledcurves", "financebars",
  "fillsteps", "fsteps", "histeps",
  "heatmaps", "histograms", "hsteps", "image", "impulses", "labels",
  "lines", "linespoints", "marks", "linesmarks", "parallelaxes",
  "pm3d", "points", "polygons", "rgbalpha", "rgbimage", "sectors",
  "spiderplot", "steps", "surface", "vectors", "xerrorbars", "xyerrorbars",
  "xerrorlines", "xyerrorlines", "yerrorbars", "yerrorlines",
  "zerrorfill", "isosurface"
)

#' Supported Gnuplot Plotting Styles
#'
#' @return A character vector of plotting style names known to gunplotR.
#' @export
gp_styles <- function() {
  GP_PLOTTING_STYLES
}

#' Build a Gnuplot Style Specification
#'
#' @param style Gnuplot plotting style, such as `"lines"`, `"points"`,
#'   `"vectors"`, `"yerrorbars"`, `"labels"`, or `"pm3d"`.
#' @param using Optional `using` specification, for example `"1:2:3"`.
#' @param title Optional legend title. Use `NULL` or `FALSE` for `notitle`.
#' @param axes Optional axes specification such as `"x1y2"`.
#' @param smooth Optional smoothing mode such as `"csplines"` or `"unique"`.
#' @param col,color,line_col,point_col R-like color aliases. Numeric values
#'   use R's current palette; character values can be color names or hex codes.
#' @param lwd,lty,pch,cex R-like aliases for line width, dash type, point type,
#'   and point size.
#' @param linecolor Optional gnuplot line color expression, e.g. `rgb "#3366cc"`.
#' @param linetype Optional line type.
#' @param linestyle,ls Optional reusable line style index passed as `ls`.
#' @param linewidth Optional line width.
#' @param dashtype Optional dash type.
#' @param pointtype Optional point type.
#' @param pointsize Optional point size.
#' @param fill Optional fill option, e.g. `"solid 0.5"` or `"transparent solid 0.3"`.
#' @param fillcolor,fill_col,fc Fill color. Numeric values use R's current
#'   palette; character values may be color names, hex codes, `"background"`,
#'   `"palette"`, or `"rgb variable"`.
#' @param border Border token for filled styles. Use `TRUE`, `FALSE`, or a raw
#'   string such as `"lc black"`.
#' @param textcolor,text_col,tc Text color for labels.
#' @param font Optional font specification for labels, e.g. `"Arial,10"`.
#' @param rotate Optional label rotation. Use `TRUE`, `FALSE`, a number, or a
#'   raw gnuplot string.
#' @param offset Optional label offset. Use a numeric vector or a raw string.
#' @param pointinterval,pointnumber Point interval and point number controls.
#' @param arrowstyle Optional arrow style index or `"variable"`.
#' @param units Optional raw units token for styles such as circles and marks.
#' @param marktype Optional mark type for the gnuplot `marks` style.
#' @param palette If `TRUE`, add `palette` to the style clause.
#' @param extra Optional raw style tokens appended at the end.
#'
#' @return A `gp_style` object used by `gp_layer()`.
#' @export
gp_style <- function(style,
                     using = NULL,
                     title = NULL,
                     axes = NULL,
                     smooth = NULL,
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
                     linestyle = NULL,
                     ls = NULL,
                     linewidth = NULL,
                     dashtype = NULL,
                     pointtype = NULL,
                     pointsize = NULL,
                     fill = NULL,
                     fillcolor = NULL,
                     fill_col = NULL,
                     fc = NULL,
                     border = NULL,
                     textcolor = NULL,
                     text_col = NULL,
                     tc = NULL,
                     font = NULL,
                     rotate = NULL,
                     offset = NULL,
                     pointinterval = NULL,
                     pointnumber = NULL,
                     arrowstyle = NULL,
                     units = NULL,
                     marktype = NULL,
                     palette = FALSE,
                     extra = NULL) {
  style <- validate_scalar_character(style, "style")
  using <- validate_optional_scalar(using, "using")
  axes <- validate_optional_scalar(axes, "axes")
  smooth <- validate_optional_scalar(smooth, "smooth")
  fill <- normalize_fill(fill)
  extra <- validate_script_lines(extra, "extra")

  linecolor <- linecolor %||% line_col %||% point_col %||% color %||% col
  if (!is.null(linecolor)) {
    linecolor <- gp_color(linecolor)
  }
  linewidth <- linewidth %||% lwd
  dashtype <- dashtype %||% lty
  pointtype <- pointtype %||% pch
  pointsize <- pointsize %||% cex
  linestyle <- linestyle %||% ls
  fillcolor <- fillcolor %||% fill_col %||% fc
  textcolor <- textcolor %||% text_col %||% tc
  if (!is.null(fillcolor)) {
    fillcolor <- gp_color(fillcolor)
  }
  if (!is.null(textcolor)) {
    textcolor <- gp_color(textcolor)
  }
  border <- normalize_border(border)
  if (identical(border, "noborder") && !is.null(fill)) {
    fill <- paste(fill, "noborder")
    border <- NULL
  }
  rotate <- normalize_rotate(rotate)
  offset <- normalize_offset(offset)

  if (!is.null(title) && !identical(title, FALSE)) {
    title <- validate_scalar_character(title, "title")
  }
  palette <- validate_flag(palette, "palette")

  out <- list(
    style = style,
    using = using,
    title = title,
    axes = axes,
    smooth = smooth,
    linecolor = linecolor,
    linetype = linetype,
    linestyle = linestyle,
    linewidth = linewidth,
    dashtype = dashtype,
    pointtype = pointtype,
    pointsize = pointsize,
    fill = fill,
    fillcolor = fillcolor,
    border = border,
    textcolor = textcolor,
    font = font,
    rotate = rotate,
    offset = offset,
    pointinterval = pointinterval,
    pointnumber = pointnumber,
    arrowstyle = arrowstyle,
    units = units,
    marktype = marktype,
    palette = palette,
    extra = extra
  )
  class(out) <- "gp_style"
  out
}

#' Create a Plot Layer
#'
#' `gp_layer()` is the escape hatch for gnuplot's plotting styles. It accepts
#' data, a file, or a raw expression plus a style specification. Use it with
#' `gp_multi()` to mix styles in a single plot.
#'
#' @param data Optional data frame, matrix, or vector written to a temporary
#'   data file.
#' @param x,y,z Optional vectors used to construct a data frame when `data` is
#'   not supplied.
#' @param expr Optional raw gnuplot expression, such as `"sin(x)"`.
#' @param file Optional existing data file.
#' @param style A style name or object returned by `gp_style()`.
#' @param using Optional `using` specification. Overrides `gp_style(using = )`.
#' @param title Optional legend title. Overrides `gp_style(title = )`.
#' @param grid_data If `TRUE`, write 3-column x/y/z data with blank lines
#'   between y-groups, suitable for `splot` surfaces and pm3d.
#' @param quote_data If `TRUE`, quote character columns when writing data.
#' @param ... Style options passed to `gp_style()`.
#'
#' @return A `gp_layer` object.
#' @export
gp_layer <- function(data = NULL,
                     x = NULL,
                     y = NULL,
                     z = NULL,
                     expr = NULL,
                     file = NULL,
                     style = "lines",
                     using = NULL,
                     title = NULL,
                     grid_data = FALSE,
                     quote_data = FALSE,
                     ...) {
  sources <- sum(!vapply(list(data, expr, file), is.null, logical(1)))
  if (!is.null(x) || !is.null(y) || !is.null(z)) {
    sources <- sources + 1L
  }
  if (sources != 1L) {
    stop("Supply exactly one data source: `data`, `x`/`y`/`z`, `expr`, or `file`.",
         call. = FALSE)
  }

  if (!inherits(style, "gp_style")) {
    style <- gp_style(style, ...)
  }
  if (!is.null(using)) {
    style$using <- validate_scalar_character(using, "using")
  }
  if (!is.null(title) || identical(title, FALSE)) {
    style$title <- title
  }

  if (!is.null(expr)) {
    expr <- validate_scalar_character(expr, "expr")
  }
  if (!is.null(file)) {
    file <- validate_scalar_character(file, "file")
  }
  if (!is.logical(grid_data) || length(grid_data) != 1L || is.na(grid_data)) {
    stop("`grid_data` must be TRUE or FALSE.", call. = FALSE)
  }
  if (!is.logical(quote_data) || length(quote_data) != 1L || is.na(quote_data)) {
    stop("`quote_data` must be TRUE or FALSE.", call. = FALSE)
  }

  if (is.null(data) && (sources == 1L) && (!is.null(x) || !is.null(y) || !is.null(z))) {
    data <- layer_xyz_data(x, y, z)
  }

  out <- list(
    data = data,
    expr = expr,
    file = file,
    style = style,
    grid_data = grid_data,
    quote_data = quote_data
  )
  class(out) <- "gp_layer"
  out
}

#' Plot Multiple Layers
#'
#' @param ... `gp_layer()` objects.
#' @param layers Optional list of `gp_layer()` objects.
#' @param dimensions `"2d"` for `plot` or `"3d"` for `splot`.
#' @param key If `TRUE`, show legend. If `FALSE`, unset legend.
#' @param zlab Optional z-axis label.
#' @inheritParams gp_plot
#' @inheritParams gp_surface
#' @inheritParams gp_3d_setup
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_multi <- function(...,
                     layers = NULL,
                     dimensions = c("2d", "3d"),
                     main = NULL,
                     subtitle = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     zlab = NULL,
                     grid = TRUE,
                     key = TRUE,
                     xrange = NULL,
                     yrange = NULL,
                     zrange = NULL,
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
                     hidden3d = NULL,
                     surface = NULL,
                     contour = NULL,
                     dgrid3d = NULL,
                     theme = NULL,
                     font_family = NULL,
                     title_font = NULL,
                     title_size = NULL,
                     subtitle_font = NULL,
                     subtitle_size = NULL,
                     axis_font = NULL,
                     axis_size = NULL,
                     tick_font = NULL,
                     tick_size = NULL,
                     legend_font = NULL,
                     legend_size = NULL,
                     legend_position = NULL,
                     legend_box = NULL,
                     legend_cols = NULL,
                     grid_col = NULL,
                     grid_lty = NULL,
                     border_lwd = NULL,
                     border_col = NULL,
                     background = NULL,
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
  dimensions <- match.arg(dimensions)
  dots <- list(...)
  if (!is.null(layers)) {
    dots <- c(dots, layers)
  }
  if (length(dots) == 0L) {
    stop("Supply at least one `gp_layer()`.", call. = FALSE)
  }
  if (!all(vapply(dots, inherits, logical(1), what = "gp_layer"))) {
    stop("All layers must be created with `gp_layer()`.", call. = FALSE)
  }

  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  files_to_remove <- character()
  on.exit(if (isTRUE(cleanup)) unlink(files_to_remove), add = TRUE)

  terms <- vapply(dots, function(layer) {
    prepared <- prepare_layer(layer, workdir = workdir)
    if (!is.na(prepared$file)) {
      files_to_remove <<- c(files_to_remove, prepared$file)
    }
    prepared$term
  }, character(1))

  command <- if (dimensions == "3d") "splot" else "plot"
  body <- paste(command, paste(terms, collapse = ", \\\n     "))
  setup <- if (dimensions == "3d") {
    gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab, ylab = ylab,
                zlab = zlab, grid = grid, key = key, xrange = xrange,
                yrange = yrange, zrange = zrange, palette = palette,
                cbrange = cbrange, cblabel = cblabel,
                colorbox = colorbox, view = view, view_map = view_map,
                samples = samples, isosamples = isosamples,
                ticslevel = ticslevel, xyplane = xyplane,
                pm3d = pm3d, pm3d_at = pm3d_at, hidden3d = hidden3d,
                surface = surface, contour = contour, dgrid3d = dgrid3d,
                theme = theme, font_family = font_family,
                title_font = title_font, title_size = title_size,
                subtitle_font = subtitle_font, subtitle_size = subtitle_size,
                axis_font = axis_font, axis_size = axis_size,
                tick_font = tick_font, tick_size = tick_size,
                legend_font = legend_font, legend_size = legend_size,
                legend_position = legend_position, legend_box = legend_box,
                legend_cols = legend_cols, grid_col = grid_col,
                grid_lty = grid_lty, border_lwd = border_lwd,
                border_col = border_col, background = background,
                settings = settings,
                extra = extra)
  } else {
    gp_setup(main = main, subtitle = subtitle, xlab = xlab, ylab = ylab,
             zlab = zlab, grid = grid, key = key, xrange = xrange,
             yrange = yrange, zrange = zrange, palette = palette,
             theme = theme, font_family = font_family,
             title_font = title_font, title_size = title_size,
             subtitle_font = subtitle_font, subtitle_size = subtitle_size,
             axis_font = axis_font, axis_size = axis_size,
             tick_font = tick_font, tick_size = tick_size,
             legend_font = legend_font, legend_size = legend_size,
             legend_position = legend_position, legend_box = legend_box,
             legend_cols = legend_cols, grid_col = grid_col,
             grid_lty = grid_lty, border_lwd = border_lwd,
             border_col = border_col, background = background,
             settings = settings,
             extra = extra)
  }

  gp_render(
    plot_body = body,
    setup = setup,
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

#' Plot Error Bars or Error Lines
#'
#' @param x,y Numeric coordinates.
#' @param ymin,ymax Lower and upper y errors.
#' @param xmin,xmax Lower and upper x errors.
#' @param type Error style.
#' @param ... Style options passed to `gp_style()` plus named theme options
#'   accepted by `gp_multi()`, such as `subtitle`, `theme`, `legend_position`,
#'   `grid_col`, or `tick_size`.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_errorbars <- function(x,
                         y,
                         ymin = NULL,
                         ymax = NULL,
                         xmin = NULL,
                         xmax = NULL,
                         type = c("yerrorbars", "yerrorlines", "xerrorbars",
                                  "xerrorlines", "xyerrorbars", "xyerrorlines"),
                         main = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         grid = TRUE,
                         extra = NULL,
                         output = NULL,
                         terminal = NULL,
                         preview = TRUE,
                         preview_terminal = NULL,
                         path = NULL,
                         workdir = tempdir(),
                         echo = FALSE,
                         cleanup = TRUE,
                         ...) {
  type <- match.arg(type)
  validate_same_length(x = x, y = y)
  dots <- split_gp_dots(list(...))

  if (startsWith(type, "xy")) {
    validate_same_length(x = x, y = y, xmin = xmin, xmax = xmax,
                         ymin = ymin, ymax = ymax)
    data <- data.frame(x = x, y = y, xmin = xmin, xmax = xmax,
                       ymin = ymin, ymax = ymax)
    using <- "1:2:3:4:5:6"
  } else if (startsWith(type, "x")) {
    validate_same_length(x = x, y = y, xmin = xmin, xmax = xmax)
    data <- data.frame(x = x, y = y, xmin = xmin, xmax = xmax)
    using <- "1:2:3:4"
  } else {
    validate_same_length(x = x, y = y, ymin = ymin, ymax = ymax)
    data <- data.frame(x = x, y = y, ymin = ymin, ymax = ymax)
    using <- "1:2:3:4"
  }

  layer <- do.call(
    gp_layer,
    c(list(data = data, style = type, using = using), dots$style)
  )
  do.call(
    gp_multi,
    c(
      list(layer, main = main, xlab = xlab, ylab = ylab, grid = grid,
           extra = extra, output = output, terminal = terminal,
           preview = preview, preview_terminal = preview_terminal, path = path,
           workdir = workdir, echo = echo, cleanup = cleanup),
      dots$setup
    )
  )
}

#' Vector Field
#'
#' @param x,y Numeric vector origins.
#' @param dx,dy Vector components.
#' @param ... Style options passed to `gp_style()` plus named theme options
#'   accepted by `gp_multi()`.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_vectors <- function(x,
                       y,
                       dx,
                       dy,
                       main = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       grid = TRUE,
                       extra = NULL,
                       output = NULL,
                       terminal = NULL,
                       preview = TRUE,
                       preview_terminal = NULL,
                       path = NULL,
                       workdir = tempdir(),
                       echo = FALSE,
                       cleanup = TRUE,
                       ...) {
  validate_same_length(x = x, y = y, dx = dx, dy = dy)
  dots <- split_gp_dots(list(...))
  layer <- do.call(
    gp_layer,
    c(
      list(data = data.frame(x = x, y = y, dx = dx, dy = dy),
           style = "vectors", using = "1:2:3:4"),
      dots$style
    )
  )
  do.call(
    gp_multi,
    c(
      list(layer, main = main, xlab = xlab, ylab = ylab, grid = grid,
           extra = extra, output = output, terminal = terminal,
           preview = preview, preview_terminal = preview_terminal, path = path,
           workdir = workdir, echo = echo, cleanup = cleanup),
      dots$setup
    )
  )
}

#' Text Labels
#'
#' @param x,y Numeric label positions.
#' @param labels Character labels.
#' @param ... Style options passed to `gp_style()` plus named theme options
#'   accepted by `gp_multi()`.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_labels <- function(x,
                      y,
                      labels,
                      main = NULL,
                      xlab = NULL,
                      ylab = NULL,
                      grid = TRUE,
                      extra = NULL,
                      output = NULL,
                      terminal = NULL,
                      preview = TRUE,
                      preview_terminal = NULL,
                      path = NULL,
                      workdir = tempdir(),
                      echo = FALSE,
                      cleanup = TRUE,
                      ...) {
  validate_same_length(x = x, y = y, labels = labels)
  dots <- split_gp_dots(list(...))
  layer <- do.call(
    gp_layer,
    c(
      list(data = data.frame(x = x, y = y, label = labels),
           style = "labels", using = "1:2:3", quote_data = TRUE),
      dots$style
    )
  )
  do.call(
    gp_multi,
    c(
      list(layer, main = main, xlab = xlab, ylab = ylab, grid = grid,
           extra = extra, output = output, terminal = terminal,
           preview = preview, preview_terminal = preview_terminal, path = path,
           workdir = workdir, echo = echo, cleanup = cleanup),
      dots$setup
    )
  )
}

#' Circles
#'
#' @param x,y Numeric circle centers.
#' @param radius Optional radius per circle.
#' @param ... Style options passed to `gp_style()` plus named theme options
#'   accepted by `gp_multi()`.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_circles <- function(x,
                       y,
                       radius = NULL,
                       main = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       grid = TRUE,
                       extra = NULL,
                       output = NULL,
                       terminal = NULL,
                       preview = TRUE,
                       preview_terminal = NULL,
                       path = NULL,
                       workdir = tempdir(),
                       echo = FALSE,
                       cleanup = TRUE,
                       ...) {
  dots <- split_gp_dots(list(...))
  if (is.null(radius)) {
    validate_same_length(x = x, y = y)
    data <- data.frame(x = x, y = y)
    using <- "1:2"
  } else {
    validate_same_length(x = x, y = y, radius = radius)
    data <- data.frame(x = x, y = y, radius = radius)
    using <- "1:2:3"
  }

  layer <- do.call(
    gp_layer,
    c(list(data = data, style = "circles", using = using), dots$style)
  )
  do.call(
    gp_multi,
    c(
      list(layer, main = main, xlab = xlab, ylab = ylab, grid = grid,
           extra = extra, output = output, terminal = terminal,
           preview = preview, preview_terminal = preview_terminal, path = path,
           workdir = workdir, echo = echo, cleanup = cleanup),
      dots$setup
    )
  )
}

#' Filled Curves
#'
#' @param x,y Numeric coordinates.
#' @param y2 Optional second y boundary.
#' @param fill Fill option.
#' @param ... Style options passed to `gp_style()` plus named theme options
#'   accepted by `gp_multi()`.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_filledcurves <- function(x,
                            y,
                            y2 = NULL,
                            fill = "solid 0.35",
                            main = NULL,
                            xlab = NULL,
                            ylab = NULL,
                            grid = TRUE,
                            extra = NULL,
                            output = NULL,
                            terminal = NULL,
                            preview = TRUE,
                            preview_terminal = NULL,
                            path = NULL,
                            workdir = tempdir(),
                            echo = FALSE,
                            cleanup = TRUE,
                            ...) {
  dots <- split_gp_dots(list(...))
  if (is.null(y2)) {
    validate_same_length(x = x, y = y)
    data <- data.frame(x = x, y = y)
    using <- "1:2"
  } else {
    validate_same_length(x = x, y = y, y2 = y2)
    data <- data.frame(x = x, y = y, y2 = y2)
    using <- "1:2:3"
  }

  style <- do.call(
    gp_style,
    c(list(style = "filledcurves", using = using, fill = fill), dots$style)
  )
  do.call(
    gp_multi,
    c(
      list(gp_layer(data = data, style = style), main = main, xlab = xlab,
           ylab = ylab, grid = grid, extra = extra, output = output,
           terminal = terminal, preview = preview,
           preview_terminal = preview_terminal, path = path, workdir = workdir,
           echo = echo, cleanup = cleanup),
      dots$setup
    )
  )
}

#' Box Plot
#'
#' @param x Numeric values.
#' @param group Optional group labels.
#' @param ... Style options passed to `gp_style()` plus named theme options
#'   accepted by `gp_multi()`.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_boxplot <- function(x,
                       group = NULL,
                       main = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       grid = TRUE,
                       extra = NULL,
                       output = NULL,
                       terminal = NULL,
                       preview = TRUE,
                       preview_terminal = NULL,
                       path = NULL,
                       workdir = tempdir(),
                       echo = FALSE,
                       cleanup = TRUE,
                       ...) {
  if (!is.numeric(x) || length(x) == 0L) {
    stop("`x` must be a non-empty numeric vector.", call. = FALSE)
  }
  if (is.null(group)) {
    group <- rep("All", length(x))
  }
  validate_same_length(x = x, group = group)
  group <- as.character(group)
  level <- match(group, unique(group))
  data <- data.frame(group = level, value = x, label = group)
  dots <- split_gp_dots(list(...))
  layer <- do.call(
    gp_layer,
    c(
      list(data = data, style = "boxplot", using = "1:2:xtic(3)",
           quote_data = TRUE),
      dots$style
    )
  )

  do.call(
    gp_multi,
    c(
      list(layer, main = main, xlab = xlab, ylab = ylab, grid = grid,
           extra = c("set style boxplot", extra), output = output,
           terminal = terminal, preview = preview,
           preview_terminal = preview_terminal, path = path, workdir = workdir,
           echo = echo, cleanup = cleanup),
      dots$setup
    )
  )
}

#' Finance Bars or Candlesticks
#'
#' @param x Numeric x positions.
#' @param open,high,low,close Numeric OHLC values.
#' @param type `"candlesticks"` or `"financebars"`.
#' @param ... Style options passed to `gp_style()` plus named theme options
#'   accepted by `gp_multi()`.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_finance <- function(x,
                       open,
                       high,
                       low,
                       close,
                       type = c("candlesticks", "financebars"),
                       main = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       grid = TRUE,
                       extra = NULL,
                       output = NULL,
                       terminal = NULL,
                       preview = TRUE,
                       preview_terminal = NULL,
                      path = NULL,
                      workdir = tempdir(),
                      echo = FALSE,
                      cleanup = TRUE,
                      ...) {
  type <- match.arg(type)
  validate_same_length(x = x, open = open, high = high, low = low,
                       close = close)
  dots <- split_gp_dots(list(...))
  layer <- do.call(
    gp_layer,
    c(
      list(data = data.frame(x = x, open = open, low = low,
                             high = high, close = close),
           style = type, using = "1:2:3:4:5"),
      dots$style
    )
  )
  do.call(
    gp_multi,
    c(
      list(layer, main = main, xlab = xlab, ylab = ylab, grid = grid,
           extra = extra, output = output, terminal = terminal,
           preview = preview, preview_terminal = preview_terminal, path = path,
           workdir = workdir, echo = echo, cleanup = cleanup),
      dots$setup
    )
  )
}

#' Generic Image-Style Plot
#'
#' @param data Matrix or data frame with image columns.
#' @param style `"image"`, `"rgbimage"`, or `"rgbalpha"`.
#' @param using Optional gnuplot using specification.
#' @param ... Style options passed to `gp_style()` plus named theme options
#'   accepted by `gp_multi()`.
#' @inheritParams gp_heatmap
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
gp_image <- function(data,
                     style = c("image", "rgbimage", "rgbalpha"),
                     using = NULL,
                     x = NULL,
                     y = NULL,
                     palette = NULL,
                     main = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     grid = FALSE,
                     extra = NULL,
                     output = NULL,
                     terminal = NULL,
                     preview = TRUE,
                     preview_terminal = NULL,
                     path = NULL,
                     workdir = tempdir(),
                     echo = FALSE,
                     cleanup = TRUE,
                     ...) {
  style <- match.arg(style)
  if (is.null(using)) {
    using <- if (style == "image") "1:2:3" else "1:2:3:4:5"
  }
  if (is.matrix(data)) {
    data <- matrix_to_xyz(data, x = x, y = y)
  }
  dots <- split_gp_dots(list(...))
  layer <- do.call(
    gp_layer,
    c(list(data = data, style = style, using = using), dots$style)
  )

  do.call(
    gp_multi,
    c(
      list(layer, main = main, xlab = xlab, ylab = ylab, grid = grid,
           key = FALSE,
           extra = c(if (!is.null(palette)) paste("set palette", palette),
                     extra),
           output = output, terminal = terminal, preview = preview,
           preview_terminal = preview_terminal, path = path, workdir = workdir,
           echo = echo, cleanup = cleanup),
      dots$setup
    )
  )
}

prepare_layer <- function(layer, workdir) {
  style <- layer$style
  temporary_file <- NA_character_

  if (!is.null(layer$expr)) {
    source <- layer$expr
  } else if (!is.null(layer$file)) {
    source <- gp_quote(normalizePath(layer$file, winslash = "/",
                                     mustWork = FALSE))
  } else {
    data <- coerce_layer_data(layer$data)
    if (isTRUE(layer$grid_data)) {
      temporary_file <- write_grid_table(data, workdir = workdir)
    } else {
      temporary_file <- write_plot_table(data, workdir = workdir,
                                         quote = layer$quote_data)
    }
    source <- gp_quote(temporary_file)
  }

  list(
    term = layer_term(source, style),
    file = temporary_file
  )
}

layer_term <- function(source, style) {
  parts <- c(
    source,
    if (!is.null(style$using)) paste("using", style$using),
    if (!is.null(style$smooth)) paste("smooth", style$smooth),
    if (!is.null(style$axes)) paste("axes", style$axes),
    title_clause(style$title),
    "with",
    style_clause(style)
  )
  paste(parts, collapse = " ")
}

style_clause <- function(style) {
  parts <- c(
    style$style,
    if (!is.null(style$linetype)) paste("lt", gp_value(style$linetype)),
    if (!is.null(style$linestyle)) paste("ls", gp_value(style$linestyle)),
    if (!is.null(style$linewidth)) paste("lw", gp_value(style$linewidth)),
    if (!is.null(style$dashtype)) paste("dt", gp_value(style$dashtype)),
    if (!is.null(style$pointtype)) paste("pt", gp_value(style$pointtype)),
    if (!is.null(style$pointsize)) paste("ps", gp_value(style$pointsize)),
    if (!is.null(style$linecolor)) paste("lc", style$linecolor),
    if (!is.null(style$fill)) paste("fs", style$fill),
    if (!is.null(style$fillcolor)) paste("fc", style$fillcolor),
    if (!is.null(style$border)) style$border,
    if (!is.null(style$textcolor)) paste("tc", style$textcolor),
    if (!is.null(style$font)) paste("font", gp_string_quote(style$font)),
    if (!is.null(style$rotate)) style$rotate,
    if (!is.null(style$offset)) style$offset,
    if (!is.null(style$pointinterval)) paste("pi", gp_value(style$pointinterval)),
    if (!is.null(style$pointnumber)) paste("pn", gp_value(style$pointnumber)),
    if (!is.null(style$arrowstyle)) paste("arrowstyle", gp_value(style$arrowstyle)),
    if (!is.null(style$units)) paste("units", validate_scalar_character(style$units, "units")),
    if (!is.null(style$marktype)) paste("mt", gp_value(style$marktype)),
    if (isTRUE(style$palette)) "palette",
    style$extra
  )
  paste(parts, collapse = " ")
}

title_clause <- function(title) {
  if (is.null(title) || identical(title, FALSE)) {
    "notitle"
  } else {
    paste("title", gp_quote(title))
  }
}

coerce_layer_data <- function(data) {
  if (is.data.frame(data)) {
    return(data)
  }
  if (is.matrix(data)) {
    return(as.data.frame(data))
  }
  if (is.atomic(data)) {
    return(data.frame(x = seq_along(data), y = data))
  }
  stop("Layer data must be a data frame, matrix, or vector.", call. = FALSE)
}

layer_xyz_data <- function(x, y, z = NULL) {
  if (is.null(y)) {
    stop("`y` is required when constructing a layer from vectors.",
         call. = FALSE)
  }
  if (is.null(z)) {
    validate_same_length(x = x, y = y)
    data.frame(x = x, y = y)
  } else {
    validate_same_length(x = x, y = y, z = z)
    data.frame(x = x, y = y, z = z)
  }
}

validate_scalar_character <- function(x, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", arg, "` must be a single non-empty character value.",
         call. = FALSE)
  }
  x
}

validate_optional_scalar <- function(x, arg) {
  if (is.null(x)) {
    return(NULL)
  }
  validate_scalar_character(x, arg)
}

validate_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be TRUE or FALSE.", call. = FALSE)
  }
  x
}

validate_same_length <- function(...) {
  values <- list(...)
  names <- names(values)
  missing <- vapply(values, is.null, logical(1))
  if (any(missing)) {
    stop("Missing required vector(s): ",
         paste(names[missing], collapse = ", "), call. = FALSE)
  }
  lengths <- vapply(values, length, integer(1))
  if (length(unique(lengths)) != 1L) {
    stop("Input vectors must have the same length.", call. = FALSE)
  }
  invisible(TRUE)
}

split_gp_dots <- function(dots) {
  if (length(dots) == 0L) {
    return(list(style = list(), setup = list()))
  }
  setup_names <- c(
    "subtitle", "theme", "key", "font_family", "title_font", "title_size",
    "subtitle_font", "subtitle_size", "axis_font", "axis_size",
    "tick_font", "tick_size", "legend_font", "legend_size",
    "legend_position", "legend_box", "legend_cols", "grid_col", "grid_lty",
    "border_lwd", "border_col", "background", "palette", "cbrange",
    "cblabel", "colorbox", "settings", "view", "view_map", "samples", "isosamples",
    "ticslevel", "xyplane", "pm3d", "pm3d_at", "hidden3d", "surface",
    "contour", "dgrid3d"
  )
  dot_names <- names(dots)
  dot_names[is.na(dot_names)] <- ""
  is_setup <- nzchar(dot_names) & dot_names %in% setup_names
  list(style = dots[!is_setup], setup = dots[is_setup])
}
