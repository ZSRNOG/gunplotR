#' Plot XY data with Gunplot or Gnuplot
#'
#' `gp_plot()` writes XY data to a temporary data file, generates a small
#' plotting script, and calls the external plotting program.
#'
#' @param x Numeric x values, a numeric y vector when `y = NULL`, or a data
#'   frame whose first two columns are x and y.
#' @param y Optional numeric y values.
#' @param output Optional output file path. The default is `NULL`, so no image
#'   file is saved unless you explicitly supply a path. Supported automatic
#'   terminals are selected from file extensions: `png`, `pdf`, `svg`, `eps`,
#'   `jpg`, `jpeg`, and `gif`.
#' @param terminal Optional terminal command, such as
#'   `"pngcairo size 900,600"`. When omitted and `output` is supplied, a
#'   terminal is inferred from the output extension. When `output = NULL`, this
#'   can be used as the preview terminal.
#' @param preview If `TRUE`, show the plot in an interactive window before
#'   returning. This is the default.
#' @param preview_terminal Optional terminal command used for the preview
#'   window. When omitted, gunplotR uses `options("gunplotR.preview_terminal")`,
#'   then falls back to `windows` on Windows and `qt` elsewhere.
#' @param type Plot style passed to the plotting program.
#' @param main Optional plot title.
#' @param subtitle Optional subtitle drawn below the main title.
#' @param xlab Optional x-axis label.
#' @param ylab Optional y-axis label.
#' @param legend Optional legend label for the series. Use `NULL` for no legend.
#' @param grid If `TRUE`, add a grid.
#' @param col,color,line_col,point_col R-like color aliases. Numeric values
#'   use R's current palette; character values can be color names or hex codes.
#' @param lwd,lty,pch,cex R-like aliases for line width, dash type, point type,
#'   and point size.
#' @param linecolor,linetype,linewidth,dashtype,pointtype,pointsize Gnuplot
#'   style parameters. The R-like aliases above are usually more convenient.
#' @param fill Optional fill style for styles such as `boxes`; a number is
#'   treated as fill opacity.
#' @param theme Optional object created by `gp_theme()`.
#' @param font_family,title_font,title_size,subtitle_font,subtitle_size Axis,
#'   title, and subtitle font overrides. These are shortcuts merged into
#'   `theme`.
#' @param axis_font,axis_size,tick_font,tick_size Axis label and tick font
#'   overrides.
#' @param legend_font,legend_size,legend_position,legend_box,legend_cols
#'   Legend appearance overrides.
#' @param grid_col,grid_lty,border_lwd,border_col,background Plot frame,
#'   grid, and background overrides.
#' @param settings Optional character vector returned by `gp_options()`.
#' @param extra Optional additional script lines inserted before the final
#'   `plot` command.
#' @param path Optional executable path or command name.
#' @param workdir Directory used for temporary files.
#' @param echo If `TRUE`, print the generated script before running it.
#' @param cleanup If `TRUE`, delete generated temporary files.
#' @param ... Additional arguments passed to `gp_plot()` by wrapper functions
#'   such as `gp_line()` and `gp_scatter()`.
#'
#' @return Invisibly returns a list containing the process result, output path,
#'   data file path, and data used for plotting.
#' @export
gp_plot <- function(x,
                    y = NULL,
                    output = NULL,
                    terminal = NULL,
                    preview = TRUE,
                    preview_terminal = NULL,
                    type = c("lines", "points", "linespoints", "dots",
                             "impulses", "boxes"),
                    main = NULL,
                    subtitle = NULL,
                    xlab = NULL,
                    ylab = NULL,
                    legend = NULL,
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
                    path = NULL,
                    workdir = tempdir(),
                    echo = FALSE,
                    cleanup = TRUE) {
  type <- match.arg(type)
  data <- xy_data(x, y)

  if (!is.logical(preview) || length(preview) != 1L || is.na(preview)) {
    stop("`preview` must be TRUE or FALSE.", call. = FALSE)
  }
  terminal <- validate_terminal(terminal, "terminal")
  preview_terminal <- validate_terminal(preview_terminal, "preview_terminal")
  if (!is.null(extra) && !is.character(extra)) {
    stop("`extra` must be a character vector of script lines.", call. = FALSE)
  }
  if (anyNA(extra)) {
    stop("`extra` cannot contain missing values.", call. = FALSE)
  }

  workdir <- normalizePath(
    ensure_directory(workdir),
    winslash = "/",
    mustWork = TRUE
  )
  data_file <- tempfile("gunplotR-data-", tmpdir = workdir, fileext = ".dat")
  utils::write.table(
    data,
    file = data_file,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE,
    quote = FALSE,
    na = "NaN"
  )

  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }

  output_path <- normalize_output(output)
  if (!is.null(output_path)) {
    ensure_directory(dirname(output_path))
  }

  if (is.null(output_path) && !isTRUE(preview)) {
    stop("Nothing to draw: use `preview = TRUE` or supply `output`.",
         call. = FALSE)
  }

  output_terminal <- NULL
  if (!is.null(output_path)) {
    output_terminal <- if (is.null(terminal)) {
      terminal_from_output(output_path)
    } else {
      terminal
    }
  }

  display_terminal <- NULL
  if (isTRUE(preview)) {
    display_terminal <- if (!is.null(preview_terminal)) {
      preview_terminal
    } else if (is.null(output_path) && !is.null(terminal)) {
      terminal
    } else {
      interactive_terminal()
    }
  }

  style <- gp_style(
    type,
    title = legend,
    col = col,
    color = color,
    line_col = line_col,
    point_col = point_col,
    lwd = lwd,
    lty = lty,
    pch = pch,
    cex = cex,
    linecolor = linecolor,
    linetype = linetype,
    linewidth = linewidth,
    dashtype = dashtype,
    pointtype = pointtype,
    pointsize = pointsize,
    fill = fill
  )

  data_path <- normalizePath(data_file, winslash = "/", mustWork = FALSE)
  plot_setup <- gp_setup(
    main = main,
    subtitle = subtitle,
    xlab = xlab,
    ylab = ylab,
    grid = grid,
    key = !is.null(legend),
    theme = theme,
    font_family = font_family,
    title_font = title_font,
    title_size = title_size,
    subtitle_font = subtitle_font,
    subtitle_size = subtitle_size,
    axis_font = axis_font,
    axis_size = axis_size,
    tick_font = tick_font,
    tick_size = tick_size,
    legend_font = legend_font,
    legend_size = legend_size,
    legend_position = legend_position,
    legend_box = legend_box,
    legend_cols = legend_cols,
    grid_col = grid_col,
    grid_lty = grid_lty,
    border_lwd = border_lwd,
    border_col = border_col,
    background = background,
    settings = settings,
    extra = extra
  )
  plot_command <- paste(
    "plot",
    gp_quote(data_path),
    "using 1:2",
    "with",
    style_clause(style),
    title_clause(style$title)
  )

  script <- c(
    if (isTRUE(preview)) {
      c(
        paste("set terminal", display_terminal),
        plot_setup,
        plot_command
      )
    },
    if (!is.null(output_path)) {
      c(
        paste("set terminal", output_terminal),
        paste("set output", gp_quote(output_path)),
        plot_setup,
        plot_command,
        "unset output"
      )
    }
  )

  result <- gp_run(
    script = script,
    path = path,
    workdir = workdir,
    echo = echo,
    persist = preview,
    cleanup = cleanup
  )

  invisible(list(
    status = result$status,
    process_output = result$output,
    output = output_path,
    preview = preview,
    preview_terminal = display_terminal,
    data_file = normalizePath(data_file, winslash = "/", mustWork = FALSE),
    data = data,
    script = result$script,
    script_file = result$script_file,
    executable = result$executable
  ))
}

#' @rdname gp_plot
#' @export
gp_lines <- function(x, y = NULL, ...) {
  gp_plot(x = x, y = y, type = "lines", ...)
}

#' @rdname gp_plot
#' @export
gp_points <- function(x, y = NULL, ...) {
  gp_plot(x = x, y = y, type = "points", ...)
}
