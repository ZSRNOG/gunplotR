#' Fence Plot
#'
#' Draw a 3D fence plot. Matrix rows are separate fences along the y axis;
#' columns are x positions and values are z heights.
#'
#' @param z Numeric matrix, vector, data frame with x/y/z columns, or an R
#'   function `function(x, y)`.
#' @param x Optional x positions for matrix or function input.
#' @param y Optional y positions for matrix or function input.
#' @param n Grid size used when `z` is a function.
#' @param type `"zerrorfill"` for filled fences or `"lines"` for line fences.
#' @param baseline Fence baseline.
#' @param fill Fill opacity or raw fill style.
#' @param col,lwd,lty R-like color, line width, and line type aliases.
#' @param depthorder If `TRUE`, use `set pm3d depthorder` to depth-sort
#'   filled fences.
#' @inheritParams gp_surface
#' @inheritParams gp_3d_setup
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- seq(-5, 5, length.out = 60)
#' y <- seq(-4, 4, length.out = 9)
#' z <- outer(y, x, function(y, x) sin(sqrt(x^2 + y^2)) /
#'   sqrt(x^2 + y^2))
#' gp_fenceplot(z, x = x, y = y, main = "Fence plot")
#' }
gp_fenceplot <- function(z,
                         x = NULL,
                         y = NULL,
                         n = 60,
                         type = c("zerrorfill", "lines"),
                         baseline = NULL,
                         fill = 0.65,
                         col = 4,
                         lwd = NULL,
                         lty = NULL,
                         xrange = NULL,
                         yrange = NULL,
                         zrange = NULL,
                         main = NULL,
                         subtitle = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         zlab = NULL,
                         grid = TRUE,
                         view = c(70, 25),
                         hidden3d = NULL,
                         pm3d = NULL,
                         depthorder = FALSE,
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
  type <- match.arg(type)
  data <- fence_data(z, x = x, y = y, n = n, baseline = baseline)
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- write_grouped_table(data[setdiff(names(data), "group")],
                                   group = data$group, workdir = workdir,
                                   prefix = "gunplotR-fence-")
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }

  style <- if (type == "zerrorfill") {
    gp_style("zerrorfill", using = "1:2:3:4:5", fill = fill, col = col,
             lwd = lwd, lty = lty)
  } else {
    gp_style("lines", using = "1:2:5", col = col, lwd = lwd, lty = lty)
  }
  gp_render(
    plot_body = paste("splot", layer_term(gp_quote(data_file), style)),
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid, key = FALSE,
                        xrange = xrange, yrange = yrange, zrange = zrange,
                        view = view, hidden3d = hidden3d,
                        pm3d = if (isTRUE(depthorder) && is.null(pm3d)) {
                          "depthorder"
                        } else {
                          pm3d
                        },
                        theme = theme, extra = c(
                          if (type == "zerrorfill") {
                            paste("set style fill", normalize_fill(fill),
                                  "noborder")
                          },
                          extra
                        )),
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

#' Gantt Chart
#'
#' Draw a simple Gantt chart using gnuplot time axes and vector arrows.
#'
#' @param data Optional data frame. If supplied, `task`, `start`, and `end`
#'   may be column names.
#' @param task Task labels or column name.
#' @param start,end Start and end dates or column names.
#' @param date_format Gnuplot time format used for the data file.
#' @param x_format Gnuplot format used for x-axis labels.
#' @param show_start_labels If `TRUE`, show task labels near the start date.
#' @param arrow_style Raw `set style arrow 1` value.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' tasks <- data.frame(
#'   task = c("A", "B", "C"),
#'   start = as.Date(c("2026-01-01", "2026-02-01", "2026-03-15")),
#'   end = as.Date(c("2026-01-31", "2026-03-01", "2026-04-15"))
#' )
#' gp_gantt(tasks, task = "task", start = "start", end = "end")
#' }
gp_gantt <- function(data = NULL,
                     task,
                     start,
                     end,
                     date_format = "%Y-%m-%d",
                     x_format = "%b\\n'%y",
                     show_start_labels = TRUE,
                     arrow_style = "filled size screen 0.02,15 fixed lt 3 lw 1.5",
                     main = NULL,
                     subtitle = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     grid = TRUE,
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
  gantt <- gantt_data(data = data, task = task, start = start, end = end,
                      date_format = date_format)
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- write_gantt_table(gantt, workdir = workdir)
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }
  y_range <- c(-1, nrow(gantt))
  body <- paste(
    "plot",
    gp_quote(data_file),
    "using (timecolumn(2,timeformat)):($0):(timecolumn(3,timeformat)-timecolumn(2,timeformat)):(0.0):yticlabel(1)",
    "with vectors as 1 notitle",
    if (isTRUE(show_start_labels)) {
      paste(", \\\n     '' using (timecolumn(2,timeformat)):($0):1",
            "with labels right offset -2 notitle")
    }
  )
  gp_render(
    plot_body = body,
    setup = c(
      gp_setup(main = main, subtitle = subtitle, xlab = xlab, ylab = ylab,
               grid = grid, key = FALSE, theme = theme, yrange = y_range),
      "set xdata time",
      paste("timeformat =", gp_string_quote(date_format)),
      paste("set timefmt", gp_string_quote(date_format)),
      paste("set format x", gp_string_quote(x_format)),
      "set xtics nomirror",
      "set ytics nomirror",
      "set border 3",
      paste("set style arrow 1", arrow_style),
      extra
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

#' Violin Plot
#'
#' Draw a violin plot from grouped numeric data. The violin is computed in R
#' with `stats::density()` and drawn with mirrored filled curves. Optional
#' jittered points and boxplots can be overlaid.
#'
#' @param value Numeric values or a data frame.
#' @param group Group labels or column name when `value` is a data frame.
#' @param data Optional data frame.
#' @param width Maximum half-width of each violin.
#' @param adjust Bandwidth adjustment passed to `stats::density()`.
#' @param n Number of density points.
#' @param points If `TRUE`, overlay jittered points.
#' @param boxplot If `TRUE`, overlay a boxplot.
#' @param jitter Width of random jitter for points.
#' @param seed Optional random seed for jitter.
#' @param fill Fill opacity or raw fill style.
#' @param col Violin color.
#' @inheritParams gp_plot
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' gp_violinplot(iris, group = "Species", value = "Sepal.Length",
#'               points = TRUE, boxplot = TRUE)
#' }
gp_violinplot <- function(data = NULL,
                          group,
                          value = NULL,
                          width = 0.4,
                          adjust = 1,
                          n = 256,
                          points = FALSE,
                          boxplot = TRUE,
                          jitter = 0.08,
                          seed = NULL,
                          fill = 0.45,
                          col = 4,
                          main = NULL,
                          subtitle = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          grid = TRUE,
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
  violin <- violin_data(data = data, group = group, value = value,
                        width = width, adjust = adjust, n = n,
                        jitter = jitter, seed = seed)
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  files_to_remove <- character()
  on.exit(if (isTRUE(cleanup)) unlink(files_to_remove), add = TRUE)
  density_file <- write_plot_table(violin$density, workdir = workdir,
                                   prefix = "gunplotR-violin-density-")
  raw_file <- write_plot_table(violin$raw, workdir = workdir,
                               prefix = "gunplotR-violin-raw-")
  files_to_remove <- c(density_file, raw_file)

  terms <- character()
  for (i in seq_along(violin$groups)) {
    terms <- c(
      terms,
      paste(gp_quote(density_file), "using",
            paste0("(($3==", i, ")?$1:1/0):2"),
            "with filledcurves x=", i,
            "lc", gp_color(col), "fs", normalize_fill(fill), "notitle"),
      paste(gp_quote(density_file), "using",
            paste0("(($3==", i, ")?$4:1/0):2"),
            "with filledcurves x=", i,
            "lc", gp_color(col), "fs", normalize_fill(fill), "notitle")
    )
  }
  if (isTRUE(points)) {
    terms <- c(terms, paste(gp_quote(raw_file),
                            "using 3:2 with points pt 7 ps 0.35",
                            "lc rgb \"#333333\" notitle"))
  }
  if (isTRUE(boxplot)) {
    terms <- c(terms, paste(gp_quote(raw_file),
                            "using 1:2 with boxplot fc rgb \"white\"",
                            "lw 1 notitle"))
  }
  body <- paste("plot", paste(terms, collapse = ", \\\n     "))
  gp_render(
    plot_body = body,
    setup = c(
      gp_setup(main = main, subtitle = subtitle, xlab = xlab, ylab = ylab,
               grid = grid, key = FALSE, theme = theme, extra = NULL),
      "set border 2",
      "set xtics nomirror scale 0",
      "set ytics nomirror",
      paste0("set xtics (", paste(paste(gp_quote(violin$groups),
                                       seq_along(violin$groups)),
                                  collapse = ", "), ")"),
      "set style fill solid border -1",
      "set style boxplot",
      "set boxwidth 0.08",
      extra
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

#' Waterfall Plot
#'
#' Draw a 3D waterfall plot from a matrix, vector, data frame, or R function.
#' Matrix rows are scan lines, matrix columns are x positions.
#'
#' @param z Numeric matrix, vector, data frame with x/y/z columns, or R
#'   function `function(x, y)`.
#' @param x Optional x positions for matrix or function input.
#' @param y Optional scan positions for matrix or function input.
#' @param n Grid size used when `z` is a function.
#' @param fill Fill color. Use `"background"` to mimic the gnuplot demo.
#' @param col Line color.
#' @param scan_order `"back_to_front"` draws high y values first.
#' @inheritParams gp_surface
#' @inheritParams gp_3d_setup
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- seq(0, 1, length.out = 100)
#' y <- seq(1, 40)
#' z <- outer(y, x, function(y, x) dnorm(x, 0.2 + y / 400, 0.05))
#' gp_waterfall(z, x = x, y = y, main = "Waterfall plot")
#' }
gp_waterfall <- function(z,
                         x = NULL,
                         y = NULL,
                         n = 80,
                         fill = "background",
                         col = "black",
                         scan_order = c("back_to_front", "front_to_back"),
                         xrange = NULL,
                         yrange = NULL,
                         zrange = NULL,
                         main = NULL,
                         subtitle = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         zlab = NULL,
                         grid = TRUE,
                         view = c(42, 27, 1, 1.2),
                         xyplane = 0,
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
  scan_order <- match.arg(scan_order)
  data <- waterfall_data(z, x = x, y = y, n = n)
  if (scan_order == "back_to_front") {
    data <- data[order(-data$y, data$x), , drop = FALSE]
  } else {
    data <- data[order(data$y, data$x), , drop = FALSE]
  }
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- write_grouped_table(data[, c("x", "y", "z")],
                                   group = data$y, workdir = workdir,
                                   prefix = "gunplotR-waterfall-")
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }
  fill_clause <- if (identical(fill, "background")) {
    "fc background"
  } else {
    paste("fc", gp_color(fill))
  }
  body <- paste("splot", gp_quote(data_file),
                "using 1:2:3 with filledcurves",
                "lc", gp_color(col), fill_clause, "notitle")
  gp_render(
    plot_body = body,
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid, key = FALSE,
                        xrange = xrange, yrange = yrange, zrange = zrange,
                        view = view, xyplane = xyplane, theme = theme,
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

fence_data <- function(z, x = NULL, y = NULL, n = 60, baseline = NULL) {
  xyz <- surface_like_xyz(z, x = x, y = y, n = n)
  if (is.null(baseline)) {
    finite_z <- xyz$z[is.finite(xyz$z)]
    baseline <- if (length(finite_z) == 0L) 0 else min(finite_z)
  }
  xyz$zmin <- baseline
  xyz$zlow <- baseline
  xyz$group <- xyz$y
  xyz[, c("x", "y", "zmin", "zlow", "z", "group")]
}

waterfall_data <- function(z, x = NULL, y = NULL, n = 80) {
  xyz <- surface_like_xyz(z, x = x, y = y, n = n)
  xyz[, c("x", "y", "z")]
}

surface_like_xyz <- function(z, x = NULL, y = NULL, n = 60) {
  if (is.function(z)) {
    return(function_to_xyz(z, x = x, y = y, n = n))
  }
  if (is.data.frame(z)) {
    return(matrix_to_xyz(z))
  }
  matrix_to_xyz(z, x = x, y = y)
}

gantt_data <- function(data = NULL, task, start, end, date_format) {
  if (!is.null(data)) {
    task <- extract_column(data, task, "task")
    start <- extract_column(data, start, "start")
    end <- extract_column(data, end, "end")
  }
  if (length(task) != length(start) || length(task) != length(end)) {
    stop("`task`, `start`, and `end` must have the same length.",
         call. = FALSE)
  }
  start <- as.Date(start)
  end <- as.Date(end)
  if (anyNA(start) || anyNA(end)) {
    stop("`start` and `end` must be coercible to Date.", call. = FALSE)
  }
  if (any(end < start)) {
    stop("Every `end` date must be on or after its `start` date.",
         call. = FALSE)
  }
  data.frame(task = as.character(task),
             start = format(start, date_format),
             end = format(end, date_format),
             stringsAsFactors = FALSE)
}

write_gantt_table <- function(data, workdir,
                              prefix = "gunplotR-gantt-") {
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- tempfile(prefix, tmpdir = workdir, fileext = ".dat")
  lines <- paste(gp_quote(data$task), data$start, data$end, sep = "\t")
  writeLines(lines, data_file, useBytes = TRUE)
  normalizePath(data_file, winslash = "/", mustWork = FALSE)
}

violin_data <- function(data = NULL, group, value = NULL, width = 0.4,
                        adjust = 1, n = 256, jitter = 0.08, seed = NULL) {
  if (!is.null(data)) {
    if (is.null(value)) {
      stop("`value` must be supplied when `data` is used.", call. = FALSE)
    }
    group <- extract_column(data, group, "group")
    value <- extract_column(data, value, "value")
  }
  if (!is.numeric(value) || length(value) == 0L) {
    stop("`value` must be a non-empty numeric vector.", call. = FALSE)
  }
  if (length(group) != length(value)) {
    stop("`group` and `value` must have the same length.", call. = FALSE)
  }
  keep <- !is.na(group) & !is.na(value)
  group <- as.character(group[keep])
  value <- value[keep]
  groups <- unique(group)
  density_rows <- list()
  raw_rows <- list()
  if (!is.null(seed)) {
    set.seed(seed)
  }
  for (i in seq_along(groups)) {
    values <- value[group == groups[i]]
    if (length(values) < 2L) {
      values <- rep(values, 2L)
    }
    dens <- stats::density(values, adjust = adjust, n = n)
    scale <- if (max(dens$y) > 0) width / max(dens$y) else 0
    half <- dens$y * scale
    density_rows[[i]] <- data.frame(
      right = i + half,
      y = dens$x,
      group = i,
      left = i - half
    )
    raw_rows[[i]] <- data.frame(
      x = i,
      y = values,
      jitter = i + stats::runif(length(values), -jitter, jitter),
      group = i
    )
  }
  list(density = do.call(rbind, density_rows),
       raw = do.call(rbind, raw_rows),
       groups = groups)
}

extract_column <- function(data, column, arg) {
  if (is.character(column) && length(column) == 1L && column %in% names(data)) {
    return(data[[column]])
  }
  if (length(column) == nrow(data)) {
    return(column)
  }
  stop("`", arg, "` must be a column name or a vector matching nrow(data).",
       call. = FALSE)
}

walls_command <- function(walls) {
  if (is.null(walls)) {
    return(NULL)
  }
  if (identical(walls, FALSE)) {
    return("unset walls")
  }
  if (identical(walls, TRUE)) {
    return("set walls z0")
  }
  walls <- validate_scalar_character(walls, "walls")
  paste("set walls", strsplit(walls, "[[:space:]]+")[[1L]])
}
