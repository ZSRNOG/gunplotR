gp_render <- function(plot_body,
                      setup = NULL,
                      output = NULL,
                      terminal = NULL,
                      preview = TRUE,
                      preview_terminal = NULL,
                      path = NULL,
                      workdir = tempdir(),
                      echo = FALSE,
                      cleanup = TRUE) {
  if (!is.character(plot_body) || length(plot_body) == 0L) {
    stop("`plot_body` must be a non-empty character vector.", call. = FALSE)
  }
  if (anyNA(plot_body)) {
    stop("`plot_body` cannot contain missing values.", call. = FALSE)
  }
  if (!is.logical(preview) || length(preview) != 1L || is.na(preview)) {
    stop("`preview` must be TRUE or FALSE.", call. = FALSE)
  }

  terminal <- validate_terminal(terminal, "terminal")
  preview_terminal <- validate_terminal(preview_terminal, "preview_terminal")
  setup <- validate_script_lines(setup, "setup")

  workdir <- normalizePath(
    ensure_directory(workdir),
    winslash = "/",
    mustWork = TRUE
  )

  output_path <- normalize_output(output)
  if (!is.null(output_path)) {
    ensure_directory(dirname(output_path))
  }
  if (is.null(output_path) && !isTRUE(preview)) {
    stop("Nothing to draw: use `preview = TRUE` or supply `output`.",
         call. = FALSE)
  }

  display_terminal <- NULL
  if (isTRUE(preview)) {
    display_terminal <- if (is.null(preview_terminal)) {
      interactive_terminal()
    } else {
      preview_terminal
    }
  }

  output_terminal <- NULL
  if (!is.null(output_path)) {
    output_terminal <- if (is.null(terminal)) {
      terminal_from_output(output_path)
    } else {
      terminal
    }
  }

  script <- c(
    if (isTRUE(preview)) {
      c(
        paste("set terminal", display_terminal),
        setup,
        plot_body
      )
    },
    if (!is.null(output_path)) {
      c(
        paste("set terminal", output_terminal),
        paste("set output", gp_quote(output_path)),
        setup,
        plot_body,
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
    workdir = workdir,
    script = result$script,
    script_file = result$script_file,
    executable = result$executable
  ))
}

gp_setup <- function(main = NULL,
                     subtitle = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     zlab = NULL,
                     grid = TRUE,
                     key = NULL,
                     xrange = NULL,
                     yrange = NULL,
                     zrange = NULL,
                     palette = NULL,
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
                     extra = NULL) {
  extra <- validate_script_lines(extra, "extra")
  settings <- validate_script_lines(settings, "settings")
  theme <- theme_merge(
    theme,
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
    background = background
  )

  c(
    if (isTRUE(grid)) "set grid",
    if (identical(grid, FALSE)) "unset grid",
    if (!is.null(key)) {
      if (isTRUE(key)) "set key" else "unset key"
    },
    theme_commands(theme, key = key),
    range_command("xrange", xrange),
    range_command("yrange", yrange),
    range_command("zrange", zrange),
    title_command(main, theme),
    subtitle_command(subtitle, theme),
    if (!is.null(xlab)) paste("set xlabel", gp_quote(xlab)),
    if (!is.null(ylab)) paste("set ylabel", gp_quote(ylab)),
    if (!is.null(zlab)) paste("set zlabel", gp_quote(zlab)),
    if (!is.null(palette)) paste("set palette", palette),
    settings,
    extra
  )
}

validate_script_lines <- function(lines, arg) {
  if (is.null(lines)) {
    return(NULL)
  }
  if (!is.character(lines)) {
    stop("`", arg, "` must be a character vector.", call. = FALSE)
  }
  if (anyNA(lines)) {
    stop("`", arg, "` cannot contain missing values.", call. = FALSE)
  }
  lines
}

range_command <- function(name, range) {
  if (is.null(range)) {
    return(NULL)
  }
  if (!is.numeric(range) || length(range) != 2L || anyNA(range)) {
    stop("`", name, "` must be a numeric vector of length 2.", call. = FALSE)
  }
  paste0("set ", name, " [", format_gnuplot_number(range[1L]), ":",
         format_gnuplot_number(range[2L]), "]")
}

format_gnuplot_number <- function(x) {
  format(x, scientific = FALSE, trim = TRUE, digits = 15)
}

plot_title_clause <- function(legend, default = NULL) {
  if (identical(legend, FALSE) || is.null(legend)) {
    return("notitle")
  }
  if (identical(legend, TRUE)) {
    legend <- default
  }
  if (length(legend) != 1L || is.na(legend)) {
    stop("`legend` must be NULL, TRUE, FALSE, or a single label.",
         call. = FALSE)
  }
  paste("title", gp_quote(legend))
}

write_plot_table <- function(data,
                             workdir,
                             prefix = "gunplotR-data-",
                             quote = FALSE) {
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- tempfile(prefix, tmpdir = workdir, fileext = ".dat")
  utils::write.table(
    data,
    file = data_file,
    sep = "\t",
    row.names = FALSE,
    col.names = FALSE,
    quote = quote,
    na = "NaN"
  )
  normalizePath(data_file, winslash = "/", mustWork = FALSE)
}

write_grid_table <- function(data,
                             workdir,
                             prefix = "gunplotR-grid-") {
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- tempfile(prefix, tmpdir = workdir, fileext = ".dat")
  y_values <- unique(data$y)
  lines <- character()

  for (value in y_values) {
    chunk <- data[data$y == value, , drop = FALSE]
    chunk_lines <- apply(chunk, 1L, function(row) {
      paste(format_gnuplot_number(as.numeric(row)), collapse = "\t")
    })
    lines <- c(lines, chunk_lines, "")
  }

  writeLines(lines, data_file, useBytes = TRUE)
  normalizePath(data_file, winslash = "/", mustWork = FALSE)
}

matrix_to_xyz <- function(z, x = NULL, y = NULL) {
  if (is.data.frame(z)) {
    if (ncol(z) < 3L) {
      stop("A data frame `z` must have at least three columns: x, y, z.",
           call. = FALSE)
    }
    out <- data.frame(x = z[[1L]], y = z[[2L]], z = z[[3L]])
  } else if (is.matrix(z)) {
    if (!is.numeric(z)) {
      stop("Matrix `z` must be numeric.", call. = FALSE)
    }
    if (is.null(x)) {
      x <- seq_len(ncol(z))
    }
    if (is.null(y)) {
      y <- seq_len(nrow(z))
    }
    if (length(x) != ncol(z)) {
      stop("`x` length must match the number of matrix columns.",
           call. = FALSE)
    }
    if (length(y) != nrow(z)) {
      stop("`y` length must match the number of matrix rows.", call. = FALSE)
    }
    grid <- expand.grid(x = x, y = y)
    out <- data.frame(x = grid$x, y = grid$y, z = as.vector(t(z)))
  } else {
    stop("`z` must be a numeric matrix or a data frame with x, y, z columns.",
         call. = FALSE)
  }

  if (!is.numeric(out$x) || !is.numeric(out$y) || !is.numeric(out$z)) {
    stop("x, y, and z values must be numeric.", call. = FALSE)
  }
  if (nrow(out) == 0L) {
    stop("Plot data must contain at least one row.", call. = FALSE)
  }

  out
}

function_to_xyz <- function(fun, x = NULL, y = NULL, n = 60) {
  if (is.null(x)) {
    x <- seq(-10, 10, length.out = n)
  }
  if (is.null(y)) {
    y <- seq(-10, 10, length.out = n)
  }
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("`x` and `y` grid values must be numeric.", call. = FALSE)
  }

  grid <- expand.grid(x = x, y = y)
  values <- tryCatch(fun(grid$x, grid$y), error = function(err) NULL)
  if (is.null(values) || length(values) != nrow(grid)) {
    values <- mapply(fun, grid$x, grid$y)
  }
  if (!is.numeric(values)) {
    stop("The function must return numeric z values.", call. = FALSE)
  }

  data.frame(x = grid$x, y = grid$y, z = as.numeric(values))
}
