#' Create a Plot Theme
#'
#' `gp_theme()` collects common presentation options that are shared by many
#' plotting functions. Numeric color values use R's current palette; character
#' colors can be names such as `"red"` or hex values such as `"#3366cc"`.
#'
#' @param font_family Base font family used when a more specific font is not
#'   supplied.
#' @param title_font,title_size Main title font family and size.
#' @param subtitle_font,subtitle_size Subtitle font family and size.
#' @param axis_font,axis_size Axis label font family and size.
#' @param tick_font,tick_size Tick label font family and size.
#' @param legend_font,legend_size Legend font family and size.
#' @param legend_position Raw gnuplot key position, for example `"top right"`,
#'   `"outside right top"`, or `"bottom center"`.
#' @param legend_box If `TRUE`, draw a box around the legend.
#' @param legend_cols Optional maximum number of legend columns.
#' @param grid_col Optional grid color.
#' @param grid_lty Optional grid dash type.
#' @param border_lwd Optional border line width.
#' @param border_col Optional border color.
#' @param background Optional object background color.
#'
#' @return A `gp_theme` object.
#' @export
gp_theme <- function(font_family = NULL,
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
                     background = NULL) {
  out <- list(
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
  class(out) <- "gp_theme"
  out
}

theme_merge <- function(theme, ...) {
  if (is.null(theme)) {
    theme <- gp_theme()
  }
  if (!inherits(theme, "gp_theme")) {
    stop("`theme` must be created with gp_theme().", call. = FALSE)
  }

  overrides <- list(...)
  for (name in names(overrides)) {
    if (!is.null(overrides[[name]])) {
      theme[[name]] <- overrides[[name]]
    }
  }
  theme
}

theme_commands <- function(theme, key = NULL) {
  if (is.null(theme)) {
    theme <- gp_theme()
  }
  if (!inherits(theme, "gp_theme")) {
    stop("`theme` must be created with gp_theme().", call. = FALSE)
  }

  commands <- character()

  if (!is.null(theme$grid_col) || !is.null(theme$grid_lty)) {
    commands <- c(
      commands,
      paste(
        "set grid",
        if (!is.null(theme$grid_col)) paste("lc", gp_color(theme$grid_col)),
        if (!is.null(theme$grid_lty)) paste("dt", gp_value(theme$grid_lty))
      )
    )
  }

  if (!is.null(theme$border_lwd) || !is.null(theme$border_col)) {
    commands <- c(
      commands,
      paste(
        "set border",
        if (!is.null(theme$border_lwd)) paste("lw", gp_number(theme$border_lwd)),
        if (!is.null(theme$border_col)) paste("lc", gp_color(theme$border_col))
      )
    )
  }

  if (!is.null(theme$background)) {
    commands <- c(
      commands,
      paste("set object 1 rectangle from screen 0,0 to screen 1,1 behind fillcolor",
            gp_color(theme$background), "fillstyle solid 1.0 noborder")
    )
  }

  axis_font <- font_spec(theme$axis_font %||% theme$font_family,
                         theme$axis_size)
  if (!is.null(axis_font)) {
    commands <- c(
      commands,
      paste("set xlabel font", axis_font),
      paste("set ylabel font", axis_font),
      paste("set zlabel font", axis_font),
      paste("set cblabel font", axis_font)
    )
  }

  tick_font <- font_spec(theme$tick_font %||% theme$font_family,
                         theme$tick_size)
  if (!is.null(tick_font)) {
    commands <- c(
      commands,
      paste("set xtics font", tick_font),
      paste("set ytics font", tick_font),
      paste("set ztics font", tick_font),
      paste("set x2tics font", tick_font),
      paste("set y2tics font", tick_font),
      paste("set cbtics font", tick_font)
    )
  }

  if (!identical(key, FALSE)) {
    key_parts <- character()
    if (!is.null(theme$legend_position)) {
      key_parts <- c(key_parts, theme$legend_position)
    }
    if (isTRUE(theme$legend_box)) {
      key_parts <- c(key_parts, "box")
    } else if (identical(theme$legend_box, FALSE)) {
      key_parts <- c(key_parts, "nobox")
    }
    legend_font <- font_spec(theme$legend_font %||% theme$font_family,
                             theme$legend_size)
    if (!is.null(legend_font)) {
      key_parts <- c(key_parts, "font", legend_font)
    }
    if (!is.null(theme$legend_cols)) {
      key_parts <- c(key_parts, "maxcols", gp_number(theme$legend_cols))
    }
    if (length(key_parts) > 0L) {
      commands <- c(commands, paste("set key", paste(key_parts, collapse = " ")))
    }
  }

  commands
}

title_command <- function(main, theme) {
  if (is.null(main)) {
    return(NULL)
  }
  title_font <- font_spec(theme$title_font %||% theme$font_family,
                          theme$title_size)
  paste(
    "set title",
    gp_quote(main),
    if (!is.null(title_font)) paste("font", title_font)
  )
}

subtitle_command <- function(subtitle, theme) {
  if (is.null(subtitle)) {
    return(NULL)
  }
  subtitle_font <- font_spec(theme$subtitle_font %||% theme$font_family,
                             theme$subtitle_size)
  paste(
    "set label 999",
    gp_quote(subtitle),
    "at screen 0.5,0.94 center front",
    if (!is.null(subtitle_font)) paste("font", subtitle_font)
  )
}

font_spec <- function(font = NULL, size = NULL) {
  if (is.null(font) && is.null(size)) {
    return(NULL)
  }
  if (is.null(font)) {
    font <- ""
  }
  if (is.null(size)) {
    return(gp_quote(font))
  }
  gp_quote(paste0(font, ",", gp_number(size)))
}

gp_color <- function(col) {
  if (length(col) != 1L || is.na(col)) {
    stop("Color values must be single non-missing values.", call. = FALSE)
  }
  if (is.numeric(col)) {
    palette <- grDevices::palette()
    idx <- ((as.integer(col) - 1L) %% length(palette)) + 1L
    col <- palette[idx]
  }

  rgb <- tryCatch(grDevices::col2rgb(col), error = function(err) NULL)
  if (is.null(rgb)) {
    return(as.character(col))
  }
  paste0("rgb \"", grDevices::rgb(rgb[1L], rgb[2L], rgb[3L],
                                  maxColorValue = 255), "\"")
}

gp_number <- function(x) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x)) {
    stop("Expected a single numeric value.", call. = FALSE)
  }
  format_gnuplot_number(x)
}

gp_value <- function(x) {
  if (is.numeric(x)) {
    return(gp_number(x))
  }
  validate_scalar_character(x, "value")
}

normalize_fill <- function(fill) {
  if (is.null(fill)) {
    return(NULL)
  }
  if (is.numeric(fill)) {
    return(paste("solid", gp_number(fill)))
  }
  validate_scalar_character(fill, "fill")
}

normalize_border <- function(border) {
  if (is.null(border)) {
    return(NULL)
  }
  if (identical(border, TRUE)) {
    return("border")
  }
  if (identical(border, FALSE)) {
    return("noborder")
  }
  paste("border", validate_scalar_character(border, "border"))
}

normalize_rotate <- function(rotate) {
  if (is.null(rotate)) {
    return(NULL)
  }
  if (identical(rotate, TRUE)) {
    return("rotate")
  }
  if (identical(rotate, FALSE)) {
    return("norotate")
  }
  if (is.numeric(rotate)) {
    return(paste("rotate by", gp_number(rotate)))
  }
  paste("rotate", validate_scalar_character(rotate, "rotate"))
}

normalize_offset <- function(offset) {
  if (is.null(offset)) {
    return(NULL)
  }
  if (is.numeric(offset)) {
    if (length(offset) < 1L || length(offset) > 3L || anyNA(offset)) {
      stop("`offset` must contain one to three numeric values.",
           call. = FALSE)
    }
    return(paste("offset", paste(vapply(offset, gp_number, character(1)),
                                 collapse = ",")))
  }
  paste("offset", validate_scalar_character(offset, "offset"))
}

recycle_gp_arg <- function(x, n, arg) {
  if (is.null(x)) {
    return(vector("list", n))
  }
  if (length(x) == 1L) {
    return(rep(list(x), n))
  }
  if (length(x) != n) {
    stop("`", arg, "` must have length 1 or match the number of series.",
         call. = FALSE)
  }
  as.list(x)
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
