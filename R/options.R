#' Build Gnuplot Setup Commands from R-Style Options
#'
#' `gp_options()` converts common gnuplot `set` commands into R-friendly
#' arguments. It is intended for broad coverage of the gnuplot reference
#' manual and demo gallery without needing a separate R function for every
#' command. Use the returned character vector through `settings =` in core
#' plotting functions or `extra =` in any wrapper.
#'
#' @param xlim,ylim,zlim,cblim R-style aliases for `set xrange`, `set yrange`,
#'   `set zrange`, and `set cbrange`.
#' @param xrange,yrange,zrange,cbrange,x2range,y2range,rrange,trange,urange,vrange
#'   Numeric ranges of length two.
#' @param xlog,ylog,zlog,cblog Logical values controlling per-axis log scale.
#' @param log Raw axis string passed to `set logscale`, such as `"xy"` or
#'   `"xyzcb"`.
#' @param xformat,yformat,zformat,cbformat,x2format,y2format Axis format
#'   strings. Backslash escapes are preserved.
#' @param xtics,ytics,ztics,cbtics,x2tics,y2tics Tic specifications. Use
#'   `TRUE`/`FALSE`, a raw string, numeric values, or a named numeric vector.
#' @param mxtics,mytics,mztics,mcbtics Minor tic settings. Use `TRUE`/`FALSE`,
#'   a number, or a raw string.
#' @param xdata,ydata,zdata,cdata Axis data type, for example `"time"`.
#' @param timefmt Time parsing format.
#' @param x2lab,y2lab,cblab Axis labels for secondary axes and color box.
#' @param grid,border,key,zeroaxis,xzeroaxis,yzeroaxis Logical or raw gnuplot
#'   options.
#' @param size,origin Numeric vectors of length two or raw strings.
#' @param square If `TRUE`, use `set size square`; if `FALSE`, use
#'   `set size nosquare`.
#' @param ratio Optional plot ratio passed to `set size ratio`.
#' @param margins Numeric vector `c(left, right, bottom, top)` or raw string.
#' @param lmargin,rmargin,bmargin,tmargin Individual margin settings.
#' @param encoding,termoption Raw values for `set encoding` and
#'   `set termoption`.
#' @param samples,isosamples Numeric sample counts.
#' @param angles,mapping Raw values such as `"degrees"` or `"spherical"`.
#' @param polar,parametric Logical values controlling polar/parametric mode.
#' @param palette,colorbox,pm3d,hidden3d,contour,surface,dgrid3d,view,xyplane
#'   Common 3D, palette, and surface controls.
#' @param boxwidth,boxdepth,errorbars,pointintervalbox Box, errorbar, and point
#'   interval settings.
#' @param style_fill,style_data,style_function,style_histogram,style_boxplot
#'   Raw gnuplot style settings.
#' @param datafile_separator Separator passed to `set datafile separator`.
#' @param linetype,linestyle,arrowstyle Character vectors returned by
#'   `gp_linetype()`, `gp_linestyle()`, or `gp_arrowstyle()`.
#' @param label,arrow,object Raw `set label`, `set arrow`, and `set object`
#'   commands without the leading `set`.
#' @param set Named list of additional `set <name> <value>` commands.
#' @param unset Character vector of additional commands to unset.
#' @param raw Raw script lines appended unchanged.
#' @param ... Additional named gnuplot `set` commands. For example
#'   `gp_options(mouse = TRUE, autoscale = "xy")`.
#'
#' @return A character vector of gnuplot setup commands.
#' @export
#'
#' @examples
#' gp_options(
#'   xlog = TRUE,
#'   xtics = c(A = 1, B = 2, C = 3),
#'   yformat = "%.2f",
#'   style_fill = "solid 0.4 border lc black",
#'   set = list(encoding = "utf8")
#' )
gp_options <- function(xlim = NULL,
                       ylim = NULL,
                       zlim = NULL,
                       cblim = NULL,
                       xrange = NULL,
                       yrange = NULL,
                       zrange = NULL,
                       cbrange = NULL,
                       x2range = NULL,
                       y2range = NULL,
                       rrange = NULL,
                       trange = NULL,
                       urange = NULL,
                       vrange = NULL,
                       xlog = NULL,
                       ylog = NULL,
                       zlog = NULL,
                       cblog = NULL,
                       log = NULL,
                       xformat = NULL,
                       yformat = NULL,
                       zformat = NULL,
                       cbformat = NULL,
                       x2format = NULL,
                       y2format = NULL,
                       xtics = NULL,
                       ytics = NULL,
                       ztics = NULL,
                       cbtics = NULL,
                       x2tics = NULL,
                       y2tics = NULL,
                       mxtics = NULL,
                       mytics = NULL,
                       mztics = NULL,
                       mcbtics = NULL,
                       xdata = NULL,
                       ydata = NULL,
                       zdata = NULL,
                       cdata = NULL,
                       timefmt = NULL,
                       x2lab = NULL,
                       y2lab = NULL,
                       cblab = NULL,
                       grid = NULL,
                       border = NULL,
                       key = NULL,
                       zeroaxis = NULL,
                       xzeroaxis = NULL,
                       yzeroaxis = NULL,
                       size = NULL,
                       origin = NULL,
                       square = NULL,
                       ratio = NULL,
                       margins = NULL,
                       lmargin = NULL,
                       rmargin = NULL,
                       bmargin = NULL,
                       tmargin = NULL,
                       encoding = NULL,
                       termoption = NULL,
                       samples = NULL,
                       isosamples = NULL,
                       angles = NULL,
                       mapping = NULL,
                       polar = NULL,
                       parametric = NULL,
                       palette = NULL,
                       colorbox = NULL,
                       pm3d = NULL,
                       hidden3d = NULL,
                       contour = NULL,
                       surface = NULL,
                       dgrid3d = NULL,
                       view = NULL,
                       xyplane = NULL,
                       boxwidth = NULL,
                       boxdepth = NULL,
                       errorbars = NULL,
                       pointintervalbox = NULL,
                       style_fill = NULL,
                       style_data = NULL,
                       style_function = NULL,
                       style_histogram = NULL,
                       style_boxplot = NULL,
                       datafile_separator = NULL,
                       linetype = NULL,
                       linestyle = NULL,
                       arrowstyle = NULL,
                       label = NULL,
                       arrow = NULL,
                       object = NULL,
                       set = NULL,
                       unset = NULL,
                       raw = NULL,
                       ...) {
  dots <- list(...)
  if (length(dots) > 0L) {
    names_ok <- !is.null(names(dots)) && all(nzchar(names(dots)))
    if (!names_ok) {
      stop("Additional `gp_options()` arguments must be named.",
           call. = FALSE)
    }
  }

  xrange <- xrange %||% xlim
  yrange <- yrange %||% ylim
  zrange <- zrange %||% zlim
  cbrange <- cbrange %||% cblim

  commands <- c(
    gp_range_command("xrange", xrange),
    gp_range_command("yrange", yrange),
    gp_range_command("zrange", zrange),
    gp_range_command("cbrange", cbrange),
    gp_range_command("x2range", x2range),
    gp_range_command("y2range", y2range),
    gp_range_command("rrange", rrange),
    gp_range_command("trange", trange),
    gp_range_command("urange", urange),
    gp_range_command("vrange", vrange),
    gp_log_commands(log = log, x = xlog, y = ylog, z = zlog, cb = cblog),
    gp_format_command("x", xformat),
    gp_format_command("y", yformat),
    gp_format_command("z", zformat),
    gp_format_command("cb", cbformat),
    gp_format_command("x2", x2format),
    gp_format_command("y2", y2format),
    gp_tics_command("xtics", xtics),
    gp_tics_command("ytics", ytics),
    gp_tics_command("ztics", ztics),
    gp_tics_command("cbtics", cbtics),
    gp_tics_command("x2tics", x2tics),
    gp_tics_command("y2tics", y2tics),
    gp_set_unset_command("mxtics", mxtics),
    gp_set_unset_command("mytics", mytics),
    gp_set_unset_command("mztics", mztics),
    gp_set_unset_command("mcbtics", mcbtics),
    gp_set_value_command("xdata", xdata),
    gp_set_value_command("ydata", ydata),
    gp_set_value_command("zdata", zdata),
    gp_set_value_command("cdata", cdata),
    if (!is.null(timefmt)) paste("set timefmt", gp_string_quote(timefmt)),
    if (!is.null(x2lab)) paste("set x2label", gp_string_quote(x2lab)),
    if (!is.null(y2lab)) paste("set y2label", gp_string_quote(y2lab)),
    if (!is.null(cblab)) paste("set cblabel", gp_string_quote(cblab)),
    gp_set_unset_command("grid", grid),
    gp_set_unset_command("border", border),
    gp_set_unset_command("key", key),
    gp_set_unset_command("zeroaxis", zeroaxis),
    gp_set_unset_command("xzeroaxis", xzeroaxis),
    gp_set_unset_command("yzeroaxis", yzeroaxis),
    gp_size_command(size = size, square = square, ratio = ratio),
    gp_pair_command("origin", origin),
    gp_margins_command(margins),
    gp_margin_command("lmargin", lmargin),
    gp_margin_command("rmargin", rmargin),
    gp_margin_command("bmargin", bmargin),
    gp_margin_command("tmargin", tmargin),
    gp_set_value_command("encoding", encoding),
    gp_set_value_command("termoption", termoption),
    sample_command("samples", samples),
    sample_command("isosamples", isosamples),
    gp_set_value_command("angles", angles),
    gp_set_value_command("mapping", mapping),
    gp_set_unset_command("polar", polar),
    gp_set_unset_command("parametric", parametric),
    if (!is.null(palette)) paste("set palette", validate_scalar_character(palette, "palette")),
    colorbox_command(colorbox),
    pm3d_command(pm3d),
    flag_command("hidden3d", hidden3d),
    contour_command(contour),
    flag_command("surface", surface),
    dgrid3d_command(dgrid3d),
    view_command(view),
    xyplane_command(xyplane),
    gp_set_value_command("boxwidth", boxwidth),
    gp_set_value_command("boxdepth", boxdepth),
    gp_set_unset_command("errorbars", errorbars),
    gp_set_value_command("pointintervalbox", pointintervalbox),
    gp_style_setting_command("fill", style_fill),
    gp_style_setting_command("data", style_data),
    gp_style_setting_command("function", style_function),
    gp_style_setting_command("histogram", style_histogram),
    gp_style_setting_command("boxplot", style_boxplot),
    if (!is.null(datafile_separator)) {
      paste("set datafile separator", gp_string_quote(datafile_separator))
    },
    validate_script_lines(linetype, "linetype"),
    validate_script_lines(linestyle, "linestyle"),
    validate_script_lines(arrowstyle, "arrowstyle"),
    gp_prefixed_commands("label", label),
    gp_prefixed_commands("arrow", arrow),
    gp_prefixed_commands("object", object),
    gp_named_set_commands(set),
    gp_named_set_commands(dots),
    if (!is.null(unset)) paste("unset", validate_script_lines(unset, "unset")),
    validate_script_lines(raw, "raw")
  )
  commands[!is.na(commands) & nzchar(commands)]
}

#' Build a Gnuplot Terminal Specification
#'
#' @param type Terminal name, for example `"pngcairo"`, `"pdfcairo"`,
#'   `"svg"`, `"qt"`, or `"windows"`.
#' @param width,height Optional canvas size.
#' @param font Optional font family.
#' @param font_size Optional font size.
#' @param background Optional background color.
#' @param enhanced If `TRUE`, add `enhanced`; if `FALSE`, add ` noenhanced`.
#' @param extra Raw terminal options appended unchanged.
#'
#' @return A terminal specification suitable for the `terminal` argument.
#' @export
#'
#' @examples
#' gp_terminal("pngcairo", width = 1200, height = 800,
#'             font = "Arial", font_size = 12)
gp_terminal <- function(type = "pngcairo",
                        width = NULL,
                        height = NULL,
                        font = NULL,
                        font_size = NULL,
                        background = NULL,
                        enhanced = NULL,
                        extra = NULL) {
  type <- validate_scalar_character(type, "type")
  if (!is.null(width) || !is.null(height)) {
    if (is.null(width) || is.null(height)) {
      stop("Both `width` and `height` are required when setting terminal size.",
           call. = FALSE)
    }
    if (!is.numeric(width) || !is.numeric(height) || length(width) != 1L ||
        length(height) != 1L || anyNA(c(width, height))) {
      stop("`width` and `height` must be single numeric values.",
           call. = FALSE)
    }
  }
  parts <- c(
    type,
    if (!is.null(width)) paste("size", paste(gp_number(width),
                                             gp_number(height), sep = ",")),
    if (!is.null(font) || !is.null(font_size)) paste("font", font_spec(font, font_size)),
    if (!is.null(background)) paste("background", gp_color(background)),
    if (isTRUE(enhanced)) "enhanced",
    if (identical(enhanced, FALSE)) "noenhanced",
    validate_script_lines(extra, "extra")
  )
  paste(parts, collapse = " ")
}

#' Build Linetype, Linestyle, and Arrowstyle Commands
#'
#' These helpers map R-style color and line arguments to reusable gnuplot
#' styles. Use them in `gp_options(linetype = ..., linestyle = ...,
#' arrowstyle = ...)` or directly in `extra`.
#'
#' @param index Style index.
#' @param col,color Line color.
#' @param lwd,lty,pch,cex R-style aliases for width, dash, point type, and
#'   point size.
#' @param raw Raw tokens appended unchanged.
#'
#' @return A single gnuplot command.
#' @name gp_style_commands
NULL

#' @rdname gp_style_commands
#' @export
gp_linetype <- function(index,
                        col = NULL,
                        color = NULL,
                        lwd = NULL,
                        lty = NULL,
                        pch = NULL,
                        cex = NULL,
                        raw = NULL) {
  paste(c("set linetype", gp_value(index),
          gp_line_property_tokens(col = color %||% col, lwd = lwd,
                                  lty = lty, pch = pch, cex = cex),
          validate_script_lines(raw, "raw")), collapse = " ")
}

#' @rdname gp_style_commands
#' @export
gp_linestyle <- function(index,
                         col = NULL,
                         color = NULL,
                         lwd = NULL,
                         lty = NULL,
                         pch = NULL,
                         cex = NULL,
                         raw = NULL) {
  paste(c("set style line", gp_value(index),
          gp_line_property_tokens(col = color %||% col, lwd = lwd,
                                  lty = lty, pch = pch, cex = cex),
          validate_script_lines(raw, "raw")), collapse = " ")
}

#' @rdname gp_style_commands
#' @param head If `TRUE`, use `head`; if `FALSE`, use `nohead`.
#' @param filled If `TRUE`, use `filled`; if `FALSE`, use `empty`.
#' @param size Optional arrow head size. Use a raw string for full control.
#' @export
gp_arrowstyle <- function(index,
                          head = TRUE,
                          filled = NULL,
                          col = NULL,
                          color = NULL,
                          lwd = NULL,
                          lty = NULL,
                          size = NULL,
                          raw = NULL) {
  head_token <- if (isTRUE(head)) "head" else if (identical(head, FALSE)) "nohead"
  filled_token <- if (isTRUE(filled)) {
    "filled"
  } else if (identical(filled, FALSE)) {
    "empty"
  }
  size_token <- if (is.null(size)) {
    NULL
  } else if (is.numeric(size)) {
    paste("size screen", gp_number(size))
  } else {
    paste("size", validate_scalar_character(size, "size"))
  }
  paste(c("set style arrow", gp_value(index), head_token, filled_token,
          size_token,
          gp_line_property_tokens(col = color %||% col, lwd = lwd,
                                  lty = lty),
          validate_script_lines(raw, "raw")), collapse = " ")
}

gp_line_property_tokens <- function(col = NULL,
                                    lwd = NULL,
                                    lty = NULL,
                                    pch = NULL,
                                    cex = NULL) {
  c(
    if (!is.null(col)) paste("lc", gp_color(col)),
    if (!is.null(lwd)) paste("lw", gp_value(lwd)),
    if (!is.null(lty)) paste("dt", gp_value(lty)),
    if (!is.null(pch)) paste("pt", gp_value(pch)),
    if (!is.null(cex)) paste("ps", gp_value(cex))
  )
}

gp_range_command <- function(name, range) {
  if (is.null(range)) {
    return(NULL)
  }
  range_command(name, range)
}

gp_log_commands <- function(log = NULL, x = NULL, y = NULL, z = NULL,
                            cb = NULL) {
  commands <- character()
  if (!is.null(log)) {
    if (identical(log, FALSE)) {
      commands <- c(commands, "unset logscale")
    } else if (identical(log, TRUE)) {
      commands <- c(commands, "set logscale")
    } else {
      commands <- c(commands, paste("set logscale",
                                    validate_scalar_character(log, "log")))
    }
  }
  axes <- list(x = x, y = y, z = z, cb = cb)
  for (axis in names(axes)) {
    value <- axes[[axis]]
    if (is.null(value)) {
      next
    }
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      stop("`", axis, "log` must be TRUE or FALSE.", call. = FALSE)
    }
    commands <- c(commands,
                  paste(if (isTRUE(value)) "set" else "unset",
                        "logscale", axis))
  }
  commands
}

gp_format_command <- function(axis, fmt) {
  if (is.null(fmt)) {
    return(NULL)
  }
  paste("set format", axis, gp_string_quote(fmt))
}

gp_tics_command <- function(name, tics) {
  if (is.null(tics)) {
    return(NULL)
  }
  if (is.logical(tics) && length(tics) == 1L && !is.na(tics)) {
    return(paste(if (isTRUE(tics)) "set" else "unset", name))
  }
  if (is.character(tics) && length(tics) == 1L) {
    return(paste("set", name, validate_scalar_character(tics, name)))
  }
  if (is.numeric(tics)) {
    labels <- names(tics)
    if (!is.null(labels) && any(nzchar(labels))) {
      entries <- paste(gp_string_quote(labels), format_gnuplot_number(tics))
    } else {
      entries <- format_gnuplot_number(tics)
    }
    return(paste0("set ", name, " (", paste(entries, collapse = ", "), ")"))
  }
  stop("`", name, "` must be TRUE, FALSE, a raw string, a numeric vector, ",
       "or a named numeric vector.", call. = FALSE)
}

gp_set_unset_command <- function(name, value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.logical(value) && length(value) == 1L && !is.na(value)) {
    return(paste(if (isTRUE(value)) "set" else "unset", name))
  }
  paste("set", name, gp_value_string(value))
}

gp_set_value_command <- function(name, value) {
  if (is.null(value)) {
    return(NULL)
  }
  paste("set", name, gp_value_string(value))
}

gp_value_string <- function(value) {
  if (is.numeric(value)) {
    return(paste(vapply(value, gp_number, character(1)), collapse = ","))
  }
  validate_scalar_character(value, "value")
}

gp_pair_command <- function(name, value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (is.numeric(value)) {
    if (length(value) != 2L || anyNA(value)) {
      stop("`", name, "` must be a numeric vector of length two.",
           call. = FALSE)
    }
    return(paste("set", name, paste(vapply(value, gp_number, character(1)),
                                    collapse = ",")))
  }
  paste("set", name, validate_scalar_character(value, name))
}

gp_size_command <- function(size = NULL, square = NULL, ratio = NULL) {
  commands <- character()
  if (!is.null(size)) {
    commands <- c(commands, gp_pair_command("size", size))
  }
  if (!is.null(square)) {
    if (!is.logical(square) || length(square) != 1L || is.na(square)) {
      stop("`square` must be TRUE or FALSE.", call. = FALSE)
    }
    commands <- c(commands, paste("set size", if (isTRUE(square)) {
      "square"
    } else {
      "nosquare"
    }))
  }
  if (!is.null(ratio)) {
    commands <- c(commands, paste("set size ratio", gp_value_string(ratio)))
  }
  commands
}

gp_margins_command <- function(margins) {
  if (is.null(margins)) {
    return(NULL)
  }
  if (is.character(margins)) {
    return(paste("set margins", validate_scalar_character(margins, "margins")))
  }
  if (!is.numeric(margins) || length(margins) != 4L || anyNA(margins)) {
    stop("`margins` must be a numeric vector c(left, right, bottom, top) ",
         "or a raw string.", call. = FALSE)
  }
  paste("set margins", paste(vapply(margins, gp_number, character(1)),
                             collapse = ","))
}

gp_margin_command <- function(name, margin) {
  if (is.null(margin)) {
    return(NULL)
  }
  paste("set", name, gp_value_string(margin))
}

gp_style_setting_command <- function(name, value) {
  if (is.null(value)) {
    return(NULL)
  }
  paste("set style", name, validate_scalar_character(value, name))
}

gp_prefixed_commands <- function(prefix, value) {
  if (is.null(value)) {
    return(NULL)
  }
  paste("set", prefix, validate_script_lines(value, prefix))
}

gp_named_set_commands <- function(values) {
  if (is.null(values)) {
    return(NULL)
  }
  if (length(values) == 0L) {
    return(NULL)
  }
  if (!is.list(values)) {
    stop("Named set commands must be supplied as a list.", call. = FALSE)
  }
  nms <- names(values)
  if (is.null(nms) || any(!nzchar(nms))) {
    stop("Named set command lists must have non-empty names.", call. = FALSE)
  }
  out <- character()
  for (i in seq_along(values)) {
    out <- c(out, gp_set_unset_command(nms[[i]], values[[i]]))
  }
  out
}
