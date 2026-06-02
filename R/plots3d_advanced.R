#' Advanced 3D and 4D Plot Controls
#'
#' These arguments mirror common controls used throughout the official
#' gnuplot 3D and 4D demos: `splot`, `pm3d`, palette color mapping, contours,
#' map projection, hidden line removal, parametric surfaces, 3D vectors,
#' polygons, voxels, and isosurfaces.
#'
#' @param view Numeric vector or raw gnuplot view string. For example
#'   `view = c(60, 30)` becomes `set view 60,30`.
#' @param view_map If `TRUE`, use `set view map` for a 2D projection.
#' @param samples,isosamples Numeric sample counts used by gnuplot functions
#'   and pseudodata such as `"++"`.
#' @param ticslevel Numeric value passed to `set ticslevel`.
#' @param xyplane Numeric value passed to `set xyplane at`, or `TRUE` for
#'   `set xyplane relative 0`.
#' @param pm3d `TRUE`, `FALSE`, or a raw gnuplot pm3d option string such as
#'   `"map"` or `"depthorder"`.
#' @param pm3d_at Optional raw `pm3d at` location string, such as `"s"` or
#'   `"b"`.
#' @param hidden3d If `TRUE`, enable hidden line removal; if `FALSE`, unset it.
#' @param surface If `TRUE`, use `set surface`; if `FALSE`, use
#'   `unset surface`.
#' @param contour `TRUE` for `set contour base`, or a raw contour location
#'   such as `"base"`, `"surface"`, or `"both"`.
#' @param dgrid3d Numeric grid size used to interpolate scattered 3D data.
#' @param cbrange Optional color-box range.
#' @param cblabel Optional color-box label.
#' @param colorbox `TRUE`, `FALSE`, or a raw gnuplot colorbox option string.
#' @param cbformat Optional color-box tick format.
#' @param urange,vrange Optional parameter ranges for parametric plots.
#' @param angles Optional raw gnuplot angle mode, e.g. `"degrees"` or
#'   `"radians"`.
#' @param mapping Optional raw gnuplot mapping, e.g. `"spherical"`.
#' @param settings Optional character vector returned by `gp_options()`.
#' @name gp_3d_setup
#' @keywords internal
NULL

gp_3d_setup <- function(main = NULL,
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
                        cbrange = NULL,
                        cblabel = NULL,
                        colorbox = NULL,
                        cbformat = NULL,
                        urange = NULL,
                        vrange = NULL,
                        angles = NULL,
                        mapping = NULL,
                        parametric = NULL,
                        settings = NULL,
                        extra = NULL,
                        ...) {
  extra <- validate_script_lines(extra, "extra")
  settings <- validate_script_lines(settings, "settings")
  c(
    gp_setup(main = main, subtitle = subtitle, xlab = xlab, ylab = ylab,
             zlab = zlab, grid = grid, key = key, xrange = xrange,
             yrange = yrange, zrange = zrange, palette = palette,
             theme = theme, settings = NULL, extra = NULL, ...),
    range_command("urange", urange),
    range_command("vrange", vrange),
    range_command("cbrange", cbrange),
    if (!is.null(cblabel)) paste("set cblabel", gp_quote(cblabel)),
    view_command(view = view, view_map = view_map),
    sample_command("samples", samples),
    sample_command("isosamples", isosamples),
    if (!is.null(ticslevel)) paste("set ticslevel", gp_number(ticslevel)),
    xyplane_command(xyplane),
    pm3d_command(pm3d, pm3d_at),
    flag_command("hidden3d", hidden3d),
    flag_command("surface", surface),
    contour_command(contour),
    dgrid3d_command(dgrid3d),
    colorbox_command(colorbox),
    if (!is.null(cbformat)) paste("set cbformat", gp_quote(cbformat)),
    if (!is.null(angles)) paste("set angles", validate_scalar_character(angles, "angles")),
    if (!is.null(mapping)) paste("set mapping", validate_scalar_character(mapping, "mapping")),
    flag_command("parametric", parametric),
    settings,
    extra
  )
}

view_command <- function(view = NULL, view_map = FALSE) {
  if (isTRUE(view_map)) {
    return("set view map")
  }
  if (is.null(view)) {
    return(NULL)
  }
  if (is.character(view)) {
    return(paste("set view", validate_scalar_character(view, "view")))
  }
  if (!is.numeric(view) || length(view) < 2L || length(view) > 4L ||
      anyNA(view)) {
    stop("`view` must be a numeric vector of length 2 to 4, or a string.",
         call. = FALSE)
  }
  paste("set view", paste(vapply(view, gp_number, character(1)),
                          collapse = ","))
}

sample_command <- function(name, value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (!is.numeric(value) || length(value) > 2L || length(value) < 1L ||
      anyNA(value)) {
    stop("`", name, "` must be one or two numeric values.", call. = FALSE)
  }
  paste("set", name, paste(vapply(value, gp_number, character(1)),
                           collapse = ","))
}

xyplane_command <- function(xyplane) {
  if (is.null(xyplane)) {
    return(NULL)
  }
  if (identical(xyplane, TRUE)) {
    return("set xyplane relative 0")
  }
  if (identical(xyplane, FALSE)) {
    return("unset xyplane")
  }
  if (!is.numeric(xyplane) || length(xyplane) != 1L || is.na(xyplane)) {
    stop("`xyplane` must be TRUE, FALSE, or a single numeric value.",
         call. = FALSE)
  }
  paste("set xyplane at", gp_number(xyplane))
}

pm3d_command <- function(pm3d, pm3d_at = NULL) {
  if (is.null(pm3d) && is.null(pm3d_at)) {
    return(NULL)
  }
  if (identical(pm3d, FALSE)) {
    return("unset pm3d")
  }
  parts <- "set pm3d"
  if (is.character(pm3d)) {
    parts <- c(parts, validate_scalar_character(pm3d, "pm3d"))
  }
  if (!is.null(pm3d_at)) {
    parts <- c(parts, "at", validate_scalar_character(pm3d_at, "pm3d_at"))
  }
  paste(parts, collapse = " ")
}

flag_command <- function(name, value) {
  if (is.null(value)) {
    return(NULL)
  }
  if (!is.logical(value) || length(value) != 1L || is.na(value)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  paste(if (isTRUE(value)) "set" else "unset", name)
}

contour_command <- function(contour) {
  if (is.null(contour)) {
    return(NULL)
  }
  if (identical(contour, FALSE)) {
    return("unset contour")
  }
  if (identical(contour, TRUE)) {
    return("set contour base")
  }
  paste("set contour", validate_scalar_character(contour, "contour"))
}

dgrid3d_command <- function(dgrid3d) {
  if (is.null(dgrid3d)) {
    return(NULL)
  }
  if (identical(dgrid3d, FALSE)) {
    return("unset dgrid3d")
  }
  if (!is.numeric(dgrid3d) || length(dgrid3d) < 1L || length(dgrid3d) > 3L ||
      anyNA(dgrid3d)) {
    stop("`dgrid3d` must be FALSE or one to three numeric values.",
         call. = FALSE)
  }
  paste("set dgrid3d", paste(vapply(dgrid3d, gp_number, character(1)),
                             collapse = ","))
}

colorbox_command <- function(colorbox) {
  if (is.null(colorbox)) {
    return(NULL)
  }
  if (identical(colorbox, TRUE)) {
    return("set colorbox")
  }
  if (identical(colorbox, FALSE)) {
    return("unset colorbox")
  }
  paste("set colorbox", validate_scalar_character(colorbox, "colorbox"))
}

#' Generic 3D `splot`
#'
#' Use `gp_splot()` when you already know the gnuplot expression or data file
#' clause you want to pass to `splot`. It exposes the same high-level preview,
#' output, theme, palette, and 3D controls as the convenience wrappers.
#'
#' @param expr A raw gnuplot expression or data source clause.
#' @param type Gnuplot plotting style.
#' @param using Optional `using` clause for a data source.
#' @param legend Legend label. Use `FALSE` for no legend.
#' @inheritParams gp_surface
#' @inheritParams gp_3d_setup
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' gp_splot("sin(x*y) / (x*y)", xrange = c(-10, 10), yrange = c(-10, 10),
#'          samples = 80, isosamples = 80, hidden3d = TRUE)
#' }
gp_splot <- function(expr,
                     type = "lines",
                     using = NULL,
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
                     hidden3d = NULL,
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
  expr <- validate_scalar_character(expr, "expr")
  style <- gp_style(type, using = using, title = legend, col = col,
                    color = color, line_col = line_col,
                    point_col = point_col, lwd = lwd, lty = lty, pch = pch,
                    cex = cex)
  body <- paste("splot", layer_term(expr, style))
  gp_render(
    plot_body = body,
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid,
                        key = !is.null(legend) && !identical(legend, FALSE),
                        xrange = xrange, yrange = yrange, zrange = zrange,
                        palette = palette, cbrange = cbrange,
                        cblabel = cblabel, colorbox = colorbox,
                        view = view, view_map = view_map, samples = samples,
                        isosamples = isosamples, ticslevel = ticslevel,
                        xyplane = xyplane, pm3d = pm3d, pm3d_at = pm3d_at,
                        hidden3d = hidden3d, surface = surface,
                        contour = contour, dgrid3d = dgrid3d,
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

#' 4D Surface Plot
#'
#' Draw a 3D surface where the fourth dimension is mapped to palette color.
#' `z` may be a matrix, data frame, R function `function(x, y)`, or a gnuplot
#' expression string. `color` may be omitted, in which case the z value is also
#' used as the color value.
#'
#' @param z Matrix, data frame, R function, or gnuplot expression for z.
#' @param color Optional fourth-dimension values, function, matrix, vector, or
#'   gnuplot expression.
#' @param x,y Optional x/y grid values for matrix and function input.
#' @param n Grid size when `z` is an R function and `x` or `y` is omitted.
#' @param definitions Optional raw gnuplot function definitions used when `z`
#'   is a character expression.
#' @param type Surface style, usually `"pm3d"`, `"points"`, or `"lines"`.
#' @param legend Legend label. Use `FALSE` for no legend.
#' @inheritParams gp_surface
#' @inheritParams gp_3d_setup
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- seq(-3, 3, length.out = 80)
#' y <- seq(-3, 3, length.out = 80)
#' z <- outer(y, x, function(y, x) sin(x) * cos(y))
#' c <- outer(y, x, function(y, x) x^2 + y^2)
#' gp_surface4d(z, c, x = x, y = y, main = "4D surface",
#'              cblabel = "radius^2", view = c(55, 35))
#'
#' gp_surface4d("sin(sqrt(x**2+y**2))", "x*y",
#'              xrange = c(-8, 8), yrange = c(-8, 8),
#'              samples = 80, isosamples = 80)
#' }
gp_surface4d <- function(z,
                         color = NULL,
                         x = NULL,
                         y = NULL,
                         n = 60,
                         definitions = NULL,
                         type = c("pm3d", "points", "lines", "linespoints"),
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
                         pm3d = TRUE,
                         pm3d_at = NULL,
                         hidden3d = NULL,
                         surface = NULL,
                         contour = NULL,
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
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  style <- gp_style(type, title = legend)

  if (is.character(z)) {
    if (length(z) != 1L || is.na(z) || !nzchar(z)) {
      stop("Character `z` must be a single gnuplot expression.", call. = FALSE)
    }
    if (is.null(color)) {
      color <- z
    }
    color <- validate_scalar_character(color, "color")
    definitions <- validate_script_lines(definitions, "definitions")
    source <- "'++'"
    using <- paste0("1:2:(gpr_z($1,$2)):(gpr_color($1,$2))")
    setup_extra <- c(
      definitions,
      paste("gpr_z(x,y) =", z),
      paste("gpr_color(x,y) =", color),
      extra
    )
  } else {
    data <- xyzc_data(z = z, color = color, x = x, y = y, n = n)
    data_file <- write_grid_table(data, workdir = workdir,
                                  prefix = "gunplotR-xyzc-")
    if (isTRUE(cleanup)) {
      on.exit(unlink(data_file), add = TRUE)
    }
    source <- gp_quote(data_file)
    using <- "1:2:3:4"
    setup_extra <- extra
  }
  style$using <- using

  gp_render(
    plot_body = paste("splot", layer_term(source, style)),
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid,
                        key = !is.null(legend) && !identical(legend, FALSE),
                        xrange = xrange, yrange = yrange, zrange = zrange,
                        palette = palette, cbrange = cbrange,
                        cblabel = cblabel, colorbox = colorbox,
                        view = view, view_map = view_map,
                        samples = samples, isosamples = isosamples,
                        ticslevel = ticslevel, xyplane = xyplane,
                        pm3d = pm3d, pm3d_at = pm3d_at,
                        hidden3d = hidden3d, surface = surface,
                        contour = contour, theme = theme, extra = setup_extra),
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

#' 4D Point Cloud
#'
#' Draw 3D points, lines, impulses, or linespoints with a fourth variable
#' mapped to palette color.
#'
#' @param x Numeric x values or a data frame with x/y/z/color columns.
#' @param y,z,color Optional numeric vectors when `x` is not a data frame.
#' @param type Gnuplot 3D style.
#' @param legend Legend label. Use `FALSE` for no legend.
#' @param pch,cex,lwd R-like point type, point size, and line width aliases.
#' @inheritParams gp_surface
#' @inheritParams gp_3d_setup
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' theta <- seq(0, 8 * pi, length.out = 600)
#' gp_points4d(cos(theta), sin(theta), theta, theta,
#'             pch = 7, cex = 0.8, main = "4D helix",
#'             cblabel = "theta", view = c(60, 35))
#' }
gp_points4d <- function(x,
                        y = NULL,
                        z = NULL,
                        color = NULL,
                        type = c("points", "linespoints", "lines",
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
                        pch = NULL,
                        cex = NULL,
                        lwd = NULL,
                        palette = NULL,
                        cbrange = NULL,
                        cblabel = NULL,
                        colorbox = NULL,
                        view = NULL,
                        view_map = FALSE,
                        hidden3d = NULL,
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
  data <- xyzc_points_data(x, y = y, z = z, color = color)
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- write_plot_table(data, workdir = workdir,
                                prefix = "gunplotR-4d-points-")
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }

  style <- gp_style(type, using = "1:2:3:4", title = legend, pch = pch,
                    cex = cex, lwd = lwd, palette = TRUE)
  gp_render(
    plot_body = paste("splot", layer_term(gp_quote(data_file), style)),
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid,
                        key = !is.null(legend) && !identical(legend, FALSE),
                        xrange = xrange, yrange = yrange, zrange = zrange,
                        palette = palette, cbrange = cbrange,
                        cblabel = cblabel, colorbox = colorbox,
                        view = view, view_map = view_map,
                        hidden3d = hidden3d, theme = theme, extra = extra),
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

#' Project a 3D Surface as a Color Map
#'
#' This is a convenience wrapper for the common gnuplot demo pattern
#' `set view map` plus `pm3d map`.
#'
#' @inheritParams gp_surface4d
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- seq(-4, 4, length.out = 100)
#' y <- seq(-4, 4, length.out = 100)
#' z <- outer(y, x, function(y, x) sin(x * y))
#' gp_map3d(z, x = x, y = y, main = "pm3d map")
#' }
gp_map3d <- function(z,
                     color = NULL,
                     x = NULL,
                     y = NULL,
                     n = 80,
                     definitions = NULL,
                     xrange = NULL,
                     yrange = NULL,
                     zrange = NULL,
                     main = NULL,
                     subtitle = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     zlab = NULL,
                     grid = FALSE,
                     palette = NULL,
                     cbrange = NULL,
                     cblabel = NULL,
                     colorbox = NULL,
                     samples = NULL,
                     isosamples = NULL,
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
  gp_surface4d(z = z, color = color, x = x, y = y, n = n,
               definitions = definitions, type = "pm3d", xrange = xrange,
               yrange = yrange, zrange = zrange, main = main,
               subtitle = subtitle, xlab = xlab, ylab = ylab, zlab = zlab,
               grid = grid, palette = palette, cbrange = cbrange,
               cblabel = cblabel, colorbox = colorbox, view_map = TRUE,
               samples = samples, isosamples = isosamples, pm3d = "map",
               theme = theme, extra = extra, output = output,
               terminal = terminal, preview = preview,
               preview_terminal = preview_terminal, path = path,
               workdir = workdir, echo = echo, cleanup = cleanup)
}

#' Parametric Surface
#'
#' Draw a parametric 3D surface from three gnuplot expressions in `u` and `v`.
#'
#' @param x_expr,y_expr,z_expr Gnuplot expressions for x(u,v), y(u,v), z(u,v).
#' @param type Gnuplot plotting style.
#' @param legend Legend label. Use `FALSE` for no legend.
#' @param col,lwd,lty R-like color, line width, and line type aliases.
#' @inheritParams gp_surface
#' @inheritParams gp_3d_setup
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' gp_parametric_surface(
#'   "cos(u) * cos(v)", "sin(u) * cos(v)", "sin(v)",
#'   urange = c(-pi, pi), vrange = c(-pi / 2, pi / 2),
#'   samples = 80, isosamples = 40, hidden3d = TRUE,
#'   main = "Parametric sphere"
#' )
#' }
gp_parametric_surface <- function(x_expr,
                                  y_expr,
                                  z_expr,
                                  type = c("lines", "pm3d", "points",
                                           "linespoints"),
                                  legend = NULL,
                                  urange = NULL,
                                  vrange = NULL,
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
                                  lwd = NULL,
                                  lty = NULL,
                                  palette = NULL,
                                  view = NULL,
                                  samples = NULL,
                                  isosamples = NULL,
                                  hidden3d = NULL,
                                  pm3d = NULL,
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
  x_expr <- validate_scalar_character(x_expr, "x_expr")
  y_expr <- validate_scalar_character(y_expr, "y_expr")
  z_expr <- validate_scalar_character(z_expr, "z_expr")
  style <- gp_style(type, title = legend, col = col, lwd = lwd, lty = lty)
  gp_render(
    plot_body = paste("splot", paste(x_expr, y_expr, z_expr, sep = ","),
                      "with", style_clause(style),
                      title_clause(style$title)),
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid,
                        key = !is.null(legend) && !identical(legend, FALSE),
                        xrange = xrange, yrange = yrange, zrange = zrange,
                        palette = palette, view = view, samples = samples,
                        isosamples = isosamples, hidden3d = hidden3d,
                        pm3d = if (is.null(pm3d) && type == "pm3d") TRUE else pm3d,
                        urange = urange, vrange = vrange,
                        parametric = TRUE, theme = theme, extra = extra),
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

#' 3D Boxes
#'
#' Draw 3D boxes from x/y/z values. If `color` is supplied, it is used as a
#' palette value.
#'
#' @param x,y,z Numeric box positions or `x` as a data frame.
#' @param color Optional palette color value.
#' @param boxwidth Optional raw gnuplot boxwidth value.
#' @param legend Legend label. Use `FALSE` for no legend.
#' @inheritParams gp_points4d
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- rep(1:4, each = 4)
#' y <- rep(1:4, 4)
#' z <- as.vector(outer(1:4, 1:4, "+"))
#' gp_boxes3d(x, y, z, color = z, cblabel = "height",
#'            view = c(60, 30), main = "3D boxes")
#' }
gp_boxes3d <- function(x,
                       y = NULL,
                       z = NULL,
                       color = NULL,
                       boxwidth = NULL,
                       legend = NULL,
                       main = NULL,
                       subtitle = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       zlab = NULL,
                       grid = TRUE,
                       palette = NULL,
                       cblabel = NULL,
                       view = NULL,
                       hidden3d = NULL,
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
  has_color <- !is.null(color) || (is.data.frame(x) && ncol(x) >= 4L)
  data <- if (has_color) {
    xyzc_points_data(x, y = y, z = z, color = color)
  } else {
    xyz_data(x, y = y, z = z)
  }
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- write_plot_table(data, workdir = workdir,
                                prefix = "gunplotR-boxes3d-")
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }
  using <- if (has_color) "1:2:3:4" else "1:2:3"
  style <- gp_style("boxes", using = using, title = legend,
                    palette = has_color)
  gp_render(
    plot_body = paste("splot", layer_term(gp_quote(data_file), style)),
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid,
                        key = !is.null(legend) && !identical(legend, FALSE),
                        palette = palette, cblabel = cblabel, view = view,
                        hidden3d = hidden3d, theme = theme,
                        extra = c(if (!is.null(boxwidth)) {
                          paste("set boxwidth", gp_value(boxwidth))
                        }, extra)),
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

#' 3D Bar Plot
#'
#' Draw a 3D bar chart from a matrix, vector, or data frame. For matrix input,
#' columns become the x axis and rows become the y axis. Row and column names
#' are used as tick labels when present.
#'
#' @param height Numeric matrix, vector, or data frame. A data frame must have
#'   x, y, and height columns; an optional fourth column is used for color.
#' @param x,y Optional x/y positions or category labels for matrix input.
#' @param color Optional color values for palette coloring. These values are
#'   used only for color; bar width and depth are controlled by `width` and
#'   `depth`.
#' @param width Optional bar width passed to `set boxwidth`.
#' @param depth Optional bar depth passed to `set boxdepth`.
#' @param fill Fill opacity or raw fill style.
#' @param color_mode How to use `color`: `"palette"` maps it through the
#'   current palette, `"line_variable"` passes it to `lc variable`,
#'   `"rgb_variable"` passes it to `fc rgb variable`, and `"none"` ignores it.
#' @param xyplane Numeric value passed to `set xyplane at`, or `TRUE` for
#'   `set xyplane relative 0`.
#' @param pm3d_border Optional raw `set pm3d border` value, such as
#'   `"lc black"`.
#' @param depthorder If `TRUE`, use `set pm3d depthorder` to depth-sort boxes.
#' @param lighting If `TRUE`, use `set pm3d lighting`.
#' @param walls Optional `set walls` value. Use `TRUE` for `"z0"`, or a raw
#'   string such as `"x0 y0 z0"`.
#' @param legend Legend label. Use `FALSE` for no legend.
#' @param cbrange Optional color-box range.
#' @param colorbox `TRUE`, `FALSE`, or a raw gnuplot colorbox option string.
#' @inheritParams gp_boxes3d
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' z <- outer(1:5, 1:4, "*")
#' rownames(z) <- paste0("R", 1:5)
#' colnames(z) <- paste0("C", 1:4)
#' gp_bar3d(z, main = "3D bar chart", view = c(60, 35))
#' }
gp_bar3d <- function(height,
                     x = NULL,
                     y = NULL,
                     color = NULL,
                     width = 0.8,
                     depth = NULL,
                     fill = 0.7,
                     color_mode = c("palette", "line_variable",
                                    "rgb_variable", "none"),
                     xyplane = 0,
                     pm3d_border = NULL,
                     depthorder = FALSE,
                     lighting = FALSE,
                     walls = NULL,
                     legend = NULL,
                     main = NULL,
                     subtitle = NULL,
                     xlab = NULL,
                     ylab = NULL,
                     zlab = NULL,
                     grid = TRUE,
                     palette = NULL,
                     cbrange = NULL,
                     cblabel = NULL,
                     colorbox = NULL,
                     view = c(60, 35),
                     hidden3d = TRUE,
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
  color_mode <- match.arg(color_mode)
  prepared <- bar3d_data(height, x = x, y = y, color = color)
  data <- prepared$data
  has_color <- "color" %in% names(data) && color_mode != "none"
  if (has_color) {
    if (color_mode == "rgb_variable") {
      data$color <- rgb_variable_values(data$color)
    } else if (!is.numeric(data$color)) {
      stop("`color` must be numeric for palette or line-variable coloring.",
           call. = FALSE)
    }
  }
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- write_plot_table(data, workdir = workdir,
                                prefix = "gunplotR-bar3d-")
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }

  style_extra <- switch(
    color_mode,
    palette = NULL,
    line_variable = if (has_color) "lc variable" else NULL,
    rgb_variable = if (has_color) "fc rgb variable" else NULL,
    none = NULL
  )
  style <- gp_style(
    "boxes",
    using = bar3d_using_spec(has_color, width = width, depth = depth),
    title = legend,
    fill = fill,
    palette = has_color && color_mode == "palette",
    extra = style_extra
  )
  gp_render(
    plot_body = paste("splot", layer_term(gp_quote(data_file), style)),
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid,
                        key = !is.null(legend) && !identical(legend, FALSE),
                        palette = palette, cbrange = cbrange,
                        cblabel = cblabel, colorbox = colorbox,
                        view = view, xyplane = xyplane,
                        hidden3d = hidden3d, theme = theme,
                        extra = c(
                          paste("set boxwidth", gp_value(width)),
                          if (!is.null(depth)) paste("set boxdepth", gp_value(depth)),
                          if (!is.null(pm3d_border)) {
                            paste("set pm3d border",
                                  validate_scalar_character(pm3d_border,
                                                            "pm3d_border"))
                          },
                          if (isTRUE(depthorder)) "set pm3d depthorder",
                          if (isTRUE(lighting)) "set pm3d lighting",
                          walls_command(walls),
                          prepared$xtics,
                          prepared$ytics,
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

#' 3D Parallel Line Plot
#'
#' Draw multiple parallel polylines in 3D. Matrix rows are treated as separate
#' lines offset along the y axis; columns are x positions and values are z.
#'
#' @param z Numeric matrix or data frame with x/y/z columns.
#' @param x Optional x positions or labels for matrix input.
#' @param y Optional y positions or labels for matrix input.
#' @param palette_by Color lines by `"none"`, `"y"`, or `"z"`.
#' @param legend Legend label. Use `FALSE` for no legend.
#' @param col,lwd,lty R-like line color, width, and type aliases.
#' @inheritParams gp_surface
#' @inheritParams gp_3d_setup
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- seq(0, 2 * pi, length.out = 80)
#' z <- rbind(sin(x), cos(x), sin(x) * cos(x))
#' rownames(z) <- c("sin", "cos", "mix")
#' gp_parallel3d(z, x = x, main = "3D parallel lines",
#'               view = c(65, 35), lwd = 2)
#' }
gp_parallel3d <- function(z,
                          x = NULL,
                          y = NULL,
                          palette_by = c("none", "y", "z"),
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
                          lwd = NULL,
                          lty = NULL,
                          palette = NULL,
                          cbrange = NULL,
                          cblabel = NULL,
                          colorbox = NULL,
                          view = c(60, 35),
                          hidden3d = FALSE,
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
  palette_by <- match.arg(palette_by)
  prepared <- parallel3d_data(z, x = x, y = y, palette_by = palette_by)
  data <- prepared$data
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- write_grouped_table(data[setdiff(names(data), "group")],
                                   group = data$group, workdir = workdir,
                                   prefix = "gunplotR-parallel3d-")
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }

  has_color <- palette_by != "none"
  style <- gp_style("lines", using = if (has_color) "1:2:3:4" else "1:2:3",
                    title = legend, col = col, lwd = lwd, lty = lty,
                    palette = has_color)
  gp_render(
    plot_body = paste("splot", layer_term(gp_quote(data_file), style)),
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid,
                        key = !is.null(legend) && !identical(legend, FALSE),
                        xrange = xrange, yrange = yrange, zrange = zrange,
                        palette = palette, cbrange = cbrange,
                        cblabel = cblabel, colorbox = colorbox,
                        view = view, hidden3d = hidden3d, theme = theme,
                        extra = c(prepared$xtics, prepared$ytics, extra)),
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

#' 3D Vectors
#'
#' Draw vectors from `(x, y, z)` with components `(dx, dy, dz)`.
#'
#' @param x,y,z Numeric vector origins.
#' @param dx,dy,dz Numeric vector components.
#' @param legend Legend label. Use `FALSE` for no legend.
#' @param col,lwd R-like vector color and line width aliases.
#' @inheritParams gp_surface
#' @inheritParams gp_3d_setup
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' x <- y <- z <- seq(-1, 1, length.out = 5)
#' grid <- expand.grid(x = x, y = y, z = z)
#' gp_vectors3d(grid$x, grid$y, grid$z, -grid$y, grid$x, 0,
#'              col = 4, main = "3D vector field")
#' }
gp_vectors3d <- function(x,
                         y,
                         z,
                         dx,
                         dy,
                         dz,
                         legend = NULL,
                         main = NULL,
                         subtitle = NULL,
                         xlab = NULL,
                         ylab = NULL,
                         zlab = NULL,
                         grid = TRUE,
                         col = NULL,
                         lwd = NULL,
                         view = NULL,
                         hidden3d = NULL,
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
  validate_same_length(x = x, y = y, z = z)
  n <- length(x)
  dx <- recycle_numeric(dx, n, "dx")
  dy <- recycle_numeric(dy, n, "dy")
  dz <- recycle_numeric(dz, n, "dz")
  gp_multi(
    gp_layer(data = data.frame(x = x, y = y, z = z, dx = dx, dy = dy,
                               dz = dz),
             style = "vectors", using = "1:2:3:4:5:6", title = legend,
             col = col, lwd = lwd),
    dimensions = "3d", main = main, subtitle = subtitle, xlab = xlab,
    ylab = ylab, zlab = zlab, grid = grid, view = view,
    hidden3d = hidden3d, theme = theme, extra = extra, output = output,
    terminal = terminal, preview = preview,
    preview_terminal = preview_terminal, path = path, workdir = workdir,
    echo = echo, cleanup = cleanup
  )
}

#' 3D Labels
#'
#' Draw text labels at 3D positions.
#'
#' @param x,y,z Numeric label positions.
#' @param labels Character labels.
#' @param col R-like label color alias.
#' @inheritParams gp_vectors3d
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' gp_labels3d(1:3, 1:3, 1:3, labels = c("A", "B", "C"),
#'             col = 2, main = "3D labels")
#' }
gp_labels3d <- function(x,
                        y,
                        z,
                        labels,
                        legend = FALSE,
                        main = NULL,
                        subtitle = NULL,
                        xlab = NULL,
                        ylab = NULL,
                        zlab = NULL,
                        grid = TRUE,
                        col = NULL,
                        view = NULL,
                        hidden3d = NULL,
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
  validate_same_length(x = x, y = y, z = z, labels = labels)
  gp_multi(
    gp_layer(data = data.frame(x = x, y = y, z = z, label = labels),
             style = "labels", using = "1:2:3:4", title = legend,
             quote_data = TRUE, col = col),
    dimensions = "3d", main = main, subtitle = subtitle, xlab = xlab,
    ylab = ylab, zlab = zlab, grid = grid, view = view,
    hidden3d = hidden3d, theme = theme, extra = extra, output = output,
    terminal = terminal, preview = preview,
    preview_terminal = preview_terminal, path = path, workdir = workdir,
    echo = echo, cleanup = cleanup
  )
}

#' 3D Polygons
#'
#' Draw grouped 3D polygons. `data` must contain x/y/z columns and optionally a
#' group column; blank lines are inserted between groups for gnuplot.
#'
#' @param data Data frame containing x/y/z columns.
#' @param group Optional grouping vector or column name.
#' @param col Polygon line or fill color.
#' @param fill Polygon fill option. Numeric values are treated as opacity.
#' @inheritParams gp_vectors3d
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' square <- data.frame(
#'   x = c(0, 1, 1, 0, 0, 1, 1, 0),
#'   y = c(0, 0, 1, 1, 0, 0, 1, 1),
#'   z = c(0, 0, 0, 0, 1, 1, 1, 1),
#'   face = rep(c("bottom", "top"), each = 4)
#' )
#' gp_polygons3d(square, group = "face", fill = 0.4,
#'               hidden3d = TRUE, main = "Grouped polygons")
#' }
gp_polygons3d <- function(data,
                          group = NULL,
                          legend = NULL,
                          main = NULL,
                          subtitle = NULL,
                          xlab = NULL,
                          ylab = NULL,
                          zlab = NULL,
                          grid = TRUE,
                          col = NULL,
                          fill = "solid 0.5",
                          view = NULL,
                          hidden3d = NULL,
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
  if (!is.data.frame(data) || ncol(data) < 3L) {
    stop("`data` must be a data frame with at least x, y, and z columns.",
         call. = FALSE)
  }
  poly <- data.frame(x = data[[1L]], y = data[[2L]], z = data[[3L]])
  group <- polygon_group(data, group)
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- write_grouped_table(poly, group = group, workdir = workdir,
                                   prefix = "gunplotR-polygons3d-")
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }
  style <- gp_style("polygons", using = "1:2:3", title = legend, col = col,
                    fill = fill)
  gp_render(
    plot_body = paste("splot", layer_term(gp_quote(data_file), style)),
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid,
                        key = !is.null(legend) && !identical(legend, FALSE),
                        view = view, hidden3d = hidden3d, theme = theme,
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

#' 3D Voxel Cloud
#'
#' Draw voxel centers as a 4D point cloud. This is useful for inspecting a
#' volumetric scalar field before extracting an isosurface.
#'
#' @param x Numeric x values or a data frame with x/y/z/value columns.
#' @param y,z Optional numeric coordinates when `x` is not a data frame.
#' @param value Optional scalar values used for palette coloring.
#' @param type `"points"` for point voxels or `"boxes"` for box voxels.
#' @param ... Additional arguments passed to `gp_points4d()` or `gp_boxes3d()`.
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' grid <- expand.grid(x = seq(-1, 1, length.out = 8),
#'                     y = seq(-1, 1, length.out = 8),
#'                     z = seq(-1, 1, length.out = 8))
#' value <- with(grid, x^2 + y^2 + z^2)
#' gp_voxels(grid$x, grid$y, grid$z, value, cex = 0.8,
#'           cblabel = "radius^2", main = "Voxel cloud")
#' }
gp_voxels <- function(x,
                      y = NULL,
                      z = NULL,
                      value = NULL,
                      type = c("points", "boxes"),
                      ...) {
  type <- match.arg(type)
  if (type == "points") {
    gp_points4d(x = x, y = y, z = z, color = value, ...)
  } else {
    gp_boxes3d(x = x, y = y, z = z, color = value, ...)
  }
}

#' Isosurface from Voxel Contributions
#'
#' Build a gnuplot voxel grid with `vfill` and draw an isosurface. The input is
#' a set of 3D points plus optional radius and weight values.
#'
#' @param x,y,z Numeric voxel contribution centers, or `x` as a data frame with
#'   x/y/z/radius/weight columns.
#' @param radius Numeric radius values passed to `vfill`.
#' @param weight Numeric weight values passed to `vfill`.
#' @param level Isosurface level.
#' @param grid_size Voxel grid size. Use one value or three values.
#' @param legend Legend label. Use `FALSE` for no legend.
#' @param col Isosurface color.
#' @param fill Isosurface fill option. Numeric values are treated as opacity.
#' @inheritParams gp_surface
#' @inheritParams gp_3d_setup
#'
#' @return Invisibly returns rendering details from the plotting process.
#' @export
#'
#' @examples
#' \dontrun{
#' theta <- seq(0, 6 * pi, length.out = 300)
#' gp_isosurface(cos(theta), sin(theta), theta / 5,
#'               radius = 0.25, weight = 1, level = 0.15,
#'               grid_size = 50, main = "Voxel isosurface")
#' }
gp_isosurface <- function(x,
                          y = NULL,
                          z = NULL,
                          radius = 1,
                          weight = 1,
                          level = 0.5,
                          grid_size = 40,
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
                          fill = "solid 0.8",
                          view = NULL,
                          hidden3d = FALSE,
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
  data <- voxel_data(x, y = y, z = z, radius = radius, weight = weight)
  xrange <- xrange %||% padded_range(data$x, data$radius)
  yrange <- yrange %||% padded_range(data$y, data$radius)
  zrange <- zrange %||% padded_range(data$z, data$radius)
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- write_plot_table(data, workdir = workdir,
                                prefix = "gunplotR-voxels-")
  if (isTRUE(cleanup)) {
    on.exit(unlink(data_file), add = TRUE)
  }
  if (!is.numeric(level) || length(level) != 1L || is.na(level)) {
    stop("`level` must be a single numeric value.", call. = FALSE)
  }
  if (!is.numeric(grid_size) || length(grid_size) < 1L ||
      length(grid_size) > 3L || anyNA(grid_size)) {
    stop("`grid_size` must be one to three numeric values.", call. = FALSE)
  }
  style <- gp_style("isosurface", title = legend, col = col, fill = fill)
  style$style <- paste("isosurface level", gp_number(level))
  gp_render(
    plot_body = paste("splot", layer_term("$gunplotR_voxels", style)),
    setup = gp_3d_setup(main = main, subtitle = subtitle, xlab = xlab,
                        ylab = ylab, zlab = zlab, grid = grid,
                        key = !is.null(legend) && !identical(legend, FALSE),
                        xrange = xrange, yrange = yrange, zrange = zrange,
                        view = view, hidden3d = hidden3d, theme = theme,
                        extra = c(
                          paste("set vgrid $gunplotR_voxels size",
                                paste(vapply(grid_size, gp_number,
                                             character(1)),
                                      collapse = ",")),
                          paste("vfill", gp_quote(data_file),
                                "using 1:2:3:4:($5)"),
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

xyzc_data <- function(z, color = NULL, x = NULL, y = NULL, n = 60) {
  if (is.data.frame(z)) {
    if (ncol(z) < 3L) {
      stop("A data frame `z` must have at least x, y, z columns.",
           call. = FALSE)
    }
    out <- data.frame(x = z[[1L]], y = z[[2L]], z = z[[3L]])
    out$color <- if (ncol(z) >= 4L && is.null(color)) z[[4L]] else color
  } else if (is.function(z)) {
    out <- function_to_xyzc(z, color = color, x = x, y = y, n = n)
  } else {
    out <- matrix_to_xyzc(z, color = color, x = x, y = y)
  }

  if (is.null(out$color)) {
    out$color <- out$z
  }
  if (length(out$color) == 1L) {
    out$color <- rep(out$color, nrow(out))
  }
  if (length(out$color) != nrow(out)) {
    stop("`color` must have length 1 or match the number of grid points.",
         call. = FALSE)
  }
  numeric_columns(out, c("x", "y", "z", "color"))
}

matrix_to_xyzc <- function(z, color = NULL, x = NULL, y = NULL) {
  out <- matrix_to_xyz(z, x = x, y = y)
  if (is.matrix(color)) {
    if (!identical(dim(color), dim(z))) {
      stop("Matrix `color` must have the same dimensions as `z`.",
           call. = FALSE)
    }
    out$color <- as.vector(t(color))
  } else if (!is.null(color)) {
    out$color <- color
  } else {
    out$color <- out$z
  }
  out
}

function_to_xyzc <- function(fun, color = NULL, x = NULL, y = NULL, n = 60) {
  out <- function_to_xyz(fun, x = x, y = y, n = n)
  if (is.null(color)) {
    out$color <- out$z
  } else if (is.function(color)) {
    values <- tryCatch(color(out$x, out$y), error = function(err) NULL)
    if (is.null(values) || length(values) != nrow(out)) {
      values <- mapply(color, out$x, out$y)
    }
    out$color <- values
  } else {
    out$color <- color
  }
  out
}

xyzc_points_data <- function(x, y = NULL, z = NULL, color = NULL) {
  if (is.data.frame(x) && is.null(y) && is.null(z) && is.null(color)) {
    if (ncol(x) < 4L) {
      stop("A data frame must have at least x, y, z, and color columns.",
           call. = FALSE)
    }
    out <- data.frame(x = x[[1L]], y = x[[2L]], z = x[[3L]],
                      color = x[[4L]])
  } else {
    validate_same_length(x = x, y = y, z = z, color = color)
    out <- data.frame(x = x, y = y, z = z, color = color)
  }
  numeric_columns(out, c("x", "y", "z", "color"))
}

voxel_data <- function(x, y = NULL, z = NULL, radius = 1, weight = 1) {
  if (is.data.frame(x) && is.null(y) && is.null(z)) {
    if (ncol(x) < 3L) {
      stop("A voxel data frame must have at least x, y, and z columns.",
           call. = FALSE)
    }
    out <- data.frame(x = x[[1L]], y = x[[2L]], z = x[[3L]])
    out$radius <- if (ncol(x) >= 4L) x[[4L]] else radius
    out$weight <- if (ncol(x) >= 5L) x[[5L]] else weight
  } else {
    validate_same_length(x = x, y = y, z = z)
    out <- data.frame(x = x, y = y, z = z)
    out$radius <- radius
    out$weight <- weight
  }
  if (length(out$radius) == 1L) {
    out$radius <- rep(out$radius, nrow(out))
  }
  if (length(out$weight) == 1L) {
    out$weight <- rep(out$weight, nrow(out))
  }
  if (length(out$radius) != nrow(out) || length(out$weight) != nrow(out)) {
    stop("`radius` and `weight` must have length 1 or match x/y/z.",
         call. = FALSE)
  }
  numeric_columns(out, c("x", "y", "z", "radius", "weight"))
}

numeric_columns <- function(data, cols) {
  for (col in cols) {
    if (!is.numeric(data[[col]])) {
      stop("Column `", col, "` must be numeric.", call. = FALSE)
    }
  }
  if (nrow(data) == 0L) {
    stop("Plot data must contain at least one row.", call. = FALSE)
  }
  data
}

write_grouped_table <- function(data, group, workdir,
                                prefix = "gunplotR-grouped-") {
  workdir <- normalizePath(ensure_directory(workdir), winslash = "/",
                           mustWork = TRUE)
  data_file <- tempfile(prefix, tmpdir = workdir, fileext = ".dat")
  lines <- character()
  for (key in unique(group)) {
    chunk <- data[group == key, , drop = FALSE]
    chunk_lines <- apply(chunk, 1L, function(row) {
      paste(format_gnuplot_number(as.numeric(row)), collapse = "\t")
    })
    lines <- c(lines, chunk_lines, "")
  }
  writeLines(lines, data_file, useBytes = TRUE)
  normalizePath(data_file, winslash = "/", mustWork = FALSE)
}

polygon_group <- function(data, group = NULL) {
  if (is.null(group)) {
    return(rep(1L, nrow(data)))
  }
  if (is.character(group) && length(group) == 1L && group %in% names(data)) {
    return(data[[group]])
  }
  if (length(group) != nrow(data)) {
    stop("`group` must be a column name or a vector matching nrow(data).",
         call. = FALSE)
  }
  group
}

bar3d_data <- function(height, x = NULL, y = NULL, color = NULL) {
  if (is.data.frame(height)) {
    if (ncol(height) < 3L) {
      stop("A data frame `height` must have x, y, and height columns.",
           call. = FALSE)
    }
    x_axis <- category_axis(height[[1L]], fallback = NULL)
    y_axis <- category_axis(height[[2L]], fallback = NULL)
    out <- data.frame(x = x_axis$positions, y = y_axis$positions,
                      z = height[[3L]])
    if (!is.null(color)) {
      out$color <- color
    } else if (ncol(height) >= 4L) {
      out$color <- height[[4L]]
    }
    return(list(data = numeric_columns(out, setdiff(names(out), "color")),
                xtics = x_axis$tics("xtics"),
                ytics = y_axis$tics("ytics")))
  }

  if (is.vector(height) && !is.matrix(height)) {
    height <- matrix(height, nrow = 1L)
  }
  if (!is.matrix(height) || !is.numeric(height)) {
    stop("`height` must be a numeric matrix, vector, or data frame.",
         call. = FALSE)
  }

  x_axis <- category_axis(x, n = ncol(height), fallback = colnames(height))
  y_axis <- category_axis(y, n = nrow(height), fallback = rownames(height))
  grid <- expand.grid(x = x_axis$positions, y = y_axis$positions)
  out <- data.frame(x = grid$x, y = grid$y, z = as.vector(t(height)))
  if (!is.null(color)) {
    if (is.matrix(color)) {
      if (!identical(dim(color), dim(height))) {
        stop("Matrix `color` must have the same dimensions as `height`.",
             call. = FALSE)
      }
      out$color <- as.vector(t(color))
    } else {
      out$color <- color
    }
  }
  if ("color" %in% names(out) && length(out$color) == 1L) {
    out$color <- rep(out$color, nrow(out))
  }
  list(data = numeric_columns(out, setdiff(names(out), "color")),
       xtics = x_axis$tics("xtics"),
       ytics = y_axis$tics("ytics"))
}

rgb_variable_values <- function(color) {
  if (is.numeric(color)) {
    return(color)
  }
  if (!is.character(color)) {
    stop("`color` must be numeric or a color name for rgb-variable coloring.",
         call. = FALSE)
  }
  rgb <- grDevices::col2rgb(color)
  as.integer(rgb[1L, ] * 65536L + rgb[2L, ] * 256L + rgb[3L, ])
}

bar3d_using_spec <- function(has_color, width, depth) {
  if (!isTRUE(has_color)) {
    return("1:2:3")
  }
  if (!is.numeric(width) || length(width) != 1L || is.na(width)) {
    stop("`width` must be a single numeric value when `color` is used.",
         call. = FALSE)
  }
  if (is.null(depth)) {
    depth <- width
  }
  if (!is.numeric(depth) || length(depth) != 1L || is.na(depth)) {
    stop("`depth` must be a single numeric value when `color` is used.",
         call. = FALSE)
  }
  paste0("1:2:3:(", gp_number(width), "):(", gp_number(depth), "):4")
}

parallel3d_data <- function(z, x = NULL, y = NULL,
                            palette_by = c("none", "y", "z")) {
  palette_by <- match.arg(palette_by)
  if (is.data.frame(z)) {
    if (ncol(z) < 3L) {
      stop("A data frame `z` must have x, y, and z columns.", call. = FALSE)
    }
    x_axis <- category_axis(z[[1L]], fallback = NULL)
    y_axis <- category_axis(z[[2L]], fallback = NULL)
    out <- data.frame(x = x_axis$positions, y = y_axis$positions,
                      z = z[[3L]], group = y_axis$positions)
  } else {
    if (is.vector(z) && !is.matrix(z)) {
      z <- matrix(z, nrow = 1L)
    }
    if (!is.matrix(z) || !is.numeric(z)) {
      stop("`z` must be a numeric matrix, vector, or data frame.",
           call. = FALSE)
    }
    x_axis <- category_axis(x, n = ncol(z), fallback = colnames(z))
    y_axis <- category_axis(y, n = nrow(z), fallback = rownames(z))
    grid <- expand.grid(x = x_axis$positions, y = y_axis$positions)
    out <- data.frame(x = grid$x, y = grid$y, z = as.vector(t(z)),
                      group = grid$y)
  }
  if (palette_by == "y") {
    out$color <- out$y
  } else if (palette_by == "z") {
    out$color <- out$z
  }
  list(data = numeric_columns(out, setdiff(names(out), "group")),
       xtics = x_axis$tics("xtics"),
       ytics = y_axis$tics("ytics"))
}

category_axis <- function(values = NULL, n = NULL, fallback = NULL) {
  if (is.null(values)) {
    if (is.null(n)) {
      stop("Internal error: category axis needs values or n.", call. = FALSE)
    }
    positions <- seq_len(n)
    labels <- fallback
  } else if (is.numeric(values)) {
    positions <- values
    labels <- fallback
  } else {
    labels <- as.character(values)
    unique_labels <- unique(labels)
    positions <- match(labels, unique_labels)
    labels <- unique_labels
  }
  if (!is.null(n) && length(positions) != n) {
    stop("Axis values must match the corresponding matrix dimension.",
         call. = FALSE)
  }
  unique_positions <- if (is.null(values) || is.numeric(values)) {
    unique(positions)
  } else {
    seq_along(labels)
  }
  make_tics <- function(axis) {
    if (is.null(labels)) {
      return(NULL)
    }
    if (length(labels) != length(unique_positions)) {
      return(NULL)
    }
    entries <- paste(
      paste(gp_quote(labels), vapply(unique_positions, gp_number,
                                     character(1))),
      collapse = ", "
    )
    paste0("set ", axis, " (", entries, ")")
  }
  list(positions = positions, tics = make_tics)
}

recycle_numeric <- function(x, n, arg) {
  if (!is.numeric(x) || anyNA(x)) {
    stop("`", arg, "` must be numeric.", call. = FALSE)
  }
  if (length(x) == 1L) {
    return(rep(x, n))
  }
  if (length(x) != n) {
    stop("`", arg, "` must have length 1 or match x/y/z.", call. = FALSE)
  }
  x
}

padded_range <- function(x, pad = 0) {
  if (length(pad) == 0L || all(is.na(pad))) {
    pad <- 0
  }
  pad <- max(abs(pad), na.rm = TRUE)
  out <- range(x, finite = TRUE)
  if (!all(is.finite(out))) {
    stop("Cannot compute finite range for voxel data.", call. = FALSE)
  }
  if (out[1L] == out[2L]) {
    pad <- max(pad, 0.5)
  }
  out + c(-pad, pad)
}
