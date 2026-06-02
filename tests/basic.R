library(gunplotR)

stopifnot(is.logical(gp_available()))
stopifnot(length(gp_available()) == 1L)
stopifnot(is.character(gp_executable(must_work = FALSE)))
stopifnot(is.character(gp_detect_executable()))
stopifnot(is.function(gp_arrowstyle))
stopifnot(is.function(gp_configure))
stopifnot(is.function(gp_linetype))
stopifnot(is.function(gp_linestyle))
stopifnot(is.function(gp_options))
stopifnot(is.function(gp_scatter))
stopifnot(is.function(gp_styles))
stopifnot(is.function(gp_style))
stopifnot(is.function(gp_terminal))
stopifnot(is.function(gp_layer))
stopifnot(is.function(gp_multi))
stopifnot(is.function(gp_theme))
stopifnot(is.function(gp_errorbars))
stopifnot(is.function(gp_vectors))
stopifnot(is.function(gp_labels))
stopifnot(is.function(gp_circles))
stopifnot(is.function(gp_line))
stopifnot(is.function(gp_bar))
stopifnot(is.function(gp_histogram))
stopifnot(is.function(gp_surface))
stopifnot(is.function(gp_points3d))
stopifnot(is.function(gp_heatmap))
stopifnot(is.function(gp_contour))
stopifnot(is.function(gp_fenceplot))
stopifnot(is.function(gp_gantt))
stopifnot(is.function(gp_violinplot))
stopifnot(is.function(gp_waterfall))
stopifnot(is.function(gp_splot))
stopifnot(is.function(gp_surface4d))
stopifnot(is.function(gp_points4d))
stopifnot(is.function(gp_map3d))
stopifnot(is.function(gp_parametric_surface))
stopifnot(is.function(gp_boxes3d))
stopifnot(is.function(gp_bar3d))
stopifnot(is.function(gp_parallel3d))
stopifnot(is.function(gp_vectors3d))
stopifnot(is.function(gp_labels3d))
stopifnot(is.function(gp_polygons3d))
stopifnot(is.function(gp_voxels))
stopifnot(is.function(gp_isosurface))

style <- gp_style("linespoints", col = 2, lwd = 2, lty = 2, pch = 7,
                  cex = 1.2)
clause <- gunplotR:::style_clause(style)
stopifnot(grepl("lw 2", clause, fixed = TRUE))
stopifnot(grepl("dt 2", clause, fixed = TRUE))
stopifnot(grepl("pt 7", clause, fixed = TRUE))
stopifnot(grepl("ps 1.2", clause, fixed = TRUE))
stopifnot(grepl("lc rgb", clause, fixed = TRUE))

label_style <- gp_style("labels", text_col = 2, font = "Arial,10",
                        rotate = 45, offset = c(1, 0), pch = 7)
label_clause <- gunplotR:::style_clause(label_style)
stopifnot(grepl("tc rgb", label_clause, fixed = TRUE))
stopifnot(grepl("font 'Arial,10'", label_clause, fixed = TRUE))
stopifnot(grepl("rotate by 45", label_clause, fixed = TRUE))
stopifnot(grepl("offset 1,0", label_clause, fixed = TRUE))

opts <- gp_options(xlog = TRUE, xtics = c(A = 1, B = 2),
                   yformat = "%b\\n%Y",
                   style_fill = "solid 0.4 border lc black",
                   set = list(encoding = "utf8"),
                   mouse = TRUE)
stopifnot(any(grepl("set logscale x", opts, fixed = TRUE)))
stopifnot(any(grepl("set xtics ('A' 1, 'B' 2)", opts, fixed = TRUE)))
stopifnot(any(grepl("set format y '%b\\n%Y'", opts, fixed = TRUE)))
stopifnot(any(grepl("set style fill solid 0.4", opts, fixed = TRUE)))

term <- gp_terminal("pngcairo", width = 1200, height = 800,
                    font = "Arial", font_size = 12)
stopifnot(grepl("size 1200,800", term, fixed = TRUE))
stopifnot(grepl("font 'Arial,12'", term, fixed = TRUE))

stopifnot(grepl("set linetype 7", gp_linetype(7, col = 2, lwd = 2),
                fixed = TRUE))
stopifnot(grepl("set style line 8", gp_linestyle(8, col = 4),
                fixed = TRUE))
stopifnot(grepl("set style arrow 3", gp_arrowstyle(3, col = 2),
                fixed = TRUE))

theme <- gp_theme(font_family = "Arial", title_size = 16, tick_size = 9,
                  legend_position = "top right", legend_box = TRUE,
                  grid_col = 8, grid_lty = 2)
commands <- gunplotR:::gp_setup(main = "Title", subtitle = "Subtitle",
                                theme = theme)
stopifnot(any(grepl("set title", commands, fixed = TRUE)))
stopifnot(any(grepl("set label 999", commands, fixed = TRUE)))
stopifnot(any(grepl("set key top right box", commands, fixed = TRUE)))

setup3d <- gunplotR:::gp_3d_setup(view = c(60, 30), pm3d = TRUE,
                                  cblabel = "value", contour = TRUE)
stopifnot(any(grepl("set view 60,30", setup3d, fixed = TRUE)))
stopifnot(any(grepl("set pm3d", setup3d, fixed = TRUE)))
stopifnot(any(grepl("set contour base", setup3d, fixed = TRUE)))

xyzc <- gunplotR:::xyzc_points_data(1:3, 2:4, 3:5, 4:6)
stopifnot(identical(names(xyzc), c("x", "y", "z", "color")))

bars <- gunplotR:::bar3d_data(matrix(1:6, nrow = 2L))
stopifnot(all(c("x", "y", "z") %in% names(bars$data)))

fences <- gunplotR:::fence_data(matrix(1:6, nrow = 2L))
stopifnot(all(c("x", "y", "zmin", "zlow", "z", "group") %in% names(fences)))

gantt <- gunplotR:::gantt_data(
  task = c("A", "B"),
  start = as.Date(c("2026-01-01", "2026-02-01")),
  end = as.Date(c("2026-01-20", "2026-03-01")),
  date_format = "%Y-%m-%d"
)
stopifnot(all(c("task", "start", "end") %in% names(gantt)))

violins <- gunplotR:::violin_data(group = iris$Species,
                                  value = iris$Sepal.Length)
stopifnot(all(c("density", "raw", "groups") %in% names(violins)))

waterfall <- gunplotR:::waterfall_data(matrix(1:6, nrow = 2L))
stopifnot(all(c("x", "y", "z") %in% names(waterfall)))

parallel <- gunplotR:::parallel3d_data(matrix(1:6, nrow = 2L),
                                       palette_by = "y")
stopifnot(all(c("x", "y", "z", "group", "color") %in% names(parallel$data)))

stopifnot(length(gunplotR:::recycle_numeric(0, 3L, "dz")) == 3L)
