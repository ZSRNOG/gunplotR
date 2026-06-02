# gunplotR 使用文档

`gunplotR` 是一个从 R 调用外部 `gnuplot` / `gunplot` 绘图程序的包。它的默认行为是：

- 默认弹出窗口预览图形。
- 默认不保存图片。
- 只有传入 `output = "xxx.png"`、`"xxx.pdf"`、`"xxx.svg"` 等路径时才保存。
- 常用美化参数尽量使用 R 风格，例如 `col`、`lwd`、`lty`、`pch`、`cex`。
- gnuplot 6 的通用 `set` 命令可以用 `settings = gp_options(...)` 或 `extra = gp_options(...)` 映射。
- 复杂图形可以用 `gp_layer()` + `gp_multi()` 分层，也可以用 `gp_run()` 直接运行原生 gnuplot 脚本。

## 1. 安装和配置

```r
install.packages(
  "C:/Users/zsr/Documents/gunplotR/gunplotR_0.1.0.tar.gz",
  repos = NULL,
  type = "source"
)
```

加载包：

```r
library(gunplotR)
```

加载时包会自动检测常见的 gnuplot 安装路径。如果没有检测到，可以手动设置：

```r
options(gunplotR.bin = "C:/Program Files/gnuplot/bin/gnuplot.exe")
```

检查是否可用：

```r
gp_detect_executable()
gp_available()
gp_version()
```

如果 Windows 上还没有安装 gnuplot，可以在 PowerShell 中运行：

```powershell
winget install -e --id gnuplot.gnuplot
```

## 2. 输出和预览

默认只预览，不保存：

```r
x <- seq(0, 2 * pi, length.out = 300)
gp_line(x, sin(x), main = "Sine wave")
```

预览并保存：

```r
gp_line(
  x, sin(x),
  output = "figures/sine.png",
  main = "Sine wave"
)
```

只保存，不弹窗：

```r
gp_line(
  x, sin(x),
  output = "figures/sine.png",
  preview = FALSE
)
```

指定输出尺寸：

```r
gp_line(
  x, sin(x),
  output = "figures/sine-large.png",
  terminal = "pngcairo size 1400,900",
  preview = FALSE
)
```

## 3. 基础二维图

### gnuplot 通用设置

```r
gp_line(
  1:100,
  (1:100)^2,
  settings = gp_options(
    xlog = TRUE,
    ylog = TRUE,
    xtics = c(1, 10, 100),
    yformat = "%.0e",
    key = "outside right",
    style_fill = "solid 0.4 border lc black"
  ),
  main = "Log-log plot",
  xlab = "x",
  ylab = "x^2"
)
```

终端参数也可以用 R 风格写：

```r
gp_line(
  x,
  sin(x),
  terminal = gp_terminal("pngcairo", width = 1200, height = 800,
                         font = "Arial", font_size = 12),
  output = "figures/sine-large.png",
  preview = FALSE
)
```

### 折线图

```r
gp_line(
  x, sin(x),
  col = 2,
  lwd = 2,
  lty = 1,
  main = "Sine wave",
  subtitle = "Styled with R-like arguments",
  xlab = "x",
  ylab = "sin(x)",
  legend = "sin(x)"
)
```

### 散点图

```r
gp_scatter(
  mtcars$wt,
  mtcars$mpg,
  pch = 7,
  cex = 1.3,
  col = "#3366cc",
  main = "MPG vs Weight",
  xlab = "Weight",
  ylab = "MPG"
)
```

### 函数曲线

```r
gp_function(
  c("sin(x)", "cos(x)", "sin(x) * cos(x)"),
  xrange = c(-2 * pi, 2 * pi),
  col = c(2, 4, 3),
  lwd = c(2, 2, 1.5),
  lty = c(1, 2, 3),
  main = "Multiple functions",
  xlab = "x",
  ylab = "f(x)"
)
```

## 4. 主题和美化

`gp_theme()` 可以集中设置字体、标题、刻度、图例、网格和边框。

```r
paper_theme <- gp_theme(
  font_family = "Arial",
  title_size = 16,
  subtitle_size = 10,
  axis_size = 12,
  tick_size = 10,
  legend_size = 10,
  legend_position = "top right",
  legend_box = TRUE,
  grid_col = "gray80",
  grid_lty = 2,
  border_lwd = 1,
  border_col = "gray30",
  background = "white"
)

gp_line(
  x, sin(x),
  col = 2,
  lwd = 2,
  main = "Sine wave",
  subtitle = "Using a shared theme",
  xlab = "x",
  ylab = "sin(x)",
  legend = "sin(x)",
  theme = paper_theme
)
```

常用美化参数：

| 参数 | 作用 |
| --- | --- |
| `col`, `color` | 线和点的颜色，支持数字、颜色名、十六进制颜色 |
| `line_col` | 线颜色 |
| `point_col` | 点颜色 |
| `lwd` | 线宽 |
| `lty` | 线型 |
| `pch` | 点形 |
| `cex` | 点大小 |
| `main` | 主标题 |
| `subtitle` | 副标题 |
| `legend` | 图例文字 |
| `theme` | 主题对象 |

## 5. 多图层复杂二维图

### 散点加回归线

```r
df <- mtcars
fit <- lm(mpg ~ wt, data = df)
xline <- seq(min(df$wt), max(df$wt), length.out = 100)
trend <- data.frame(
  wt = xline,
  mpg = predict(fit, data.frame(wt = xline))
)

gp_multi(
  gp_layer(
    x = df$wt,
    y = df$mpg,
    style = "points",
    title = "cars",
    col = 4,
    pch = 7,
    cex = 1.2
  ),
  gp_layer(
    data = trend,
    style = "lines",
    title = "linear fit",
    col = 2,
    lwd = 2
  ),
  main = "MPG vs Weight",
  subtitle = "Scatter plot with fitted line",
  xlab = "Weight",
  ylab = "MPG",
  theme = paper_theme
)
```

### 误差棒图

```r
x <- 1:12
y <- log(x)

gp_errorbars(
  x, y,
  ymin = y - 0.15,
  ymax = y + 0.15,
  type = "yerrorlines",
  col = 2,
  lwd = 2,
  pch = 7,
  main = "Error bars",
  xlab = "Index",
  ylab = "Value",
  theme = paper_theme
)
```

### 双 y 轴图

`gp_layer()` 支持 `axes = "x1y2"`，可以配合 `extra` 手动开启 y2 轴。

```r
x <- seq(0, 10, length.out = 200)

gp_multi(
  gp_layer(
    x = x,
    y = sin(x),
    style = "lines",
    title = "sin(x)",
    col = 2,
    lwd = 2
  ),
  gp_layer(
    x = x,
    y = 100 * cos(x),
    style = "lines",
    title = "100 cos(x)",
    axes = "x1y2",
    col = 4,
    lwd = 2,
    lty = 2
  ),
  main = "Dual y-axis plot",
  xlab = "x",
  ylab = "sin(x)",
  extra = c(
    "set y2label '100 cos(x)'",
    "set y2tics",
    "set ytics nomirror"
  ),
  theme = paper_theme
)
```

### 填充区间图

```r
x <- seq(0, 2 * pi, length.out = 300)
y1 <- sin(x)
y2 <- sin(x) + 0.4

gp_multi(
  gp_layer(
    data = data.frame(x = x, y1 = y1, y2 = y2),
    style = gp_style(
      "filledcurves",
      using = "1:2:3",
      title = "band",
      fill = 0.25,
      col = "#99ccee"
    )
  ),
  gp_layer(
    x = x,
    y = y1,
    style = "lines",
    title = "lower",
    col = 4,
    lwd = 2
  ),
  gp_layer(
    x = x,
    y = y2,
    style = "lines",
    title = "upper",
    col = 2,
    lwd = 2
  ),
  main = "Filled band",
  xlab = "x",
  ylab = "y",
  theme = paper_theme
)
```

## 6. 常用统计图

### 柱状图

```r
gp_bar(
  c(A = 3, B = 7, C = 5, D = 9),
  fill = 0.65,
  col = 3,
  lwd = 1.2,
  main = "Bar plot",
  ylab = "Value",
  theme = paper_theme
)
```

### 直方图

```r
set.seed(1)
gp_histogram(
  rnorm(1000),
  bins = 30,
  fill = 0.55,
  col = 4,
  lwd = 1,
  main = "Histogram",
  theme = paper_theme
)
```

### 密度图

```r
gp_density(
  rnorm(1000),
  col = 2,
  lwd = 2,
  main = "Density plot",
  theme = paper_theme
)
```

### 箱线图

```r
gp_boxplot(
  iris$Sepal.Length,
  group = iris$Species,
  main = "Sepal length by species",
  ylab = "Sepal length",
  col = 4,
  theme = paper_theme
)
```

## 7. 热图和等高线

```r
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
z <- outer(y, x, function(y, x) x * exp(-x^2 - y^2))

gp_heatmap(
  z,
  x = x,
  y = y,
  palette = "rgbformulae 33,13,10",
  cblabel = "value",
  main = "Heatmap",
  xlab = "x",
  ylab = "y"
)

gp_contour(
  z,
  x = x,
  y = y,
  levels = 12,
  col = 4,
  lwd = 1.5,
  main = "Contour lines"
)

gp_contour(
  z,
  x = x,
  y = y,
  filled = TRUE,
  levels = 12,
  palette = "rgbformulae 22,13,-31",
  main = "Filled contour"
)
```

## 8. 三维曲面和三维点线图

### 三维曲面

```r
f <- function(x, y) sin(sqrt(x^2 + y^2))

gp_surface(
  f,
  type = "lines",
  hidden3d = TRUE,
  main = "Hidden-line surface",
  xlab = "x",
  ylab = "y",
  zlab = "z",
  view = c(60, 35),
  samples = 80,
  isosamples = 80
)
```

### 彩色 pm3d 曲面

```r
gp_surface(
  "sin(sqrt(x**2+y**2))",
  type = "pm3d",
  xrange = c(-10, 10),
  yrange = c(-10, 10),
  samples = 100,
  isosamples = 100,
  palette = "rgbformulae 33,13,10",
  cblabel = "height",
  view = c(55, 35)
)
```

### 三维螺旋线

```r
theta <- seq(0, 8 * pi, length.out = 500)

gp_points3d(
  cos(theta),
  sin(theta),
  theta,
  type = "lines",
  col = 2,
  lwd = 2,
  main = "3D spiral",
  xlab = "x",
  ylab = "y",
  zlab = "theta",
  view = c(65, 30)
)
```

## 9. 四维图：用颜色表示第四维

### 四维曲面

```r
x <- seq(-3, 3, length.out = 90)
y <- seq(-3, 3, length.out = 90)
z <- outer(y, x, function(y, x) sin(x) * cos(y))
color <- outer(y, x, function(y, x) x^2 + y^2)

gp_surface4d(
  z,
  color,
  x = x,
  y = y,
  main = "4D surface",
  cblabel = "radius^2",
  view = c(55, 35),
  pm3d = TRUE
)
```

### 四维点云

```r
theta <- seq(0, 8 * pi, length.out = 600)

gp_points4d(
  cos(theta),
  sin(theta),
  theta,
  theta,
  pch = 7,
  cex = 0.8,
  cblabel = "theta",
  main = "4D helix",
  view = c(60, 35)
)
```

### 三维投影为二维色彩图

```r
gp_map3d(
  z,
  color,
  x = x,
  y = y,
  main = "pm3d map projection",
  palette = "rgbformulae 22,13,-31",
  cblabel = "color"
)
```

## 10. 三维条形图和平行折线图

### 三维条形图

```r
zbar <- outer(1:5, 1:4, "*")
rownames(zbar) <- paste0("R", 1:5)
colnames(zbar) <- paste0("C", 1:4)

gp_bar3d(
  zbar,
  main = "3D bar chart",
  xlab = "Column",
  ylab = "Row",
  zlab = "Value",
  view = c(60, 35),
  fill = 0.7
)
```

### 彩色三维条形图

```r
gp_bar3d(
  zbar,
  color = zbar,
  palette = "rgbformulae 33,13,10",
  cblabel = "Value",
  main = "Colored 3D bar chart",
  view = c(60, 35),
  depth = 0.65,
  depthorder = TRUE,
  lighting = TRUE,
  walls = TRUE,
  pm3d_border = "lc black"
)
```

如果希望颜色不经过 palette，而是直接使用 R 的颜色名：

```r
bar_cols <- matrix(
  rep(c("tomato", "gold", "forestgreen", "royalblue"), each = 5),
  nrow = 5
)

gp_bar3d(
  zbar,
  color = bar_cols,
  color_mode = "rgb_variable",
  fill = "solid 0.8 border lc black",
  main = "RGB 3D bars",
  view = c(60, 35),
  depth = 0.6,
  walls = "x0 y0 z0",
  lighting = TRUE,
  depthorder = TRUE
)
```

### Fence plot

```r
x <- seq(-5, 5, length.out = 60)
y <- seq(-4, 4, length.out = 9)
zfence <- outer(y, x, function(y, x) {
  r <- sqrt(x^2 + y^2)
  ifelse(r == 0, 1, sin(r) / r)
})

gp_fenceplot(
  zfence,
  x = x,
  y = y,
  main = "Fence plot",
  xlab = "x",
  ylab = "scan",
  zlab = "height",
  fill = 0.6,
  view = c(70, 25)
)
```

### Waterfall plot

```r
x <- seq(0, 1, length.out = 100)
y <- seq(1, 40)
zw <- outer(y, x, function(y, x) dnorm(x, 0.18 + y / 260, 0.055))

gp_waterfall(
  zw,
  x = x,
  y = y,
  main = "Waterfall plot",
  xlab = "x",
  ylab = "scan",
  zlab = "density",
  fill = "background",
  col = "black"
)
```

### Gantt 图

```r
tasks <- data.frame(
  task = c("Design", "Prototype", "Validation", "Release"),
  start = as.Date(c("2026-01-01", "2026-02-01", "2026-03-10", "2026-04-20")),
  end = as.Date(c("2026-01-31", "2026-03-05", "2026-04-15", "2026-05-10"))
)

gp_gantt(
  tasks,
  task = "task",
  start = "start",
  end = "end",
  main = "Project plan",
  xlab = "Date"
)
```

### Violin plot

```r
gp_violinplot(
  iris,
  group = "Species",
  value = "Sepal.Length",
  points = TRUE,
  boxplot = TRUE,
  col = 4,
  fill = 0.45,
  main = "Sepal length by species",
  xlab = "Species",
  ylab = "Sepal length"
)
```

### 3D 平行折线图

```r
x <- seq(0, 2 * pi, length.out = 80)
zlines <- rbind(sin(x), cos(x), sin(x) * cos(x))
rownames(zlines) <- c("sin", "cos", "mix")

gp_parallel3d(
  zlines,
  x = x,
  main = "3D parallel lines",
  xlab = "x",
  ylab = "series",
  zlab = "value",
  view = c(65, 35),
  lwd = 2
)
```

用 y 轴位置给不同折线着色：

```r
gp_parallel3d(
  zlines,
  x = x,
  palette_by = "y",
  cblabel = "series",
  main = "Colored parallel lines",
  view = c(65, 35),
  lwd = 2
)
```

## 11. 三维向量、标签和多边形

### 三维向量场

```r
grid <- expand.grid(
  x = seq(-1, 1, length.out = 5),
  y = seq(-1, 1, length.out = 5),
  z = seq(-1, 1, length.out = 5)
)

gp_vectors3d(
  grid$x,
  grid$y,
  grid$z,
  -grid$y,
  grid$x,
  0,
  col = 4,
  main = "3D vector field",
  view = c(65, 35)
)
```

### 三维标签

```r
gp_labels3d(
  1:3,
  1:3,
  1:3,
  labels = c("A", "B", "C"),
  col = 2,
  main = "3D labels"
)
```

### 三维多边形

```r
square <- data.frame(
  x = c(0, 1, 1, 0, 0, 1, 1, 0),
  y = c(0, 0, 1, 1, 0, 0, 1, 1),
  z = c(0, 0, 0, 0, 1, 1, 1, 1),
  face = rep(c("bottom", "top"), each = 4)
)

gp_polygons3d(
  square,
  group = "face",
  fill = 0.4,
  hidden3d = TRUE,
  main = "Grouped polygons"
)
```

## 12. 体素和等值面

### 体素点云

```r
grid <- expand.grid(
  x = seq(-1, 1, length.out = 8),
  y = seq(-1, 1, length.out = 8),
  z = seq(-1, 1, length.out = 8)
)
value <- with(grid, x^2 + y^2 + z^2)

gp_voxels(
  grid$x,
  grid$y,
  grid$z,
  value,
  cex = 0.8,
  cblabel = "radius^2",
  main = "Voxel cloud"
)
```

### 等值面

```r
theta <- seq(0, 6 * pi, length.out = 300)

gp_isosurface(
  cos(theta),
  sin(theta),
  theta / 5,
  radius = 0.25,
  weight = 1,
  level = 0.15,
  grid_size = 50,
  main = "Voxel isosurface"
)
```

## 13. 直接运行 gnuplot 脚本

当你需要使用还没有封装的 gnuplot 功能时，可以直接用 `gp_run()`：

```r
gp_run(
  c(
    "set terminal windows",
    "set title 'Raw gnuplot script'",
    "set grid",
    "plot sin(x) with lines lw 2 title 'sin(x)'"
  ),
  persist = TRUE
)
```

保存原生脚本输出：

```r
gp_run(
  c(
    "set terminal pngcairo size 1200,800",
    "set output 'figures/raw-script.png'",
    "set title 'Raw gnuplot script'",
    "plot sin(x) with lines lw 2 title 'sin(x)'",
    "unset output"
  )
)
```

## 14. 推荐工作流

1. 简单二维图：优先用 `gp_line()`、`gp_scatter()`、`gp_bar()`、`gp_histogram()`。
2. 多条曲线或多种样式混合：用 `gp_layer()` + `gp_multi()`。
3. 三维曲面：用 `gp_surface()` 或 `gp_surface4d()`。
4. 三维点云：用 `gp_points3d()` 或 `gp_points4d()`。
5. 三维条形图和平行折线图：用 `gp_bar3d()` 和 `gp_parallel3d()`。
6. gnuplot demo 风格复杂图：用 `gp_fenceplot()`、`gp_waterfall()`、`gp_gantt()` 和 `gp_violinplot()`。
7. 体数据和等值面：用 `gp_voxels()` 和 `gp_isosurface()`。
8. gnuplot 特殊功能：用 `gp_run()` 或在 `extra` 中传入原生命令。

更多专项说明：

- `PLOT_AESTHETICS.md`：图形美化参数。
- `THREE_D_FOUR_D_PLOTS.md`：3D 和 4D 绘图。
- `COMPLEX_PLOTS.md`：更多复杂绘图示例。
- `GNUPLOT_FEATURE_MAP.md`：gnuplot 6 参考文档到 R 风格参数的映射。
