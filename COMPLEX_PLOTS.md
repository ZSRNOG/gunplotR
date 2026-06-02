# gunplotR 复杂图绘制指南

这份文档说明如何用 `gunplotR` 在 R 中调用 `gunplot` / `gnuplot` 绘制更复杂的二维和三维图。包的默认行为是弹出窗口预览，不保存图片；只有显式传入 `output` 时才会保存。

## 目录

- [准备工作](#准备工作)
- [输出控制](#输出控制)
- [二维复杂图](#二维复杂图)
- [三维复杂图](#三维复杂图)
- [gnuplot 6.1 demo 风格图](#gnuplot-61-demo-风格图)
- [用 `extra` 加强样式](#用-extra-加强样式)
- [使用 gnuplot Plotting Styles](#使用-gnuplot-plotting-styles)
- [用 `gp_run()` 绘制任意复杂图](#用-gp_run-绘制任意复杂图)
- [推荐工作流](#推荐工作流)
- [常见问题](#常见问题)

## 准备工作

```r
library(gunplotR)

gp_available()
gp_version()
```

如果找不到外部绘图程序，先设置可执行文件路径：

```r
options(gunplotR.bin = "C:/Program Files/gnuplot/bin/gnuplot.exe")
```

Windows 下默认用 `windows` 终端预览。也可以手动指定：

```r
options(gunplotR.preview_terminal = "windows")
```

## 输出控制

默认只预览：

```r
x <- seq(0, 2 * pi, length.out = 300)
gp_line(x, sin(x), main = "Sine wave")
```

预览并保存：

```r
gp_line(x, sin(x), output = "figures/sine.png", main = "Sine wave")
```

只保存、不弹窗：

```r
gp_line(x, sin(x), output = "figures/sine.png", preview = FALSE)
```

支持的自动输出格式包括 `png`、`pdf`、`svg`、`eps`、`jpg`、`jpeg` 和 `gif`。

## 二维复杂图

### 多条函数曲线

如果曲线可以用 gnuplot 表达式表示，推荐用 `gp_function()`：

```r
gp_function(
  c("sin(x)", "cos(x)", "sin(x) * cos(x)"),
  xrange = c(-2 * pi, 2 * pi),
  main = "Multiple function curves",
  xlab = "x",
  ylab = "f(x)"
)
```

### 散点图和趋势线

散点和趋势线属于典型的多图层图形，推荐用 `gp_layer()` 和 `gp_multi()` 组合：

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
    data = data.frame(wt = df$wt, mpg = df$mpg),
    style = "points",
    title = "cars",
    pointtype = 7,
    pointsize = 1
  ),
  gp_layer(
    data = trend,
    style = "lines",
    title = "linear fit",
    linewidth = 2
  ),
  main = "MPG vs Weight",
  xlab = "Weight",
  ylab = "MPG"
)
```

### 柱状图

```r
sales <- c(Q1 = 120, Q2 = 180, Q3 = 150, Q4 = 210)

gp_bar(
  sales,
  main = "Quarterly sales",
  xlab = "Quarter",
  ylab = "Sales",
  fill = 0.75
)
```

### 直方图和密度曲线

```r
set.seed(1)
x <- c(rnorm(700, 0, 1), rnorm(300, 3, 0.7))

gp_histogram(
  x,
  bins = 40,
  frequency = FALSE,
  main = "Distribution",
  xlab = "value",
  ylab = "density"
)

gp_density(
  x,
  adjust = 0.8,
  main = "Kernel density",
  xlab = "value"
)
```

如需把直方图和密度线叠加，用 `gp_run()` 更灵活。

### 热图

矩阵热图要求 `z` 是数值矩阵，`x` 对应列，`y` 对应行：

```r
x <- seq(-3, 3, length.out = 100)
y <- seq(-3, 3, length.out = 100)
z <- outer(y, x, function(y, x) sin(x^2 + y^2) / (1 + x^2 + y^2))

gp_heatmap(
  z,
  x = x,
  y = y,
  main = "Heatmap",
  xlab = "x",
  ylab = "y",
  palette = "rgbformulae 33,13,10"
)
```

## 三维复杂图

### 三维曲面

`gp_surface()` 可以接收 R 函数：

```r
f <- function(x, y) sin(sqrt(x^2 + y^2))

gp_surface(
  f,
  x = seq(-10, 10, length.out = 100),
  y = seq(-10, 10, length.out = 100),
  type = "pm3d",
  main = "3D surface",
  xlab = "x",
  ylab = "y",
  zlab = "z"
)
```

也可以直接传 gnuplot 表达式：

```r
gp_surface(
  "sin(sqrt(x**2+y**2))",
  xrange = c(-10, 10),
  yrange = c(-10, 10),
  type = "pm3d",
  main = "Gnuplot expression surface"
)
```

### 矩阵曲面

```r
x <- seq(-4, 4, length.out = 80)
y <- seq(-4, 4, length.out = 80)
z <- outer(y, x, function(y, x) x * exp(-x^2 - y^2))

gp_surface(
  z,
  x = x,
  y = y,
  type = "surface",
  main = "Matrix surface"
)
```

### 三维点云和三维曲线

```r
theta <- seq(0, 8 * pi, length.out = 600)

gp_points3d(
  cos(theta),
  sin(theta),
  theta,
  type = "lines",
  main = "3D spiral",
  xlab = "x",
  ylab = "y",
  zlab = "t"
)
```

三列数据框也可以直接传入：

```r
cloud <- data.frame(
  x = rnorm(500),
  y = rnorm(500),
  z = rnorm(500)
)

gp_points3d(cloud, main = "3D point cloud")
```

### 等高线

```r
gp_contour(
  z,
  x = x,
  y = y,
  levels = 15,
  main = "Contour lines",
  xlab = "x",
  ylab = "y"
)
```

填充等高线：

```r
gp_contour(
  z,
  x = x,
  y = y,
  levels = 15,
  filled = TRUE,
  palette = "rgbformulae 33,13,10",
  main = "Filled contour"
)
```

## gnuplot 6.1 demo 风格图

这些函数不是把官方 demo 网页展示出来，而是把相同类型的 gnuplot 绘图能力封装成 R 函数。

### boxes3d / 三维条形图

```r
zbar <- outer(1:5, 1:4, "*")
rownames(zbar) <- paste0("R", 1:5)
colnames(zbar) <- paste0("C", 1:4)

gp_bar3d(
  zbar,
  color = zbar,
  fill = "solid 0.8 border lc black",
  depth = 0.65,
  depthorder = TRUE,
  lighting = TRUE,
  walls = TRUE,
  pm3d_border = "lc black",
  palette = "rgbformulae 33,13,10",
  main = "boxes3d-style bars",
  cblabel = "value"
)
```

### fenceplot

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
  fill = 0.6,
  col = 4,
  view = c(70, 25),
  main = "Fence plot"
)
```

### Gantt

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

### violinplot

```r
gp_violinplot(
  iris,
  group = "Species",
  value = "Sepal.Length",
  points = TRUE,
  boxplot = TRUE,
  fill = 0.45,
  col = 4,
  main = "Violin plot"
)
```

### waterfallplot

```r
x <- seq(0, 1, length.out = 100)
y <- seq(1, 40)
zw <- outer(y, x, function(y, x) dnorm(x, 0.18 + y / 260, 0.055))

gp_waterfall(
  zw,
  x = x,
  y = y,
  fill = "background",
  col = "black",
  view = c(42, 27, 1, 1.2),
  main = "Waterfall plot"
)
```

## 用 `extra` 加强样式

多数高级函数都有 `extra` 参数，可以插入额外 gnuplot 命令：

```r
gp_surface(
  f,
  type = "pm3d",
  extra = c(
    "set view 60, 35",
    "set colorbox",
    "set ticslevel 0"
  ),
  main = "Styled surface"
)
```

二维图中也可以使用：

```r
gp_function(
  "sin(x)",
  xrange = c(-2 * pi, 2 * pi),
  extra = c(
    "set zeroaxis",
    "set samples 1000",
    "set key outside"
  ),
  main = "Styled sine"
)
```

## 使用 gnuplot Plotting Styles

gnuplot 的核心语法是：

```gnuplot
plot 'data.dat' using 1:2 with lines title 'series'
```

`gunplotR` 对应提供了三层接口：

- `gp_style()`：描述 `with <style>` 以及线型、点型、填充、颜色等样式参数。
- `gp_layer()`：描述一个数据源、表达式或文件，以及该层使用的 `using` 和 style。
- `gp_multi()`：把多个 layer 合成一个 `plot` 或 `splot`。

### 常见 style 对照

| gnuplot style | 推荐接口 |
| --- | --- |
| `lines` | `gp_line()` 或 `gp_layer(style = "lines")` |
| `points` | `gp_scatter()` 或 `gp_layer(style = "points")` |
| `linespoints` | `gp_layer(style = "linespoints")` |
| `boxes` | `gp_bar()` 或 `gp_layer(style = "boxes")` |
| `yerrorbars` / `xyerrorbars` | `gp_errorbars()` |
| `vectors` | `gp_vectors()` |
| `labels` | `gp_labels()` |
| `circles` | `gp_circles()` |
| `filledcurves` | `gp_filledcurves()` 或 `gp_layer(style = "filledcurves")` |
| `boxplot` | `gp_boxplot()` |
| `candlesticks` / `financebars` | `gp_finance()` |
| `image` / `rgbimage` / `rgbalpha` | `gp_image()` |
| `pm3d` | `gp_surface(type = "pm3d")` 或 `gp_layer(style = "pm3d")` |

查看包内已知 style 名称：

```r
gp_styles()
```

### 混合线、点、填充曲线

```r
x <- seq(0, 2 * pi, length.out = 300)
y <- sin(x)
baseline <- rep(0, length(x))

gp_multi(
  gp_layer(
    data = data.frame(x = x, y = y, y2 = baseline),
    style = gp_style("filledcurves", using = "1:2:3", fill = "solid 0.25"),
    title = "area"
  ),
  gp_layer(
    x = x,
    y = y,
    style = "lines",
    title = "sin(x)",
    linewidth = 2
  ),
  gp_layer(
    x = x[seq(1, length(x), by = 25)],
    y = y[seq(1, length(y), by = 25)],
    style = "points",
    title = "samples",
    pointtype = 7,
    pointsize = 1.2
  ),
  main = "Layered plot",
  xlab = "x",
  ylab = "y"
)
```

### 双轴 layer

```r
x <- seq(0, 10, length.out = 200)

gp_multi(
  gp_layer(x = x, y = sin(x), style = "lines", title = "sin(x)",
           axes = "x1y1", linewidth = 2),
  gp_layer(x = x, y = exp(x / 4), style = "lines", title = "exp(x/4)",
           axes = "x1y2", linewidth = 2, dashtype = 2),
  main = "Two axes",
  xlab = "x",
  ylab = "sin(x)",
  extra = c(
    "set y2label 'exp(x/4)'",
    "set ytics nomirror",
    "set y2tics"
  )
)
```

### 常用 style 封装

误差棒：

```r
x <- 1:12
y <- log(x)
gp_errorbars(x, y, ymin = y - 0.15, ymax = y + 0.15, type = "yerrorlines")
```

向量场：

```r
grid <- expand.grid(x = seq(-2, 2, length.out = 12),
                    y = seq(-2, 2, length.out = 12))
gp_vectors(grid$x, grid$y, dx = -grid$y / 8, dy = grid$x / 8)
```

标签：

```r
gp_labels(1:5, c(3, 4, 2, 5, 4), labels = LETTERS[1:5])
```

圆：

```r
gp_circles(1:5, c(3, 4, 2, 5, 4), radius = seq(0.1, 0.5, length.out = 5))
```

filledcurves：

```r
x <- seq(0, 2 * pi, length.out = 300)
gp_filledcurves(x, sin(x), y2 = rep(0, length(x)), fill = "solid 0.3")
```

boxplot：

```r
set.seed(1)
value <- c(rnorm(100), rnorm(100, 1), rnorm(100, 2))
group <- rep(c("A", "B", "C"), each = 100)
gp_boxplot(value, group = group, main = "Grouped boxplot")
```

K 线或 finance bars：

```r
set.seed(1)
n <- 30
open <- cumsum(rnorm(n, 0, 0.5)) + 100
close <- open + rnorm(n, 0, 0.8)
high <- pmax(open, close) + runif(n, 0.2, 1.2)
low <- pmin(open, close) - runif(n, 0.2, 1.2)

gp_finance(1:n, open, high, low, close, type = "candlesticks")
```

### 直接使用任意 style

官方文档里的 style 如果没有专门封装，可以直接写：

```r
gp_multi(
  gp_layer(
    data = data.frame(x = 1:10, y = (1:10)^2, size = seq(0.1, 1, length.out = 10)),
    style = "circles",
    using = "1:2:3",
    title = "circles"
  )
)
```

三维 `splot` 也可以用 `dimensions = "3d"`：

```r
x <- seq(-3, 3, length.out = 40)
y <- seq(-3, 3, length.out = 40)
grid <- expand.grid(x = x, y = y)
grid$z <- with(grid, sin(x) * cos(y))

gp_multi(
  gp_layer(data = grid, style = "pm3d", using = "1:2:3", grid_data = TRUE),
  dimensions = "3d",
  main = "pm3d layer"
)
```

### image 和 rgbimage

灰度或调色板图像可以直接用矩阵：

```r
x <- seq(-2, 2, length.out = 80)
y <- seq(-2, 2, length.out = 80)
z <- outer(y, x, function(y, x) exp(-(x^2 + y^2)))

gp_image(
  z,
  x = x,
  y = y,
  style = "image",
  palette = "rgbformulae 33,13,10",
  main = "Image style"
)
```

如果已有 RGB 数据框，可以用 `rgbimage`。数据列通常是 `x, y, r, g, b`：

```r
grid <- expand.grid(x = 1:40, y = 1:40)
rgb <- transform(
  grid,
  r = x / max(x),
  g = y / max(y),
  b = 0.4
)

gp_image(rgb, style = "rgbimage", using = "1:2:3:4:5")
```

## 用 `gp_run()` 绘制任意复杂图

当需要双轴、多图层、多面板、特殊标注、拟合曲线或动画时，直接写 gnuplot 脚本最稳妥。`gp_run()` 会负责找到外部程序并运行脚本。

### 双 y 轴示例

```r
x <- seq(0, 10, length.out = 200)
df <- data.frame(x = x, y1 = sin(x), y2 = exp(x / 4))
data_file <- tempfile(fileext = ".dat")

write.table(df, data_file, sep = "\t", row.names = FALSE, col.names = FALSE, quote = FALSE)
data_file <- gsub("\\\\", "/", data_file)

gp_run(c(
  "set terminal windows",
  "set title 'Two y axes'",
  "set xlabel 'x'",
  "set ylabel 'sin(x)'",
  "set y2label 'exp(x/4)'",
  "set ytics nomirror",
  "set y2tics",
  "set grid",
  sprintf("plot '%s' using 1:2 axes x1y1 with lines lw 2 title 'sin(x)', \\", data_file),
  sprintf("     '%s' using 1:3 axes x1y2 with lines lw 2 title 'exp(x/4)'", data_file)
), persist = TRUE)
```

### 多面板图示例

```r
gp_run(c(
  "set terminal windows",
  "set multiplot layout 2,2 title 'Multiple panels'",
  "plot sin(x) with lines title 'sin(x)'",
  "plot cos(x) with lines title 'cos(x)'",
  "plot tan(x) with lines title 'tan(x)'",
  "plot exp(-x*x) with lines title 'exp(-x^2)'",
  "unset multiplot"
), persist = TRUE)
```

## 推荐工作流

1. 简单单层图优先用 `gp_line()`、`gp_scatter()`、`gp_bar()`、`gp_heatmap()`。
2. 三维曲面、等高线、点云优先用 `gp_surface()`、`gp_contour()`、`gp_points3d()`。
3. 需要调色板、视角、采样数、坐标轴细节时，加 `extra`。
4. 需要多图层、双轴、多面板或精细排版时，用 `gp_run()` 写完整 gnuplot 脚本。
5. 最后确认无误后再加 `output = "file.png"` 保存图片。

## 常见问题

### 找不到绘图程序

```r
gp_available()
gp_executable(must_work = FALSE)
```

如果返回 `FALSE` 或 `NA`，请安装 gnuplot 并设置：

```r
options(gunplotR.bin = "C:/Program Files/gnuplot/bin/gnuplot.exe")
```

### 默认保存了图片吗

不会。默认 `output = NULL`，只弹窗预览。只有显式传入 `output` 时才保存。

### 如何查看生成的 gnuplot 脚本

加 `echo = TRUE`：

```r
gp_surface(f, type = "pm3d", echo = TRUE)
```

### 如何导出高分辨率图片

手动设置 terminal：

```r
gp_surface(
  f,
  type = "pm3d",
  output = "surface.png",
  terminal = "pngcairo size 1600,1000 enhanced font 'Arial,12'",
  preview = FALSE
)
```
