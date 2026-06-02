# gunplotR 对 gnuplot 6 功能的 R 风格映射

本文件根据本地 `Gnuplot_6.pdf` 和 gnuplot 6.1 官方 demo 索引整理。
目标不是逐字复制每一条 gnuplot 命令，而是提供三层接口：

1. 常用图形函数：`gp_line()`、`gp_surface()`、`gp_bar3d()`、`gp_violinplot()` 等。
2. 通用图层接口：`gp_layer()` + `gp_multi()`，可使用几乎所有 `with <style>`。
3. 通用设置接口：`gp_options()`、`gp_terminal()`、`gp_linetype()`、`gp_linestyle()`、`gp_arrowstyle()`。

如果某个 gnuplot 命令还没有专门参数，可以用 `gp_options(set = ...)`、`gp_options(unset = ...)`
或 `gp_options(raw = ...)` 直接平移。

## 1. 输出终端

```r
x <- seq(0, 2 * pi, length.out = 300)

gp_line(
  x,
  sin(x),
  terminal = gp_terminal(
    "pngcairo",
    width = 1200,
    height = 800,
    font = "Arial",
    font_size = 12,
    background = "white"
  ),
  output = "figures/sine.png",
  preview = FALSE
)
```

`gp_terminal()` 生成的是 gnuplot 的 `set terminal ...` 后半部分，因此也可用于
`preview_terminal`。

## 2. 坐标轴、刻度、格式和 log scale

```r
gp_line(
  1:100,
  (1:100)^2,
  settings = gp_options(
    xlog = TRUE,
    ylog = TRUE,
    xlim = c(1, 100),
    ylim = c(1, 10000),
    xtics = c(1, 10, 100),
    yformat = "%.0e",
    grid = "xtics ytics",
    key = "outside right"
  ),
  main = "Log-log plot",
  xlab = "x",
  ylab = "x^2"
)
```

命名刻度会自动转换成 gnuplot 的 `set xtics ("label" value, ...)`：

```r
gp_bar(
  c(3, 7, 5),
  names = c("A", "B", "C"),
  extra = gp_options(xtics = c(A = 1, B = 2, C = 3))
)
```

## 3. 时间轴

```r
dates <- as.Date("2026-01-01") + 0:30
y <- cumsum(rnorm(length(dates)))

gp_line(
  as.numeric(dates),
  y,
  settings = gp_options(
    xdata = "time",
    timefmt = "%s",
    xformat = "%b\\n%d"
  ),
  main = "Time axis"
)
```

## 4. 线型、点型、填充和标签样式

`gp_style()` 现在支持更多 R 风格参数：

```r
df <- data.frame(x = 1:5, y = c(2, 4, 3, 5, 6), label = LETTERS[1:5])

gp_multi(
  gp_layer(
    data = df,
    style = "linespoints",
    using = "1:2",
    title = "series",
    col = 4,
    lwd = 2,
    lty = 2,
    pch = 7,
    cex = 1.2
  ),
  gp_layer(
    data = df,
    style = "labels",
    using = "1:2:3",
    title = FALSE,
    text_col = 2,
    font = "Arial,10",
    rotate = 20,
    offset = c(1, 0)
  )
)
```

填充样式示例：

```r
gp_bar(
  c(4, 8, 6),
  fill = "solid 0.7",
  col = "black",
  extra = gp_options(style_fill = "solid 0.7 border lc black")
)
```

## 5. 可复用 linetype / linestyle / arrowstyle

```r
gp_function(
  c("sin(x)", "cos(x)"),
  xrange = c(-pi, pi),
  settings = gp_options(
    linestyle = c(
      gp_linestyle(1, col = 2, lwd = 2, lty = 1),
      gp_linestyle(2, col = 4, lwd = 2, lty = 2)
    )
  ),
  col = c(2, 4),
  lwd = 2
)
```

箭头样式：

```r
gp_vectors(
  1:5,
  1:5,
  dx = rep(0.6, 5),
  dy = rep(0.2, 5),
  settings = gp_options(
    arrowstyle = gp_arrowstyle(1, head = TRUE, filled = TRUE, col = 2, lwd = 1.5)
  ),
  extra = "set style arrow 1 head filled lc rgb 'red'"
)
```

## 6. 3D、palette、pm3d、contour

```r
f <- function(x, y) sin(sqrt(x^2 + y^2))

gp_surface(
  f,
  type = "pm3d",
  settings = gp_options(
    view = c(55, 35),
    palette = "rgbformulae 33,13,10",
    colorbox = TRUE,
    contour = "base",
    hidden3d = FALSE,
    samples = 80,
    isosamples = 80
  ),
  main = "pm3d with contour"
)
```

## 7. polar、parametric、mapping

```r
gp_function(
  "sin(5*t)",
  settings = gp_options(
    polar = TRUE,
    angles = "degrees",
    trange = c(0, 360),
    samples = 720,
    size = c(1, 1),
    square = TRUE
  ),
  main = "Polar curve"
)
```

## 8. 原生命令平移

所有尚未封装的 `set` 命令都可以这样写：

```r
gp_line(
  x,
  sin(x),
  settings = gp_options(
    set = list(
      encoding = "utf8",
      autoscale = "xy",
      decimalsign = "locale"
    ),
    unset = c("border"),
    raw = c(
      "set object 1 rectangle from graph 0,0 to graph 1,1 behind",
      "set object 1 fillstyle solid 1 fillcolor rgb '#f7f7f7'"
    )
  )
)
```

也可以把额外 named arguments 当作 `set` 命令：

```r
gp_options(mouse = TRUE, clip = "two", style_increment = "user")
```

会生成：

```gnuplot
set mouse
set clip two
set style_increment user
```

## 9. 建议

- 常规图：优先使用 `gp_line()`、`gp_scatter()`、`gp_surface()` 等高层函数。
- 多图层图：使用 `gp_layer()` + `gp_multi()`。
- 大量 gnuplot `set` 参数：使用 `settings = gp_options(...)`。
- 极特殊语法：使用 `extra = gp_options(raw = ...)` 或 `gp_run()`。
