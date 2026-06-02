# gunplotR 3D 和 4D 绘图指南

这个文件整理 `gunplotR` 中对应 gnuplot 6.1 官方 demo 的三维和四维绘图能力。
这里的“4D”通常表示：前三列是 `x/y/z` 空间坐标，第四列通过 palette 映射为颜色。

## 自动检测 gnuplot

加载包时会自动检测常见安装位置，例如：

```r
library(gunplotR)

gp_detect_executable()
gp_configure()
```

如果没有安装，Windows PowerShell 可以运行：

```powershell
winget install -e --id gnuplot.gnuplot
```

如果安装后仍不在 PATH 中，可以手动指定：

```r
options(gunplotR.bin = "C:/Program Files/gnuplot/bin/gnuplot.exe")
```

## 3D 曲面

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

## pm3d 彩色曲面

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

## 4D 曲面

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

也可以直接用 gnuplot 表达式：

```r
gp_surface4d(
  "sin(sqrt(x**2+y**2))",
  "x*y",
  xrange = c(-8, 8),
  yrange = c(-8, 8),
  samples = 100,
  isosamples = 100,
  cblabel = "x*y"
)
```

## 2D 投影和热图

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

gp_heatmap(
  z,
  x = x,
  y = y,
  main = "Image heatmap",
  palette = "rgbformulae 33,13,10",
  cblabel = "value"
)
```

## 等高线

```r
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

## 4D 点云

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

## 参数曲面

```r
gp_parametric_surface(
  "cos(u) * cos(v)",
  "sin(u) * cos(v)",
  "sin(v)",
  urange = c(-pi, pi),
  vrange = c(-pi / 2, pi / 2),
  samples = 80,
  isosamples = 40,
  hidden3d = TRUE,
  main = "Parametric sphere"
)
```

## 3D bar chart / boxes3d

```r
zbar <- outer(1:5, 1:4, "*")
rownames(zbar) <- paste0("R", 1:5)
colnames(zbar) <- paste0("C", 1:4)

gp_bar3d(
  zbar,
  color = zbar,
  main = "3D bar chart",
  view = c(60, 35),
  fill = "solid 0.75 border lc black",
  depth = 0.65,
  depthorder = TRUE,
  lighting = TRUE,
  walls = TRUE,
  pm3d_border = "lc black",
  palette = "rgbformulae 33,13,10",
  cblabel = "value"
)
```

`color_mode = "rgb_variable"` 可以直接使用 R 颜色名或十六进制颜色：

```r
bar_cols <- matrix(
  rep(c("tomato", "gold", "forestgreen", "royalblue"), each = 5),
  nrow = 5
)

gp_bar3d(
  zbar,
  color = bar_cols,
  color_mode = "rgb_variable",
  main = "RGB-colored 3D bars",
  depth = 0.6,
  walls = "x0 y0 z0",
  lighting = TRUE,
  depthorder = TRUE
)
```

## 3D boxes

```r
xb <- rep(1:4, each = 4)
yb <- rep(1:4, 4)
zb <- as.vector(outer(1:4, 1:4, "+"))

gp_boxes3d(
  xb,
  yb,
  zb,
  color = zb,
  cblabel = "height",
  view = c(60, 30),
  main = "3D boxes"
)
```

## Fence plot

矩阵的每一行会变成 y 方向上的一片 fence，默认用 gnuplot 的 `zerrorfill`
样式填充。

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
  col = 4,
  view = c(70, 25),
  depthorder = TRUE
)
```

只画线框也可以：

```r
gp_fenceplot(zfence, x = x, y = y, type = "lines", lwd = 1.5)
```

## Waterfall plot

`gp_waterfall()` 会把矩阵的每一行作为一条 scan line，并按 y 值从后往前绘制。

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
  col = "black",
  view = c(42, 27, 1, 1.2),
  xyplane = 0
)
```

## 3D parallel lines

```r
x <- seq(0, 2 * pi, length.out = 80)
zlines <- rbind(sin(x), cos(x), sin(x) * cos(x))
rownames(zlines) <- c("sin", "cos", "mix")

gp_parallel3d(
  zlines,
  x = x,
  main = "3D parallel lines",
  view = c(65, 35),
  lwd = 2
)
```

## 3D 向量和标签

```r
grid <- expand.grid(
  x = seq(-1, 1, length.out = 5),
  y = seq(-1, 1, length.out = 5),
  z = seq(-1, 1, length.out = 5)
)

gp_vectors3d(
  grid$x, grid$y, grid$z,
  -grid$y, grid$x, 0,
  col = 4,
  main = "3D vector field",
  view = c(65, 35)
)

gp_labels3d(
  1:3, 1:3, 1:3,
  labels = c("A", "B", "C"),
  col = 2,
  main = "3D labels"
)
```

## 3D polygons

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

## voxel 和 isosurface

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

## 混合图层

`gp_multi()` 现在也支持 3D 的 view、palette、pm3d、hidden3d、contour 等参数：

```r
gp_multi(
  gp_layer(
    x = cos(theta),
    y = sin(theta),
    z = theta / 5,
    style = "lines",
    title = "spiral",
    col = 2,
    lwd = 2
  ),
  gp_layer(
    x = cos(theta[seq(1, length(theta), by = 20)]),
    y = sin(theta[seq(1, length(theta), by = 20)]),
    z = theta[seq(1, length(theta), by = 20)] / 5,
    style = "points",
    title = "samples",
    col = 4,
    pch = 7,
    cex = 1.4
  ),
  dimensions = "3d",
  main = "Layered 3D plot",
  view = c(60, 35),
  hidden3d = TRUE,
  legend_position = "top right",
  legend_box = TRUE
)
```
