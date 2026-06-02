library(gunplotR)

x <- seq(0, 2 * pi, length.out = 200)
y <- sin(x)

gp_plot(
  x, y,
  main = "Sine wave",
  xlab = "x",
  ylab = "sin(x)"
)
