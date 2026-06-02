\name{gp_run}
\alias{gp_run}
\title{Run a Gunplot or Gnuplot script}
\usage{
gp_run(script, path = NULL, workdir = tempdir(), echo = FALSE,
  persist = FALSE, cleanup = TRUE)
}
\arguments{
\item{script}{Character vector containing script lines.}
\item{path}{Optional executable path or command name.}
\item{workdir}{Directory used for the temporary script and as the process
working directory.}
\item{echo}{If \code{TRUE}, print the generated script before running it.}
\item{persist}{If \code{TRUE}, pass \code{-persist} to the plotting executable.}
\item{cleanup}{If \code{TRUE}, delete the generated temporary script.}
}
\value{
Invisibly returns a list with process status, output, generated script,
script file, and executable path.
}
\description{
\code{gp_run()} writes \code{script} to a temporary script file and calls the
external plotting program with that file.
}
\examples{
\dontrun{
gp_run(c(
  "set terminal pngcairo size 900,600",
  "set output 'demo.png'",
  "plot sin(x) with lines title 'sin(x)'",
  "unset output"
))
}
}
