#' Find the Gunplot or Gnuplot executable
#'
#' `gp_executable()` returns the external plotting executable used by
#' gunplotR. The executable can be supplied directly, configured with
#' `options(gunplotR.bin = "...")`, configured with the `GUNPLOTR_BIN`
#' environment variable, or found on `PATH` as `gunplot` or `gnuplot`.
#'
#' @param path Optional executable path or command name.
#' @param must_work If `TRUE`, throw an error when no executable can be found.
#'
#' @return A character scalar path, or `NA_character_` when `must_work = FALSE`
#'   and no executable is found.
#' @export
gp_executable <- function(path = NULL, must_work = TRUE) {
  executable <- gp_detect_executable(path = path)
  if (!is.na(executable)) {
    return(executable)
  }

  if (isTRUE(must_work)) {
    stop(
      paste(c(
        "Cannot find a gunplot/gnuplot executable.",
        "Install gnuplot, add it to PATH, or set",
        "options(gunplotR.bin = 'path/to/gnuplot') or GUNPLOTR_BIN.",
        "",
        gp_install_hint()
      ), collapse = "\n"),
      call. = FALSE
    )
  }

  NA_character_
}

#' Detect the Gunplot or Gnuplot executable
#'
#' `gp_detect_executable()` searches explicit paths, package options,
#' environment variables, `PATH`, and common Windows install locations. It does
#' not throw when nothing is found.
#'
#' @inheritParams gp_executable
#'
#' @return A character scalar path or `NA_character_`.
#' @export
#'
#' @examples
#' gp_detect_executable()
gp_detect_executable <- function(path = NULL) {
  candidates <- gp_executable_candidates(path = path)

  for (candidate in candidates) {
    resolved <- Sys.which(candidate)
    if (nzchar(resolved)) {
      return(unname(normalizePath(resolved, winslash = "/", mustWork = FALSE)))
    }

    if (file.exists(candidate)) {
      return(unname(normalizePath(candidate, winslash = "/", mustWork = FALSE)))
    }
  }

  NA_character_
}

#' Configure the Gunplot or Gnuplot executable
#'
#' `gp_configure()` tries to find an executable and, when successful, stores it
#' in `options(gunplotR.bin = ...)` for the current R session.
#'
#' @inheritParams gp_executable
#' @param quiet If `FALSE`, print a short message describing the result.
#'
#' @return Invisibly returns the detected path or `NA_character_`.
#' @export
#'
#' @examples
#' \dontrun{
#' gp_configure()
#' gp_configure("C:/Program Files/gnuplot/bin/gnuplot.exe")
#' }
gp_configure <- function(path = NULL, quiet = FALSE) {
  executable <- gp_detect_executable(path = path)
  if (!is.na(executable)) {
    options(gunplotR.bin = executable)
    if (!isTRUE(quiet)) {
      message("gunplotR.bin = ", executable)
    }
  } else if (!isTRUE(quiet)) {
    message(gp_install_hint())
  }
  invisible(executable)
}

gp_executable_candidates <- function(path = NULL) {
  program_files <- unique(c(
    Sys.getenv("ProgramFiles", unset = ""),
    Sys.getenv("ProgramFiles(x86)", unset = ""),
    "C:/Program Files",
    "C:/Program Files (x86)"
  ))
  program_files <- program_files[nzchar(program_files)]
  windows_candidates <- file.path(program_files, "gnuplot", "bin",
                                  "gnuplot.exe")

  candidates <- unique(c(
    path,
    getOption("gunplotR.bin", ""),
    Sys.getenv("GUNPLOTR_BIN", unset = ""),
    "gunplot",
    "gnuplot",
    "wgnuplot",
    windows_candidates,
    "C:/gnuplot/bin/gnuplot.exe"
  ))
  candidates[!is.na(candidates) & nzchar(candidates)]
}

#' Check whether Gunplot or Gnuplot is available
#'
#' @inheritParams gp_executable
#'
#' @return `TRUE` if an executable can be found, otherwise `FALSE`.
#' @export
gp_available <- function(path = NULL) {
  !is.na(gp_executable(path = path, must_work = FALSE))
}

#' Get the Gunplot or Gnuplot version
#'
#' @inheritParams gp_executable
#'
#' @return A character vector containing the version output.
#' @export
gp_version <- function(path = NULL) {
  executable <- gp_executable(path = path, must_work = TRUE)

  output <- tryCatch(
    system2(executable, "--version", stdout = TRUE, stderr = TRUE),
    error = function(err) {
      stop("Failed to run the plotting executable: ", conditionMessage(err),
           call. = FALSE)
    }
  )

  status <- attr(output, "status")
  if (!is.null(status) && !identical(status, 0L)) {
    stop(paste(c("Failed to get version from plotting executable.", output),
               collapse = "\n"),
         call. = FALSE)
  }

  unname(output)
}

gp_install_hint <- function() {
  paste(c(
    "Install gnuplot first, then restart R or run gp_configure().",
    "Windows PowerShell:",
    "  winget install -e --id gnuplot.gnuplot",
    "After installation, if it is still not on PATH:",
    "  options(gunplotR.bin = \"C:/Program Files/gnuplot/bin/gnuplot.exe\")"
  ), collapse = "\n")
}
