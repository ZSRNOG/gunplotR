#' Run a Gunplot or Gnuplot script
#'
#' `gp_run()` writes `script` to a temporary script file and calls the external
#' plotting program with that file. Use this for full control over plotting
#' commands.
#'
#' @param script Character vector containing script lines.
#' @param path Optional executable path or command name.
#' @param workdir Directory used for the temporary script and as the process
#'   working directory.
#' @param echo If `TRUE`, print the generated script before running it.
#' @param persist If `TRUE`, pass `-persist` to the plotting executable. This is
#'   useful for interactive gnuplot windows.
#' @param cleanup If `TRUE`, delete the generated temporary script.
#'
#' @return Invisibly returns a list with `status`, `output`, `script`,
#'   `script_file`, and `executable`.
#' @export
gp_run <- function(script,
                   path = NULL,
                   workdir = tempdir(),
                   echo = FALSE,
                   persist = FALSE,
                   cleanup = TRUE) {
  if (!is.character(script) || length(script) == 0L) {
    stop("`script` must be a non-empty character vector.", call. = FALSE)
  }
  if (anyNA(script)) {
    stop("`script` cannot contain missing values.", call. = FALSE)
  }

  executable <- gp_executable(path = path, must_work = TRUE)
  workdir <- normalizePath(
    ensure_directory(workdir),
    winslash = "/",
    mustWork = TRUE
  )

  script_text <- paste(script, collapse = "\n")
  script_file <- tempfile("gunplotR-", tmpdir = workdir, fileext = ".gp")
  writeLines(script_text, con = script_file, useBytes = TRUE)

  if (isTRUE(cleanup)) {
    on.exit(unlink(script_file), add = TRUE)
  }

  if (isTRUE(echo)) {
    cat(script_text, sep = "\n")
  }

  oldwd <- getwd()
  on.exit(setwd(oldwd), add = TRUE)
  setwd(workdir)

  args <- c(
    if (isTRUE(persist)) "-persist",
    normalizePath(script_file, winslash = "/", mustWork = FALSE)
  )

  output <- tryCatch(
    suppressWarnings(system2(executable, args, stdout = TRUE, stderr = TRUE)),
    error = function(err) {
      stop("Failed to run the plotting executable: ", conditionMessage(err),
           call. = FALSE)
    }
  )

  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }

  result <- list(
    status = status,
    output = unname(output),
    script = script_text,
    script_file = normalizePath(script_file, winslash = "/", mustWork = FALSE),
    executable = executable
  )

  if (!identical(status, 0L)) {
    message <- paste(c("Plotting program exited with a non-zero status.", output),
                     collapse = "\n")
    stop(message, call. = FALSE)
  }

  invisible(result)
}
