ensure_directory <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }

  if (!dir.exists(path)) {
    stop("Cannot create directory: ", path, call. = FALSE)
  }

  invisible(path)
}

gp_quote <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "/", x, fixed = TRUE)
  x <- gsub("'", "''", x, fixed = TRUE)
  paste0("'", x, "'")
}

gp_string_quote <- function(x) {
  x <- as.character(x)
  if (length(x) == 0L || any(is.na(x)) || any(!nzchar(x))) {
    stop("`value` must contain non-empty character value(s).",
         call. = FALSE)
  }
  x <- gsub("'", "''", x, fixed = TRUE)
  paste0("'", x, "'")
}

xy_data <- function(x, y = NULL) {
  if (is.null(y) && is.data.frame(x)) {
    if (ncol(x) < 2L) {
      stop("When `x` is a data frame, it must have at least two columns.",
           call. = FALSE)
    }

    out <- data.frame(x = x[[1L]], y = x[[2L]])
  } else if (is.null(y)) {
    out <- data.frame(x = seq_along(x), y = x)
  } else {
    if (length(x) != length(y)) {
      stop("`x` and `y` must have the same length.", call. = FALSE)
    }

    out <- data.frame(x = x, y = y)
  }

  if (!is.numeric(out$x) || !is.numeric(out$y)) {
    stop("Plot data must be numeric.", call. = FALSE)
  }
  if (nrow(out) == 0L) {
    stop("Plot data must contain at least one row.", call. = FALSE)
  }

  out
}

normalize_output <- function(output) {
  if (is.null(output)) {
    return(NULL)
  }

  output <- as.character(output)
  if (length(output) != 1L || !nzchar(output)) {
    stop("`output` must be a single non-empty file path.", call. = FALSE)
  }

  normalizePath(output, winslash = "/", mustWork = FALSE)
}

terminal_from_output <- function(output) {
  ext <- tolower(tools::file_ext(output))

  switch(
    ext,
    png = "pngcairo size 900,600",
    pdf = "pdfcairo",
    svg = "svg",
    eps = "postscript eps color",
    jpg = "jpeg",
    jpeg = "jpeg",
    gif = "gif",
    stop(
      "Cannot infer terminal from output extension `.", ext,
      "`. Please supply `terminal` explicitly.",
      call. = FALSE
    )
  )
}

validate_terminal <- function(terminal, arg) {
  if (is.null(terminal)) {
    return(NULL)
  }

  terminal <- as.character(terminal)
  if (length(terminal) != 1L || is.na(terminal) || !nzchar(terminal)) {
    stop("`", arg, "` must be a single non-empty character value.",
         call. = FALSE)
  }

  terminal
}

interactive_terminal <- function() {
  terminal <- getOption("gunplotR.preview_terminal", NULL)
  if (!is.null(terminal)) {
    return(validate_terminal(terminal, "gunplotR.preview_terminal"))
  }

  if (.Platform$OS.type == "windows") {
    "windows"
  } else {
    "qt"
  }
}
