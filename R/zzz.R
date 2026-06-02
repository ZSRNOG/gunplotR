.onLoad <- function(libname, pkgname) {
  if (is.null(getOption("gunplotR.bin")) &&
      !nzchar(Sys.getenv("GUNPLOTR_BIN", unset = ""))) {
    executable <- gp_detect_executable()
    if (!is.na(executable)) {
      options(gunplotR.bin = executable)
    }
  }
}

.onAttach <- function(libname, pkgname) {
  if (identical(getOption("gunplotR.startup_message"), FALSE)) {
    return(invisible())
  }
  if (is.na(gp_detect_executable())) {
    packageStartupMessage(gp_install_hint())
  }
}
