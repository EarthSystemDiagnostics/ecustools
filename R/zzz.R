
.onAttach <- function(libname, pkgname) {

  msg <- paste("Package 'ecustools' is deprecated!",
               "Please start to use its descendants:\n\n",
               "https://github.com/EarthSystemDiagnostics/grfxtools\n",
               "https://github.com/EarthSystemDiagnostics/geostools\n",
               "https://github.com/EarthSystemDiagnostics/stattools\n",
               "https://github.com/EarthSystemDiagnostics/prxytools\n",
               "https://github.com/EarthSystemDiagnostics/ncdftools\n",
               "https://github.com/EarthSystemDiagnostics/pfields\n",
               "https://github.com/EarthSystemDiagnostics/orbitalforcing\n")
  warning(msg, call. = FALSE)

}

function_deprecated <- function(successor, new.name = NULL) {

  old.name <- rlang::call_frame(n = 2)$fn_name
  if (is.null(new.name)) new.name <- old.name

  msg <- paste0(
    sprintf("You called function '%s' from package 'ecustools'.\n", old.name),
    "Package 'ecustools' is deprecated!\n\n",
    sprintf("Please use %s::%s() from package '%s'.\n",
            successor, new.name, successor))

  stop(msg, call. = FALSE)

}
