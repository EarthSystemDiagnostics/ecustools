
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("       package ecustools has moved to GitHub, https://github.com/EarthSystemDiagnostics/ecustools
        install directly with devtools::install_github(\"EarthSystemDiagnostics/ecustools\")
        or update your remote.

        # to print the current settings
        git remote -v

        # to change origin to the new url
        git remote set-url origin git@github.com:username/repo-name.git")
}

