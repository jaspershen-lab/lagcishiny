#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Check if 'lagci' is installed
  if (requireNamespace("lagci", quietly = TRUE)) {
    # Inform the user it's available
    packageStartupMessage("✔ 'lagci' is installed and ready to use.")
  } else {
    # Warn the user to install from GitHub
    warning(
      "⚠ 'lagci' is not installed.\n",
      "You can install it using:\n",
      "  remotes::install_github(\"jaspershen-lab/lagci\")"
    )
  }
  
  packageStartupMessage("Welcome to lagcishiny 🎉 \n",
                        "You can use lagci web application via \n",
                        "lagcishiny::lagci_shiny() \n",
                        "If you want to export PDF report, please run \n",
                        "tinytex::install_tinytex()\n",
                        "or install locally\n",
                        "ref:\n",
                        "https://stackoverflow.com/questions/56770843/how-to-install-tinytex-from-local-zip-file-in-r")
}
