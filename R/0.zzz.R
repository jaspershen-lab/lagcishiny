#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Check if 'laggedcor' is installed
  if (requireNamespace("laggedcor", quietly = TRUE)) {
    # Inform the user it's available
    packageStartupMessage("✔ 'laggedcor' is installed and ready to use.")
  } else {
    # Warn the user to install from GitHub
    warning(
      "⚠ 'laggedcor' is not installed.\n",
      "You can install it using:\n",
      "  remotes::install_github(\"jaspershen-lab/laggedcor\")"
    )
  }
  
  packageStartupMessage("Welcome to laggedcorAPP 🎉 \n",
                        "You can use laggedcor web application via \n",
                        "laggedcorAPP::laggedcor_shiny()")
}
