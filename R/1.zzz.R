#' @keywords internal
.onAttach <- function(libname, pkgname) {
  # Check if 'laggedcor' is installed
  if (requireNamespace("laggedcor", quietly = TRUE)) {
    # Load the laggedcor package
    library(laggedcor, character.only = TRUE)
    
    # Inform the user
    packageStartupMessage("✔ 'laggedcor' is installed and loaded successfully.")
  } else {
    # Warn the user to install from GitHub
    warning(
      "⚠ 'laggedcor' is not installed.\n",
      "You can install it using:\n",
      "  remotes::install_github(\"jaspershen-lab/laggedcor\")"
    )
  }
  
  # Optional message for laggedcorAPP
  packageStartupMessage("Welcome to laggedcorAPP 🎉")
}
