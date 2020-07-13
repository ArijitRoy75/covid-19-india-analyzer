my_packages = c("drat","e1071","shiny","plotly","shinythemes","shinyWidgets","shinydashboard","lubridate","prophet","DT")

devtools::install_url("https://cran.r-project.org/src/contrib/Archive/BH/BH_1.62.0-1.tar.gz")
install.packages('rstan')

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(my_packages, install_if_missing))

