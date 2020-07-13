my_packages = c("dplyr","shiny","plotly","shinythemes","shinyWidgets","shinydashboard","lubridate","prophet","DT")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(my_packages, install_if_missing))
