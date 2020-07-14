install.packages("remotes")

remove.packages("rstan")
remove.packages("StanHeaders")
if (file.exists(".RData")) file.remove(".RData")

Sys.setenv(MAKEFLAGS = "-j4") # four cores used

remotes::install_github("stan-dev/rstan", ref = "develop", subdir = "rstan/rstan", build_opts = "")

my_packages = c("drat","stats","e1071","shiny","plotly","shinythemes","shinyWidgets","shinydashboard","lubridate","DT")

install.packages("prophet",type = "source")

install_if_missing = function(p) {
  if (p %in% rownames(installed.packages()) == FALSE) {
    install.packages(p, dependencies = TRUE)
  }
  else {
    cat(paste("Skipping already installed package:", p, "\n"))
  }
}
invisible(sapply(my_packages, install_if_missing))

