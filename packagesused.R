# packagesused
packages_used <- c(
  "shiny",'shinydashboard','shinydashboardPlus','glue','magrittr','purrr','dashboardthemes',"shinyBS","shinyWidgets",
  "DT","openxlsx","dplyr","mbanalytics","highcharter","kableExtra","shinyalert"
)
maisha_load_packages <- function(pkgs) {
  pkgs = pkgs
  unavailable <-
    pkgs[!(pkgs %in% installed.packages()[, 'Package'])]
  if (length(unavailable) > 0) {
    print(
      paste(
        "The  following ",
        ifelse(length(unavailable) > 1, "package ", "package "),
        paste(unavailable, sep = "," ),
        " is unavailabe and will be installed."
      )
    )
  }
  if (length(unavailable) > 0) {
    install.packages(unavailable)
  }
  v = sum(unlist(lapply(pkgs, require, character.only = T)))
  print(paste0("A total of ",v," packages were loaded"))
}

maisha_load_packages(pkgs = packages_used)


