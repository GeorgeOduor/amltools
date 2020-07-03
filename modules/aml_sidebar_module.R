aml_sidebar_UI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("sidebar_aml"))
  )
}

aml_sidebar <- function(input, output, session) {
  ns = session$ns

  output$sidebar_aml <- renderUI({
    sidebarMenu(id = "mainsidebar",
                hr(),
                menuItem("Home",icon = icon("home"),tabName = "hometab",selected = T),
                menuItem(text = "Loan Limits",icon = icon("money"),
                         href = "http://127.0.0.1:2022",startExpanded = T)
                )
  })
}
