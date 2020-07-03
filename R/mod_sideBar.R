#' sideBar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @rdname mod_sideBar
#' @export
#'
#' @importFrom shiny NS tagList
mod_sideBar_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("sidebar_aml"))
  )
}

#' sideBar Server Function
#'
#' @rdname mod_sideBar
#' @export
mod_sideBar_server <- function(input, output, session){
  ns <- session$ns
  output$sidebar_aml <- renderUI({
    sidebarMenu(id = "mainsidebar",
                hr(),
                menuItem("Home",icon = icon("home"),tabName = "hometab",selected = T),
                mod_disbinputs_ui("disbinputs_ui_1"),
                menuItem(text = "Loan Limits",icon = icon("money"),tabName = "loanLimits",
                        startExpanded = T),
                menuItem(text = "Transactions Monitoring",icon = icon("edit"),
                         tabName = "trxMonitoring",startExpanded = T),
                menuItem(text = "Overdrawn Accounts",icon = icon("bank"),
                         tabName = "overdrawnAccounts",startExpanded = T)

    )
  })
}

## To be copied in the UI
# mod_sideBar_ui("sideBar_ui_1")

## To be copied in the server
# callModule(mod_sideBar_server, "sideBar_ui_1")

