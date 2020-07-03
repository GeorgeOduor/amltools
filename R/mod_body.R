#' body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_body_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("body"))
  )
}

#' body Server Function
#'
#' @noRd
mod_body_server <- function(input, output, session){
  ns <- session$ns

  output$body <- renderUI({
    aml_body()
  })
}

## To be copied in the UI
# mod_body_ui("body_ui_1")

## To be copied in the server
# callModule(mod_body_server, "body_ui_1")

