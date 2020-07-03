#' disbinputs UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_disbinputs_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' disbinputs Server Function
#'
#' @noRd 
mod_disbinputs_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_disbinputs_ui("disbinputs_ui_1")
    
## To be copied in the server
# callModule(mod_disbinputs_server, "disbinputs_ui_1")
 
