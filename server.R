#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  callModule(mod_sideBar_server, "sideBar_ui_1")
  # populate body
  callModule(mod_body_server, "body")

  callModule(mod_loanlimits_server, "loanlims")

  callModule(mod_trx_monitoring_server, "trx_monitoring_ui_1")

  callModule(mod_overdrawn_accounts_server, "overdrawn_accounts_ui_1")

})
