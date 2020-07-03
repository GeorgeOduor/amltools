#' overdrawn_accounts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_overdrawn_accounts_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("accounts_overdrawn"))
  )
}

#' overdrawn_accounts Server Function
#'
#' @noRd
mod_overdrawn_accounts_server <- function(input, output, session){
  ns <- session$ns

  # get the datasets::

  # 1: previous deposits

  # datasets
  dataset <- reactive({
    if (is.null(unpu$reportingdate)) {
      shinyalert(title = "Error!","You must enter date",type = "error",closeOnClickOutside = T)
    }
    date = input$reportingdate






  })

  prev_deposit = reactive({
    # infile =
    if (!is.null(input$prev_dep)) {
      prev_deposit = read.csv(input$prev_dep$datapath)
    }
    return(prev_deposit)
  })
  active_deposit = reactive({
    # infile =
    if (!is.null(input$active_dep)) {
      active_deposit = read.csv(input$active_dep$datapath)
    }
    return(active_deposit)
  })

  observeEvent(input$submit_deposits,{

    req(prev_deposit)
    req(active_deposit)
  # =====================program start=====================================
  prev_depfile = isolate(prev_deposit) %>% as_tibble()
  active_depfile = isolate(active_deposit)%>% as_tibble()
print(prev_depfile)
  overdrawn_report <- overdrawn_function(prev_depfile,prev_depfile)
  output$accountactivity <- renderDataTable({
    overdrawn_report$`No Change` %>% datatable(options = list(scrollY = 300,scroller = T))
  })
  output$overdrawn_1 <- renderDataTable({
    overdrawn_report$`New Deposit` %>% datatable(options = list(scrollY = 300,scroller = T))
  })
  output$overdrawn_2 <- renderDataTable({
    overdrawn_report$`New Negative Account` %>% datatable(options = list(scrollY = 300,scroller = T))
  })
  newnegative_count = nrow(overdrawn_report$`New Negative Account`)
  newnegative_amount = abs(overdrawn_report$`New Negative Account`$Active_ClearBalance) %>% sum()
  all_negative_accounts = overdrawn_report %>% bind_rows() %>% nrow()
  change = round(newnegative_count/all_negative_accounts*100,2)
  newnegative_amount = abs(overdrawn_report$`New Negative Account`$Active_ClearBalance) %>% sum()


    output$overdrawnreports <- renderUI({
      tagList(tabBox(width = 9,
        tabPanel(title = "No Changes Observed",dataTableOutput(ns("accountactivity"))),
        tabPanel(title = "Existing Overdrawn With changes",dataTableOutput(ns("overdrawn_1"))),
        tabPanel(title = "New Overdrawn Accounts",dataTableOutput(ns("overdrawn_2")))
      ),
      boxPlus(title = "Summary",width = 3))
    })

    output$decriptions <- renderUI({
      tagList(descriptionBlock(number = paste(change,"%"),header = all_negative_accounts,number_color = ifelse(change <= 0 ,"green","red"),margin_bottom = T,number_icon = ifelse(change <= 0,'fa fa-caret-down','fa fa-caret-up'),text = "All Overdrawn Accounts"),
      descriptionBlock(number ="",header = format(newnegative_amount,big.mark = ","),margin_bottom = T,text = "New Overdrawn Amount"),
      descriptionBlock(number ="",header = format(newnegative_count,big.mark = ","),margin_bottom = T,text = "New Overdrawn Accounts")
)    })
  })

 output$accounts_overdrawn <- renderUI({
   tagList(
     accordion(
       accordionItem(id = "overdrawn",title = "OverdrawnAccounts",collapsed = T,color = "info",
         panel(
           column(3,popify(el = airDatepickerInput(inputId = ns("reportingdate1")),
                           title = "Previous Deposits File",content = "Use this button to upload previous deposits file if it doesnot exist already in the inputs folder.")),

           column(3,popify(el = airDatepickerInput(inputId = ns("reportingdate")),
                           title = "Current Deposits File",content = "Use this button to upload previous deposits file if it doesnot exist already in the inputs folder.")),

           column(3,actionBttn(ns("submit_deposits"),"Submit",icon = icon("send"),style = "gradient",size = "xs",color = "royal")),
           column(3,helpText(HTML("<small>Download this report.</small>")),actionBttn(ns("download"),label = "Download",style = "gradient",size = "md",color = "royal",icon = icon("download")))
         )
       )
     ),
     fluidRow(
       uiOutput(ns("overdrawnreports")),uiOutput(ns("decriptions"))
     )
   )

 })
}

## To be copied in the UI
# mod_overdrawn_accounts_ui("overdrawn_accounts_ui_1")

## To be copied in the server
# callModule(mod_overdrawn_accounts_server, "overdrawn_accounts_ui_1")

