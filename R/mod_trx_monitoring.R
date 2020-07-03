source("R/disbursementfunction.R")
#' trx_monitoring UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_trx_monitoring_ui <- function(id){
  ns <- NS(id)
  tagList(
    uiOutput(ns("trx_monitoring"))
  )
}


#' trx_monitoring Server Function
#'
#' @noRd
mod_trx_monitoring_server <- function(input, output, session){
  ns <- session$ns

  output$flag1 <- renderText({
    paste("Total transactions between",glue_collapse(input$totaltrxs,last = " and "))

  })

 output$flag2 <- renderText({
    paste("Total transaction Amounts between",glue_collapse(input$totaltrxs_amount,last = " and "))

  })



  observeEvent(input$submit_trx,{

    start = ifelse(is.null(input$trxdates[1]),as.character(Sys.Date()),as.character(input$trxdates[1]))

    stop = ifelse(is.null(input$trxdates[2]),as.character(Sys.Date()),as.character(input$trxdates[2]))

    largetrx_query = sprintf("EXECUTE rw_LargeTransactionsReport @FromDate='%s',@ToDate='%s',@FromAmount='-99999999',@Branches ='000,001',@Products ='001,MA01,MA02,MA03,MA04,MA05,MAMA01,MB01,MB02,MB03,MC01,MC02,MF01,MF02,MFC802,MFC805,MFC806,MFC807,MFF804,MFS803,MFS903,MFT801,ML101,ML102,ML103,ML104,ML105,ML106,ML107,ML108,ML109,ML110,ML111,ML113,ML114,ML115,ML116,ML117,ML118,ML119,ML121,ML122,MLS904,MR01,MR02,MS01,MSS902,MTS901'",
                             format(as.Date(start),"%d/%b/%Y"),
                             format(as.Date(stop),"%d/%b/%Y"))

    largeTransactions = dbGetQuery(maisha_conn("EMSHILA","test.test21"),largetrx_query) %>% mutate_all(as.character())

    largeTransactions$TrxDate %>% min() %>% print()
    largeTransactions$TrxDate %>% max() %>% print()

    dailytrx_monitoring = maisha_dailytrx_monitor(largeTransactions,valuevar = "NumerOfTransactions",trxamount_flag = 10,day = Sys.Date())
    dailytrx_monitoring2 = maisha_dailytrx_monitor(largeTransactions,valuevar = "Amount",trxamount_flag = 10,day = Sys.Date())
    weeklytrx_monitoring = list(Count = maisha_weeklytrx_monitor(trxData = largeTransactions,"NumerOfTransactions"),
    Amount = maisha_weeklytrx_monitor(trxData = largeTransactions,"Amount"))
    # print(largeTransactions)
    valueboxfigs = valueboxes(trxData = largeTransactions)


    output$trx_counts <- renderDataTable({
      min = ifelse(is.null(input$totaltrxs) ,0,min(input$totaltrxs))
      max = ifelse(is.null(input$totaltrxs) ,0,max(input$totaltrxs))
      dailytrx_monitoring$transactions %>%
        filter(Total >= min ,Total<= max) %>%
        datatable(extensions = "Buttons",options = list(dom = "Bfrtip",buttons = "excel",scrollY = "250px"))

    })

  output$trx_weekly <- renderDataTable({
   produt = input$mode
   print(produt)
   pluck(weeklytrx_monitoring,produt) %>%
     datatable(extensions = "Buttons",options = list(dom = "Bfrtip",buttons = "excel",scrollY = "250px"))


    })

    output$trx_amounts_ <- renderDataTable({


      dailytrx_monitoring2$transactions %>%
        filter(TD >= ifelse(is.null(input$TD) ,0,as.numeric(input$TD))|
                 TC >= ifelse(is.null(input$TC) ,0,as.numeric(input$TC)) |
                 CC >= ifelse(is.null(input$CC) ,0,as.numeric(input$CC)) |
                 CD >= ifelse(is.null(input$CD) ,0,as.numeric(input$CD)) ) %>%
        datatable(extensions = "Buttons",
                  options = list(dom = "Bfrtip",buttons = "excel",scrollY = "250px"))

    })



    output$infoboxes <- renderUI({

      tagList(
        column(3,infoBox(width = 12,value = format(valueboxfigs$TotalCredit,big.mark = ","),title  = "CREDIT",icon = icon("coins"))),
        column(3,infoBox(width = 12, value = format(valueboxfigs$TotalDebit,big.mark = ","),title  = "DEBIT",icon = icon("coins"))),
        column(3,infoBox(width = 12,value  = valueboxfigs$Totatrans,title  = "Trx Counts",icon = icon("bank")))
      )
    })

    # output$heading <- renderText({
    #   paste("Transaction Monitoring report for ")
    # })

    output$summaries <- renderUI({
      tabBox(width = 12,title = "Transaction Montoring",
        tabPanel(title = 'Daily Trx Counts',
                 column(4,
                        helpText("Use the controls to set flag rules"),
                        sliderInput(ns("totaltrxs"),"Total Transactions",min = min(dailytrx_monitoring$transactions$Total),max = max(dailytrx_monitoring$transactions$Total),

                                    value = c(min(dailytrx_monitoring$transactions$Total)+10,max(dailytrx_monitoring$transactions$Total),step = 1)),
                        textOutput(ns("flag1"))
                        ),
                 column(8,dataTableOutput(ns("trx_counts")))),
        tabPanel(title = 'Daily Trx Amounts',
                 column(4,
                        helpText("Use the controls to set flag rules"),
                        column(6,numericInput(ns("CC"),"CC",value = 1000000),
                               numericInput(ns("CD"),"CD",value = 1000000)),
                        column(6,numericInput(ns("TD"),"TD",value = 1000000),
                               numericInput(ns("TC"),"TC",value = 1000000))
                        ),
                 column(8,dataTableOutput(ns("trx_amounts_")))),
        tabPanel(title = 'Trx Cummulative',
                 column(3,
                        selectInput(ns("mode"),label = "Select Param",choices = c("Amount","Count"))),
                 column(9,dataTableOutput(ns("trx_weekly")))
                 )
        # tabPanel(title = 'Client Specific',column(8,),
        #          column(4,))
      )
    })

  })

  output$trx_monitoring <- renderUI({


    tabBox(width = 12,
      tabPanel(title = "Manual",value = "manual",
               fluidRow(
                 box(title = "Transaction Data File",status = "success",solidHeader = F,width = 12,collapsible = T,
                     column(2,airDatepickerInput(inputId = ns("trxdates"),placeholder = Sys.Date(),multiple = T,label = "Period",
                                                 range = TRUE,maxDate = Sys.Date(),clearButton = T)
                            ),
                     column(1,actionBttn(ns("submit_trx"),"Submit",icon = icon("send"),style = "jelly",size = "xs",color = "royal")),
                     uiOutput(ns("infoboxes"))
                     ),
                 uiOutput(ns("summaries")))),
      tabPanel(title = "Realtime",value = "realtime")
    )
  })

}

## To be copied in the UI
# mod_trx_monitoring_ui("trx_monitoring_ui_1")

## To be copied in the server
# callModule(mod_trx_monitoring_server, "trx_monitoring_ui_1")

